#include <stddef.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include "adpcm-lib.h"

typedef uint8_t   u8;
typedef uint16_t u16;
typedef uint32_t u32;
typedef int8_t    s8;
typedef int16_t  s16;
typedef int32_t  s32;
typedef int64_t  s64;

typedef volatile u8   vu8;
typedef volatile u16 vu16;
typedef volatile u32 vu32;
typedef volatile s8   vs8;
typedef volatile s16 vs16;
typedef volatile s32 vs32;

typedef u8  bool8;
typedef u16 bool16;
typedef u32 bool32;
typedef vu8  vbool8;
typedef vu16 vbool16;
typedef vu32 vbool32;

#define IS_BIG_ENDIAN (*(uint16_t *)"\0\xff" < 0x0100)
#define ADPCM_FLAG_NOISE_SHAPING    0x1
#define ADPCM_FLAG_RAW_OUTPUT       0x2

struct SwavInfo {
    u8 waveType;
    bool8 loop;
    u16 sampleRate;
    u16 time;
    u16 loopOffset;
    u32 loopLength;
};

struct NdsFileHeader {
    char type[4];
    u32 magic;
    u32 fileSize;
    u16 structSize;
    u16 numBlocks;
};

struct SwavData {
    char type[4];
    u32 size;
    struct SwavInfo info;
    u8 data[];
};

struct Swav {
    struct NdsFileHeader file;
    struct SwavData data;
};

typedef struct {
    char ckID [4];
    u32 ckSize;
    char formType [4];
} RiffChunkHeader;

typedef struct {
    char ckID [4];
    u32 ckSize;
} ChunkHeader;

#define ChunkHeaderFormat "4L"

typedef struct {
    u16 FormatTag, NumChannels;
    u32 SampleRate, BytesPerSecond;
    u16 BlockAlign, BitsPerSample;
    u16 cbSize;
    union {
        u16 ValidBitsPerSample;
        u16 SamplesPerBlock;
        u16 Reserved;
    } Samples;
    s32 ChannelMask;
    u16 SubFormat;
    char GUID [14];
} WaveHeader;

#define WaveHeaderFormat "SSLLSSSSLS"

typedef struct {
    char ckID [4];
    u32 ckSize;
    u32 TotalSamples;
} FactHeader;

struct AdpcmData {
    u32 sampleRate;
    u32 numSamples;
    u32 size;
    FILE * file;
};

#define FactHeaderFormat "4LL"

#define WAVE_FORMAT_PCM         0x1
#define WAVE_FORMAT_IMA_ADPCM   0x11
#define WAVE_FORMAT_EXTENSIBLE  0xfffe

static int write_pcm_wav_header (FILE *outfile, int num_channels, u32 num_samples, u32 sample_rate);
static int write_adpcm_wav_header (FILE *outfile, int num_channels, u32 num_samples, u32 sample_rate, int samples_per_block);
static int adpcm_decode_data (FILE *infile, FILE *outfile, int num_channels, u32 num_samples, int block_size);
static int adpcm_encode_data (FILE *infile, FILE *outfile, int num_channels, u32 num_samples, int samples_per_block, int lookahead, int noise_shaping);
static void little_endian_to_native (void *data, char *format);
static void native_to_little_endian (void *data, char *format);

struct AdpcmData adpcm_converter (char *infilename, char *outfilename, int flags, int blocksize_pow2, int lookahead)
{
    int format = 0, res = 0, bits_per_sample, num_channels, filesize;
    u32 fact_samples = 0, num_samples = 0, sample_rate;
    FILE *infile, *outfile;
    RiffChunkHeader riff_chunk_header;
    ChunkHeader chunk_header;
    WaveHeader WaveHeader;
    struct AdpcmData adpcmData;

    if (!(infile = fopen (infilename, "rb"))) {
        fprintf (stderr, "can't open file \"%s\" for reading!\n", infilename);
        exit(-1); //return -1;
    }

    // read initial RIFF form header

    if (!fread (&riff_chunk_header, sizeof (RiffChunkHeader), 1, infile) ||
        strncmp (riff_chunk_header.ckID, "RIFF", 4) ||
        strncmp (riff_chunk_header.formType, "WAVE", 4)) {
            fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
            exit(-1); //return -1;
    }

    // loop through all elements of the RIFF wav header (until the data chuck)

    while (1) {

        if (!fread (&chunk_header, sizeof (ChunkHeader), 1, infile)) {
            fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
            exit(-1); //return -1;
        }

        little_endian_to_native (&chunk_header, ChunkHeaderFormat);

        // if it's the format chunk, we want to get some info out of there and
        // make sure it's a .wav file we can handle

        if (!strncmp (chunk_header.ckID, "fmt ", 4)) {
            int supported = 1;

            if (chunk_header.ckSize < 16 || chunk_header.ckSize > sizeof (WaveHeader) ||
                !fread (&WaveHeader, chunk_header.ckSize, 1, infile)) {
                    fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                    exit(-1); //return -1;
            }

            little_endian_to_native (&WaveHeader, WaveHeaderFormat);

            format = (WaveHeader.FormatTag == WAVE_FORMAT_EXTENSIBLE && chunk_header.ckSize == 40) ?
                WaveHeader.SubFormat : WaveHeader.FormatTag;

            bits_per_sample = (chunk_header.ckSize == 40 && WaveHeader.Samples.ValidBitsPerSample) ?
                WaveHeader.Samples.ValidBitsPerSample : WaveHeader.BitsPerSample;

            if (WaveHeader.NumChannels < 1 || WaveHeader.NumChannels > 2)
                supported = 0;
            else if (format == WAVE_FORMAT_PCM) {
                if (bits_per_sample < 9 || bits_per_sample > 16)
                    supported = 0;

                if (WaveHeader.BlockAlign != WaveHeader.NumChannels * 2)
                    supported = 0;
            }
            else if (format == WAVE_FORMAT_IMA_ADPCM) {
                if (bits_per_sample != 4)
                    supported = 0;

                if (WaveHeader.Samples.SamplesPerBlock != (WaveHeader.BlockAlign - WaveHeader.NumChannels * 4) * (WaveHeader.NumChannels ^ 3) + 1) {
                    fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                    exit(-1); //return -1;
                }
            }
            else
                supported = 0;

            if (!supported) {
                fprintf (stderr, "\"%s\" is an unsupported .WAV format!\n", infilename);
                exit(-1); //return -1;
            }
        }
        else if (!strncmp (chunk_header.ckID, "fact", 4)) {

            if (chunk_header.ckSize < 4 || !fread (&fact_samples, sizeof (fact_samples), 1, infile)) {
                fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                exit(-1); //return -1;
            }

            little_endian_to_native (&fact_samples, "L");

            if (chunk_header.ckSize > 4) {
                int bytes_to_skip = chunk_header.ckSize - 4;
                char dummy;

                while (bytes_to_skip--)
                    if (!fread (&dummy, 1, 1, infile)) {
                        fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                        exit(-1); //return -1;
                    }
            }
        }
        else if (!strncmp (chunk_header.ckID, "data", 4)) {

            // on the data chunk, get size and exit parsing loop

            if (!WaveHeader.NumChannels) {      // make sure we saw a "fmt" chunk...
                fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                exit(-1); //return -1;
            }

            if (!chunk_header.ckSize) {
                fprintf (stderr, "this .WAV file has no audio samples, probably is corrupt!\n");
                exit(-1); //return -1;
            }

            if (format == WAVE_FORMAT_PCM) {
                if (chunk_header.ckSize % WaveHeader.BlockAlign) {
                    fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                    exit(-1); //return -1;
                }

                num_samples = chunk_header.ckSize / WaveHeader.BlockAlign;
            }
            else {
                u32 complete_blocks = chunk_header.ckSize / WaveHeader.BlockAlign;
                int leftover_bytes = chunk_header.ckSize % WaveHeader.BlockAlign;
                int samples_last_block;

                num_samples = complete_blocks * WaveHeader.Samples.SamplesPerBlock;

                if (leftover_bytes) {
                    if (leftover_bytes % (WaveHeader.NumChannels * 4)) {
                        fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                        exit(-1); //return -1;
                    }
                    samples_last_block = (leftover_bytes - (WaveHeader.NumChannels * 4)) * (WaveHeader.NumChannels ^ 3) + 1;
                    num_samples += samples_last_block;
                }
                else
                    samples_last_block = WaveHeader.Samples.SamplesPerBlock;

                if (fact_samples) {
                    if (fact_samples < num_samples && fact_samples > num_samples - samples_last_block) {
                        num_samples = fact_samples;
                    }
                    else if (WaveHeader.NumChannels == 2 && (fact_samples >>= 1) < num_samples && fact_samples > num_samples - samples_last_block) {
                        num_samples = fact_samples;
                    }
                }
            }

            if (!num_samples) {
                fprintf (stderr, "this .WAV file has no audio samples, probably is corrupt!\n");
                exit(-1); //return -1;
            }

            num_channels = WaveHeader.NumChannels;
            sample_rate = WaveHeader.SampleRate;
            break;
        }
        else {          // just ignore unknown chunks
            int bytes_to_eat = (chunk_header.ckSize + 1) & ~1L;
            char dummy;

            while (bytes_to_eat--)
                if (!fread (&dummy, 1, 1, infile)) {
                    fprintf (stderr, "\"%s\" is not a valid .WAV file!\n", infilename);
                    exit(-1); //return -1;
                }
        }
    }

    if (!(outfile = fopen (outfilename, "w+b"))) {
        fprintf (stderr, "can't open file \"%s\" for writing!\n", outfilename);
        exit(-1); //return -1;
    }

    if (format == WAVE_FORMAT_PCM) {
        int block_size, samples_per_block;

        if (blocksize_pow2)
            block_size = 1 << blocksize_pow2;
        else
            block_size = 256 * num_channels * (sample_rate < 11000 ? 1 : sample_rate / 11000);

        samples_per_block = (block_size - num_channels * 4) * (num_channels ^ 3) + 1;

        if (!(flags & ADPCM_FLAG_RAW_OUTPUT) && !write_adpcm_wav_header (outfile, num_channels, num_samples, sample_rate, samples_per_block)) {
            fprintf (stderr, "can't write header to file \"%s\" !\n", outfilename);
            exit(-1); //return -1;
        }

        res = adpcm_encode_data (infile, outfile, num_channels, num_samples, samples_per_block, lookahead,
            (flags & ADPCM_FLAG_NOISE_SHAPING) ? (sample_rate > 64000 ? NOISE_SHAPING_STATIC : NOISE_SHAPING_DYNAMIC) : NOISE_SHAPING_OFF);
        fseek(outfile, 0, SEEK_END);
        adpcmData.sampleRate = sample_rate;
        adpcmData.numSamples = num_samples;
        adpcmData.size = ftell(outfile);
        adpcmData.file = outfile;
    }
    else
        exit(-1); //return -1;

    //fclose (outfile);
    fclose (infile);
    return adpcmData;
}

static int write_adpcm_wav_header (FILE *outfile, int num_channels, u32 num_samples, u32 sample_rate, int samples_per_block)
{
    RiffChunkHeader riffhdr;
    ChunkHeader datahdr, fmthdr;
    WaveHeader wavhdr;
    FactHeader facthdr;

    int wavhdrsize = 20;
    int block_size = (samples_per_block - 1) / (num_channels ^ 3) + (num_channels * 4);
    u32 num_blocks = num_samples / samples_per_block;
    int leftover_samples = num_samples % samples_per_block;
    u32 total_data_bytes = num_blocks * block_size;

    if (leftover_samples) {
        int last_block_samples = ((leftover_samples + 6) & ~7) + 1;
        int last_block_size = (last_block_samples - 1) / (num_channels ^ 3) + (num_channels * 4);
        total_data_bytes += last_block_size;
    }

    memset (&wavhdr, 0, sizeof (wavhdr));

    wavhdr.FormatTag = WAVE_FORMAT_IMA_ADPCM;
    wavhdr.NumChannels = num_channels;
    wavhdr.SampleRate = sample_rate;
    wavhdr.BytesPerSecond = sample_rate * block_size / samples_per_block;
    wavhdr.BlockAlign = block_size;
    wavhdr.BitsPerSample = 4;
    wavhdr.cbSize = 2;
    wavhdr.Samples.SamplesPerBlock = samples_per_block;

    strncpy (riffhdr.ckID, "RIFF", sizeof (riffhdr.ckID));
    strncpy (riffhdr.formType, "WAVE", sizeof (riffhdr.formType));
    riffhdr.ckSize = sizeof (riffhdr) + wavhdrsize + sizeof (facthdr) + sizeof (datahdr) + total_data_bytes;
    strncpy (fmthdr.ckID, "fmt ", sizeof (fmthdr.ckID));
    fmthdr.ckSize = wavhdrsize;
    strncpy (facthdr.ckID, "fact", sizeof (facthdr.ckID));
    facthdr.TotalSamples = num_samples;
    facthdr.ckSize = 4;

    strncpy (datahdr.ckID, "data", sizeof (datahdr.ckID));
    datahdr.ckSize = total_data_bytes;

    // write the RIFF chunks up to just before the data starts

    native_to_little_endian (&riffhdr, ChunkHeaderFormat);
    native_to_little_endian (&fmthdr, ChunkHeaderFormat);
    native_to_little_endian (&wavhdr, WaveHeaderFormat);
    native_to_little_endian (&facthdr, FactHeaderFormat);
    native_to_little_endian (&datahdr, ChunkHeaderFormat);

    return fwrite (&riffhdr, sizeof (riffhdr), 1, outfile) &&
        fwrite (&fmthdr, sizeof (fmthdr), 1, outfile) &&
        fwrite (&wavhdr, wavhdrsize, 1, outfile) &&
        fwrite (&facthdr, sizeof (facthdr), 1, outfile) &&
        fwrite (&datahdr, sizeof (datahdr), 1, outfile);
}

static int adpcm_encode_data (FILE *infile, FILE *outfile, int num_channels, u32 num_samples, int samples_per_block, int lookahead, int noise_shaping)
{
    int block_size = (samples_per_block - 1) / (num_channels ^ 3) + (num_channels * 4), percent;
    s16 *pcm_block = malloc (samples_per_block * num_channels * 2);
    void *adpcm_block = malloc (block_size);
    u32 progress_divider = 0;
    void *adpcm_cnxt = NULL;

    if (!pcm_block || !adpcm_block) {
        fprintf (stderr, "could not allocate memory for buffers!\n");
        return -1;
    }

    while (num_samples) {
        int this_block_adpcm_samples = samples_per_block;
        int this_block_pcm_samples = samples_per_block;
        size_t num_bytes;

        if (this_block_pcm_samples > num_samples) {
            this_block_adpcm_samples = ((num_samples + 6) & ~7) + 1;
            block_size = (this_block_adpcm_samples - 1) / (num_channels ^ 3) + (num_channels * 4);
            this_block_pcm_samples = num_samples;
        }

        if (!fread (pcm_block, this_block_pcm_samples * num_channels * 2, 1, infile)) {
            fprintf (stderr, "\rcould not read all audio data from input file!\n");
            return -1;
        }

        if (IS_BIG_ENDIAN) {
            int scount = this_block_pcm_samples * num_channels;
            u8 *cp = (u8 *) pcm_block;

            while (scount--) {
                s16 temp = cp [0] + (cp [1] << 8);
                * (s16 *) cp = temp;
                cp += 2;
            }
        }

        // if this is the last block and it's not full, duplicate the last sample(s) so we don't
        // create problems for the lookahead

        if (this_block_adpcm_samples > this_block_pcm_samples) {
            s16 *dst = pcm_block + this_block_pcm_samples * num_channels, *src = dst - num_channels;
            int dups = (this_block_adpcm_samples - this_block_pcm_samples) * num_channels;

            while (dups--)
                *dst++ = *src++;
        }

        // if this is the first block, compute a decaying average (in reverse) so that we can let the
        // encoder know what kind of initial deltas to expect (helps initializing index)

        if (!adpcm_cnxt) {
            s32 average_deltas [2];
            int i;

            average_deltas [0] = average_deltas [1] = 0;

            for (i = this_block_adpcm_samples * num_channels; i -= num_channels;) {
                average_deltas [0] -= average_deltas [0] >> 3;
                average_deltas [0] += abs ((s32) pcm_block [i] - pcm_block [i - num_channels]);

                if (num_channels == 2) {
                    average_deltas [1] -= average_deltas [1] >> 3;
                    average_deltas [1] += abs ((s32) pcm_block [i-1] - pcm_block [i+1]);
                }
            }

            average_deltas [0] >>= 3;
            average_deltas [1] >>= 3;

            adpcm_cnxt = adpcm_create_context (num_channels, lookahead, noise_shaping, average_deltas);
        }

        adpcm_encode_block (adpcm_cnxt, adpcm_block, &num_bytes, pcm_block, this_block_adpcm_samples);

        if (num_bytes != block_size) {
            fprintf (stderr, "\radpcm_encode_block() did not return expected value (expected %d, got %d)!\n", block_size, (int) num_bytes);
            return -1;
        }

        if (!fwrite (adpcm_block, block_size, 1, outfile)) {
            fprintf (stderr, "\rcould not write all audio data to output file!\n");
            return -1;
        }

        num_samples -= this_block_pcm_samples;

        if (progress_divider) {
            int new_percent = 100 - num_samples / progress_divider;

            if (new_percent != percent) {
                fprintf (stderr, "\rprogress: %d%% ", percent = new_percent);
                fflush (stderr);
            }
        }
    }

    if (adpcm_cnxt)
        adpcm_free_context (adpcm_cnxt);

    free (adpcm_block);
    free (pcm_block);
    return 0;
}

static void little_endian_to_native (void *data, char *format)
{
    u8 *cp = (u8 *) data;
    s32 temp;

    while (*format) {
        switch (*format) {
            case 'L':
                temp = cp [0] + ((s32) cp [1] << 8) + ((s32) cp [2] << 16) + ((s32) cp [3] << 24);
                * (s32 *) cp = temp;
                cp += 4;
                break;

            case 'S':
                temp = cp [0] + (cp [1] << 8);
                * (short *) cp = (short) temp;
                cp += 2;
                break;

            default:
                if (isdigit ((u8) *format))
                    cp += *format - '0';

                break;
        }

        format++;
    }
}

static void native_to_little_endian (void *data, char *format)
{
    u8 *cp = (u8 *) data;
    s32 temp;

    while (*format) {
        switch (*format) {
            case 'L':
                temp = * (s32 *) cp;
                *cp++ = (u8) temp;
                *cp++ = (u8) (temp >> 8);
                *cp++ = (u8) (temp >> 16);
                *cp++ = (u8) (temp >> 24);
                break;

            case 'S':
                temp = * (short *) cp;
                *cp++ = (u8) temp;
                *cp++ = (u8) (temp >> 8);
                break;

            default:
                if (isdigit ((u8) *format))
                    cp += *format - '0';

                break;
        }

        format++;
    }
}

int main(int argc, char * argv[]) {
    struct Swav * swav;
    FILE * infile, * outfile1, * outfile2;
    char * infilename = argv[1], * outfilename = argv[2];
    u8 * data;
    int lookahead = 3, flags = ADPCM_FLAG_RAW_OUTPUT, blocksize_pow2 = 0, overwrite = 0, remainder;
    struct AdpcmData adpcmData = adpcm_converter(infilename, outfilename, flags, blocksize_pow2, lookahead);
    outfile1 = adpcmData.file;
    outfile2 = fopen(strncat(outfilename, ".swav", 6), "wb");
    fseek(outfile1, 0, SEEK_SET);
    data = malloc(adpcmData.size + 20 + 16 + 4);
    swav = malloc(adpcmData.size + 20 + 16 + 4);
    swav->file.type[0] = 'S';
    swav->file.type[1] = 'W';
    swav->file.type[2] = 'A';
    swav->file.type[3] = 'V';
    swav->file.magic = 0x0100feff;
    swav->file.fileSize = adpcmData.size + 20 + 16;
    swav->file.structSize = 16;
    swav->file.numBlocks = 1;
    swav->data.info.waveType = 2;
    swav->data.info.loop = 0;
    swav->data.info.sampleRate = adpcmData.sampleRate;
    swav->data.info.time = (33513982 / 2) / adpcmData.sampleRate;
    swav->data.info.loopOffset = 0;
    swav->data.info.loopLength = adpcmData.numSamples / 8;
    if (remainder = adpcmData.numSamples % 8) {
        swav->data.info.loopLength++;
        swav->data.size += (8 - remainder);
        swav->file.fileSize += (8 - remainder);
    }
    swav->data.type[0] = 'D';
    swav->data.type[1] = 'A';
    swav->data.type[2] = 'T';
    swav->data.type[3] = 'A';
    swav->data.size = swav->file.fileSize - 16;
    fread(swav->data.data, 1, adpcmData.size, outfile1);
    fclose(outfile1);
    memcpy(data, swav, swav->file.fileSize);
    return fwrite(data, swav->file.fileSize, 1, outfile2);
}
