/*
 * libbaka
 *
 * ANSI Unicode-art Muxer for ffmpeg
 * adapted from libcaca
 *
 */

#include "libavutil/opt.h"
#include "libavutil/pixdesc.h"
#include "avdevice.h"

#include <sys/time.h>
#include <unistd.h>

#include <strings.h>

#include <stdio.h>
#include <stdlib.h>

typedef struct baka_timer {
    int last_sec;
    int last_usec;
} baka_timer_t;

typedef struct baka_bitmap_info {
    int bpp;
    int w;
    int h;
    int pitch;
    uint32_t rmask, gmask, bmask, amask;
    int rright, gright, bright, aright;
    int rleft, gleft, bleft, aleft;
} baka_bitmap_info_t;

typedef struct __attribute__((__packed__)) baka_color_rgb24 {
    uint8_t r;
    uint8_t g;
    uint8_t b;
    uint8_t padding;
} baka_color_rgb24_t;

typedef struct baka_symbol {
    uint32_t char_utf32;
    baka_color_rgb24_t fg;
    baka_color_rgb24_t bg;
} baka_symbol_t;

typedef struct BAKAContext {
    AVClass         *class;
    AVFormatContext *ctx;
    int             window_width,  window_height;
    char            *antialias;
    int             antialias_mode;
    char            *charset;
    int             charset_mode;

    // Timers:
    baka_timer_t    timer;
    int             lastticks;
    int             rendertime;

    int             delay; // Frametime, usec

    // Bitmap info:
    baka_bitmap_info_t bitmap;

    baka_symbol_t   *symbol_buffer;
    size_t          symbol_buffer_size;

    char            *screen_buffer;
    size_t          screen_buffer_size;

} BAKAContext;

#define ANTIALIAS_MODE_NORMAL 1
#define ANTIALIAS_MODE_HALF 2

#define CHARSET_MODE_NORMAL 1
#define CHARSET_MODE_ETB 2

// #define DEBUG_ATTR __attribute__ ((noinline))
#define DEBUG_ATTR

#define FORCE_INLINE __attribute__((always_inline)) inline
//#define FORCE_INLINE


static void die(const char* msg) {
  fprintf(stderr, "Fatal error: %s", msg);
  exit(1);
}

static size_t DEBUG_ATTR FORCE_INLINE baka_utf32_to_utf8(char *buf, uint32_t ch) {
    static const uint8_t mark[7] =
    {
        0x00, 0x00, 0xC0, 0xE0, 0xF0, 0xF8, 0xFC
    };

    char *parser = buf;
    size_t bytes;

    if(ch < 0x80)
    {
        *parser++ = ch;
        return 1;
    }

    bytes = (ch < 0x800) ? 2 : (ch < 0x10000) ? 3 : 4;
    parser += bytes;

    switch(bytes)
    {
        case 4: *--parser = (ch | 0x80) & 0xbf; ch >>= 6;
        case 3: *--parser = (ch | 0x80) & 0xbf; ch >>= 6;
        case 2: *--parser = (ch | 0x80) & 0xbf; ch >>= 6;
    }
    *--parser = ch | mark[bytes];

    return bytes;
}

/* Convert a mask, eg. 0x0000ff00, to shift values, eg. 8 and -4. */
static void DEBUG_ATTR FORCE_INLINE baka_mask2shift(uint32_t mask, int *right, int *left) {
    int rshift = 0, lshift = 0;

    if(!mask)
    {
        *right = *left = 0;
        return;
    }

    while(!(mask & 1))
    {
        mask >>= 1;
        rshift++;
    }
    *right = rshift;

    while(mask & 1)
    {
        mask >>= 1;
        lshift++;
    }
    *left = 12 - lshift;
}

static int DEBUG_ATTR baka_bitmap_init(baka_bitmap_info_t* d,
                            int bpp, int w, int h, int pitch,
                            uint32_t rmask, uint32_t gmask,
                            uint32_t bmask, uint32_t amask) {
    // Sanity check
    if(w < 0 || h < 0 || pitch < 0 || bpp > 32 || bpp < 8)
    {
        return -1;
    }

    d->bpp = bpp;
    d->w = w;
    d->h = h;
    d->pitch = pitch;

    d->rmask = rmask;
    d->gmask = gmask;
    d->bmask = bmask;
    d->amask = amask;

    if(rmask || gmask || bmask || amask)
    {
        baka_mask2shift(rmask, &d->rright, &d->rleft);
        baka_mask2shift(gmask, &d->gright, &d->gleft);
        baka_mask2shift(bmask, &d->bright, &d->bleft);
        baka_mask2shift(amask, &d->aright, &d->aleft);
    }

    return 0;
}

static void DEBUG_ATTR FORCE_INLINE baka_bitmap_get_rgb_24bpp(baka_bitmap_info_t const *d, uint8_t const *pixels, int x, int y, uint32_t *rgb) {
    // Little-endian, 24bpp optimized
    uint8_t const *pixel_req;
    pixel_req = pixels + 3 * x + d->pitch * y;

    rgb[0] += (uint32_t)pixel_req[0];
    rgb[1] += (uint32_t)pixel_req[1];
    rgb[2] += (uint32_t)pixel_req[2];
}

static void DEBUG_ATTR baka_bitmap_get_rgba_default(baka_bitmap_info_t const *d, uint8_t const *pixels,
                             int x, int y, unsigned int *rgba) {
    uint32_t bits;

    pixels += (d->bpp / 8) * x + d->pitch * y;

    switch(d->bpp / 8)
    {
        case 4:
            bits = *(uint32_t const *)pixels;
            break;
        case 3:
        {
// #if defined(HAVE_ENDIAN_H)
//             if(__BYTE_ORDER == __BIG_ENDIAN)
// #else
            /* This is compile-time optimised with at least -O1 or -Os */
            uint32_t const tmp = 0x12345678;
            if(*(uint8_t const *)&tmp == 0x12)
// #endif
                bits = ((uint32_t)pixels[0] << 16) |
                       ((uint32_t)pixels[1] << 8) |
                       ((uint32_t)pixels[2]);
            else
                bits = ((uint32_t)pixels[2] << 16) |
                       ((uint32_t)pixels[1] << 8) |
                       ((uint32_t)pixels[0]);
            break;
        }
        case 2:
            bits = *(uint16_t const *)pixels;
            break;
        case 1:
        default:
            bits = pixels[0];
            break;
    }
    rgba[0] += ((bits & d->rmask) >> d->rright) << d->rleft;
    rgba[1] += ((bits & d->gmask) >> d->gright) << d->gleft;
    rgba[2] += ((bits & d->bmask) >> d->bright) << d->bleft;
    rgba[3] += ((bits & d->amask) >> d->aright) << d->aleft;
}

#define MIN(a,b) (((a)<(b))?(a):(b))
#define MAX(x, y) (((x) > (y)) ? (x) : (y))
#define CLAMP(x, low, high)  (((x) > (high)) ? (high) : (((x) < (low)) ? (low) : (x)))

static int DEBUG_ATTR baka_bitmap_draw(BAKAContext* c, int x, int y, int w, int h,
                        baka_bitmap_info_t const *d, void const *pixels) {

    int x1, y1, x2, y2, deltax, deltay;

    if(!d || !pixels)
        return 0;

    x1 = x; x2 = x + w - 1;
    y1 = y; y2 = y + h - 1;

    /* FIXME: do not overwrite arguments */
    w = d->w;
    h = d->h;

    deltax = x2 - x1 + 1;
    deltay = y2 - y1 + 1;

    for(y = y1 > 0 ? y1 : 0; y <= y2 && y < c->window_height; y++) {
        for(x = x1 > 0 ? x1 : 0; x <= x2 && x < c->window_width; x++) {

            int fromx, fromy, tox, toy, myx, myy, dots;
            uint32_t rgb[3];

            uint32_t outch;
            baka_symbol_t *dest_sym;

            dest_sym = c->symbol_buffer + (y * c->window_width) + x;
            if (dest_sym >= c->symbol_buffer + c->symbol_buffer_size)
                die("Buffer overflow dest_sym");

            fromx = (uint64_t)(x - x1) * w / deltax;
            fromy = (uint64_t)(y - y1) * h / deltay;
            tox = (uint64_t)(x - x1 + 1) * w / deltax;
            toy = (uint64_t)(y - y1 + 1) * h / deltay;

            /* We want at least one pixel */
            if(tox == fromx) tox++;
            if(toy == fromy) toy++;

            /// TODO: optimize by making rgb a SIMD/AVX vector for the parallel load/add/divides
            /// and rewrite/replace baka_bitmap_get_rgb_24bpp()
            /// Expected speedup: ~2x
            /// and baka_bitmap_draw() + baka_bitmap_get_rgb_24bpp() use 30% of cpu time
            /// So ~15% saving on total load possible

            if (c->antialias_mode == ANTIALIAS_MODE_HALF && (toy - fromy >= 2)) {
                int halfy = fromy + ((toy - fromy)/2);

                dots = 0;
                rgb[0] = rgb[1] = rgb[2] = 0;

                for(myy = fromy; myy < halfy; myy++)
                    for(myx = fromx; myx < tox; myx++)
                {
                    dots++;
                    baka_bitmap_get_rgb_24bpp(d, pixels, myx, myy, rgb);
                }

                /* Normalize */
                rgb[0] /= dots;
                rgb[1] /= dots;
                rgb[2] /= dots;

                dest_sym->fg.r = rgb[0];
                dest_sym->fg.g = rgb[1];
                dest_sym->fg.b = rgb[2];

                dots = 0;
                rgb[0] = rgb[1] = rgb[2] = 0;

                for(myy = halfy; myy < toy; myy++)
                    for(myx = fromx; myx < tox; myx++)
                {
                    dots++;
                    baka_bitmap_get_rgb_24bpp(d, pixels, myx, myy, rgb);
                }

                /* Normalize */
                rgb[0] /= dots;
                rgb[1] /= dots;
                rgb[2] /= dots;

                dest_sym->bg.r = rgb[0];
                dest_sym->bg.g = rgb[1];
                dest_sym->bg.b = rgb[2];

                dest_sym->char_utf32 = 0x2580; // Top half block
            } else {
                dots = 0;
                rgb[0] = rgb[1] = rgb[2] = 0;

                for(myx = fromx; myx < tox; myx++)
                    for(myy = fromy; myy < toy; myy++)
                {
                    dots++;
                    baka_bitmap_get_rgb_24bpp(d, pixels, myx, myy, rgb);
                }

                /* Normalize */
                rgb[0] /= dots;
                rgb[1] /= dots;
                rgb[2] /= dots;

                // unsigned int lum = rgb[0];
                // if(rgb[1] > lum) lum = rgb[1];
                // if(rgb[2] > lum) lum = rgb[2];

                // ch = lum * dchmax / 0x1000;
                // if(ch < 0)
                //     ch = 0;
                // else if(ch > (int)(dchmax - 1))
                //     ch = dchmax - 1;
                dest_sym->bg.r = rgb[0];
                dest_sym->bg.g = rgb[1];
                dest_sym->bg.b = rgb[2];

                dest_sym->fg.r = rgb[0]>>1;
                dest_sym->fg.g = rgb[1]>>1;
                dest_sym->fg.b = rgb[2]>>1;

                dest_sym->char_utf32 = '@';
            }
        }
    }
    return 0;
}

static int DEBUG_ATTR baka_init(BAKAContext *c) {
    // Initialize BAKAContext

    c->screen_buffer = NULL;
    c->screen_buffer_size = 0;

    c->symbol_buffer = NULL;
    c->symbol_buffer_size = 0;

    c->timer.last_sec = 0;
    c->timer.last_usec = 0;
    c->lastticks = 0;
    c->rendertime = 0;
    c->delay = 0;

    return 0;
}

static int DEBUG_ATTR baka_getticks(baka_timer_t *timer) {
    // Adapted from _caca_getticks((), linux time.h only
    struct timeval tv;
    int ticks = 0;
    int new_sec, new_usec;
    gettimeofday(&tv, NULL);
    new_sec = tv.tv_sec;
    new_usec = tv.tv_usec;

    if(timer->last_sec != 0)
    {
        /* If the delay was greater than 60 seconds, return 10 seconds
         * otherwise we may overflow our ticks counter. */
        if(new_sec >= timer->last_sec + 60)
            ticks = 60 * 1000000;
        else
        {
            ticks = (new_sec - timer->last_sec) * 1000000;
            ticks += new_usec;
            ticks -= timer->last_usec;
        }
    }
    timer->last_sec = new_sec;
    timer->last_usec = new_usec;

    return ticks;
}

static void DEBUG_ATTR baka_sleep(int usec) {
    usleep(usec);
}

#define ANSIRGB24_FG_FORMAT "\x1b[38;2;%hhu;%hhu;%hhum"
#define ANSIRGB24_BG_FORMAT "\x1b[48;2;%hhu;%hhu;%hhum"
const size_t ANSIRGB24_SIZE = sizeof("\x1b[38;2;255;255;255m") - 1; // No null byte

#define FRAME_START "\x1B[0;0f"
const size_t FRAME_START_SIZE = sizeof(FRAME_START) - 1;

#define FRAME_END "\x1b[0m"
// PRE: |FRAME_END_ETB| <= |FRAME_END|
#define FRAME_END_ETB "\x17"
const size_t FRAME_END_SIZE = sizeof(FRAME_END) - 1;

static int DEBUG_ATTR baka_draw_display(BAKAContext *c) {
    char *screen = c->screen_buffer;
    int width = c->window_width;
    int height = c->window_height;
    int x, y;
    size_t bytes;

    baka_symbol_t *src_sym = c->symbol_buffer;

    if (c->charset_mode == CHARSET_MODE_NORMAL)
        screen += sprintf(screen, "%s", FRAME_START);

    for (y = 0; y < height; y++)
    {
        for (x = 0; x < width; x++)
        {
            if (src_sym >= c->symbol_buffer + c->symbol_buffer_size)
                die("Buffer overflow src_sym");

            bytes = sprintf(screen, ANSIRGB24_BG_FORMAT, src_sym->bg.r, src_sym->bg.g, src_sym->bg.b);
            if (bytes > ANSIRGB24_SIZE) die("Buffer overflow ANSIRGB24_SIZE");
            screen += bytes;

            bytes = sprintf(screen, ANSIRGB24_FG_FORMAT, src_sym->fg.r, src_sym->fg.g, src_sym->fg.b);
            if (bytes > ANSIRGB24_SIZE) die("Buffer overflow ANSIRGB24_SIZE");
            screen += bytes;

            bytes = baka_utf32_to_utf8(screen, src_sym->char_utf32);
            screen += bytes;

            src_sym++;
        }

        *screen++ = '\n';
    }

    if (c->charset_mode == CHARSET_MODE_NORMAL)
        screen += sprintf(screen, "%s", FRAME_END);
    else
        screen += sprintf(screen, "%s", FRAME_END_ETB);
    if (screen >= c->screen_buffer + c->screen_buffer_size)
        die("Buffer overflow c->screen");

    fwrite(c->screen_buffer, sizeof(char), screen - c->screen_buffer, stdout);
    fflush(stdout);

    return 0;
}

static int DEBUG_ATTR baka_refresh_display(BAKAContext *c) {
#   define IDLE_USEC 5000
    int ticks = c->lastticks + baka_getticks(&c->timer);

    baka_draw_display(c);

    /* Wait until dp->delay + time of last call */
    ticks += baka_getticks(&c->timer);
    for(ticks += baka_getticks(&c->timer);
        ticks + IDLE_USEC < (int)c->delay;
        ticks += baka_getticks(&c->timer))
    {
        baka_sleep(IDLE_USEC);
    }

    /* Update the render time */
    c->rendertime = ticks;

    c->lastticks = ticks - c->delay;

    /* If we drifted too much, it's bad, bad, bad. */
    if(c->lastticks > (int)c->delay)
        c->lastticks = 0;

    return 0;
}

#define FRAME_CLEAR "\x1B[0;0f\x1B[2J\x1B[0;0f"

static int DEBUG_ATTR baka_init_graphics(BAKAContext *c) {
    c->screen_buffer_size =
        c->window_width * c->window_height * (
          4 +  // Max UTF-8 bytes
          ANSIRGB24_SIZE + // BG
          ANSIRGB24_SIZE // FG
        ) +
        c->window_height * 1 + // Newlines
        FRAME_START_SIZE +
        FRAME_END_SIZE; // Constant
    c->screen_buffer = av_malloc(c->screen_buffer_size * sizeof(char));
    if(c->screen_buffer == NULL)
        return -1;

    c->symbol_buffer_size = c->window_width * c->window_height;
    c->symbol_buffer = av_malloc(c->symbol_buffer_size * sizeof(baka_symbol_t));
    if(c->symbol_buffer == NULL)
        return -1;

    if (c->charset_mode == CHARSET_MODE_NORMAL) {
        // clear terminal output
        fprintf(stdout, "%s", FRAME_CLEAR);
        fflush(stdout);
    }
    return 0;
}


static int DEBUG_ATTR baka_write_trailer(AVFormatContext *s) {
    BAKAContext *c = s->priv_data;

    /// free anything here
    // e.g.:
    // av_freep(&c->window_title);

    c->screen_buffer_size = 0;
    av_freep(&c->screen_buffer);
    c->symbol_buffer_size = 0;
    av_freep(&c->symbol_buffer);

    return 0;
}

static int DEBUG_ATTR baka_write_header(AVFormatContext *s) {
    // INIT:

    BAKAContext *c = s->priv_data;
    AVStream *st = s->streams[0];
    AVCodecParameters *encctx = st->codecpar;
    int ret, bpp;

    baka_init(c);
    c->ctx = s;

    if (   s->nb_streams > 1
        || encctx->codec_type != AVMEDIA_TYPE_VIDEO
        || encctx->codec_id   != AV_CODEC_ID_RAWVIDEO) {
        av_log(s, AV_LOG_ERROR, "Only supports one rawvideo stream\n");
        return AVERROR(EINVAL);
    }

    if (encctx->format != AV_PIX_FMT_RGB24) {
        av_log(s, AV_LOG_ERROR,
               "Unsupported pixel format '%s', choose rgb24\n",
               av_get_pix_fmt_name(encctx->format));
        return AVERROR(EINVAL);
    }

    if(!strcasecmp(c->antialias, "normal") || !strcasecmp(c->antialias, "default")) {
        c->antialias_mode = ANTIALIAS_MODE_NORMAL;
    } else if (!strcasecmp(c->antialias, "half")) {
        c->antialias_mode = ANTIALIAS_MODE_HALF;
    // } else if (!strcasecmp(c->antialias, "braille")) {
        //c->antialias_mode = ANTIALIAS_MODE_BRAILLE;
    } else {
        ret = AVERROR(EINVAL);
        av_log(s, AV_LOG_ERROR,
            "Invalid antialias method: '%s'\n Valid are: default, normal, half\n",
            c->antialias);
        goto fail;
    }

    if(!strcasecmp(c->charset, "normal") || !strcasecmp(c->charset, "default")) {
        c->charset_mode = CHARSET_MODE_NORMAL;
    } else if (!strcasecmp(c->charset, "etb")) {
        c->charset_mode = CHARSET_MODE_ETB;
    } else {
        ret = AVERROR(EINVAL);
        av_log(s, AV_LOG_ERROR,
            "Invalid charset: '%s'\n Valid are: default, normal, etb\n",
            c->charset);
        goto fail;
    }

    if (c->window_width < 1 || c->window_height < 1) {
        // Defaults:
        c->window_width  = 80;
        c->window_height = 24;
    }

    if (baka_init_graphics(c) != 0)
    {
        ret = AVERROR(EINVAL);
        av_log(s, AV_LOG_ERROR, "Failed to init graphics\n");
        goto fail;
    }

    bpp = av_get_bits_per_pixel(av_pix_fmt_desc_get(encctx->format));

    if ( baka_bitmap_init( &c->bitmap,
                                   bpp, encctx->width, encctx->height,
                                   bpp / 8 * encctx->width,
                                   0x0000ff, 0x00ff00, 0xff0000, 0)
                                   != 0)
    {
        ret =  AVERROR(EINVAL);
        av_log(s, AV_LOG_ERROR, "Failed to init bitmap\n");
        goto fail;
    }

    c->delay = av_rescale_q(1, st->codec->time_base, AV_TIME_BASE_Q);

    return 0;

fail:
    baka_write_trailer(s);
    return ret;
}

static int DEBUG_ATTR baka_write_packet(AVFormatContext *s, AVPacket *pkt) {
    // Each frame:
    BAKAContext *c = s->priv_data;

    baka_bitmap_draw(c, 0, 0, c->window_width, c->window_height, &c->bitmap, pkt->data);

    baka_refresh_display(c);
    return 0;
}

#define OFFSET(x) offsetof(BAKAContext,x)
#define ENC AV_OPT_FLAG_ENCODING_PARAM

static const AVOption options[] = {
    { "window_size",  "set window forced size",  OFFSET(window_width), AV_OPT_TYPE_IMAGE_SIZE, {.str = NULL }, 0, 0, ENC},
    { "antialias",    "set antialias method",    OFFSET(antialias), AV_OPT_TYPE_STRING, {.str = "default" }, 0, 0, ENC },
    { "charset",      "set output charset",      OFFSET(charset), AV_OPT_TYPE_STRING, {.str = "default" }, 0, 0, ENC },
    { NULL },
};

static const AVClass baka_class = {
    .class_name = "baka outdev",
    .item_name  = av_default_item_name,
    .option     = options,
    .version    = LIBAVUTIL_VERSION_INT,
    .category   = AV_CLASS_CATEGORY_DEVICE_VIDEO_OUTPUT,
};

AVOutputFormat ff_baka_muxer = {
    .name           = "baka",
    .long_name      = NULL_IF_CONFIG_SMALL("baka custom output device"),
    .priv_data_size = sizeof(BAKAContext),
    .audio_codec    = AV_CODEC_ID_NONE,
    .video_codec    = AV_CODEC_ID_RAWVIDEO,
    .write_header   = baka_write_header,
    .write_packet   = baka_write_packet,
    .write_trailer  = baka_write_trailer,
    .flags          = AVFMT_NOFILE,
    .priv_class     = &baka_class,
};
