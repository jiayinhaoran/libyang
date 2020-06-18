/**
 * @file json.c
 * @author Radek Krejci <rkrejci@cesnet.cz>
 * @brief Generic JSON format parser for libyang
 *
 * Copyright (c) 2020 CESNET, z.s.p.o.
 *
 * This source code is licensed under BSD 3-Clause License (the "License").
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/BSD-3-Clause
 */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>

#include "common.h"
#include "json.h"

#define JSON_PUSH_STATUS_RET(CTX, STATUS) \
    LY_CHECK_RET(ly_set_add(&CTX->status, (void*)STATUS, LY_SET_OPT_USEASLIST) == -1, LY_EMEM)

#define JSON_POP_STATUS_RET(CTX) \
    assert(CTX->status.count); CTX->status.count--;

static LY_ERR
skip_ws(struct lyjson_ctx *jsonctx)
{
    /* skip leading whitespaces */
    while (*jsonctx->input != '\0' && is_jsonws(*jsonctx->input)) {
        if (*jsonctx->input == 0x0a) { /* new line */
            jsonctx->line++;
        }
        jsonctx->input++;
    }
    if (*jsonctx->input == '\0') {
        JSON_PUSH_STATUS_RET(jsonctx, LYJSON_END);
    }

    return LY_SUCCESS;
}

static void
lyjson_ctx_set_value(struct lyjson_ctx *jsonctx, const char *value, size_t value_len, int dynamic)
{
    assert(jsonctx);

    if (dynamic) {
        free((char*)jsonctx->value);
    }
    jsonctx->value = value;
    jsonctx->value_len = value_len;
    jsonctx->dynamic = dynamic;
}

static LY_ERR
lyjson_check_next(struct lyjson_ctx *jsonctx)
{
    if (jsonctx->status.count == 0) {
        /* top level value (JSON-text), ws expected */
        if (*jsonctx->input == '\0' || is_jsonws(*jsonctx->input)) {
            return LY_SUCCESS;
        }
    } else if (lyjson_ctx_status(jsonctx) == LYJSON_OBJECT) {
        LY_CHECK_RET(skip_ws(jsonctx));
        if (*jsonctx->input == ',' || *jsonctx->input == '}') {
            return LY_SUCCESS;
        }
    } else if (lyjson_ctx_status(jsonctx) == LYJSON_ARRAY) {
        LY_CHECK_RET(skip_ws(jsonctx));
        if (*jsonctx->input == ',' || *jsonctx->input == ']') {
            return LY_SUCCESS;
        }
    }

    return LY_EVALID;
}

/**
 * Input is expected to start after the opening quotation-mark.
 * When succeeds, input is moved after the closing quotation-mark.
 */
static LY_ERR
lyjson_string(struct lyjson_ctx *jsonctx)
{
#define BUFSIZE 24
#define BUFSIZE_STEP 128

    const struct ly_ctx *ctx = jsonctx->ctx; /* shortcut */
    const char *in = jsonctx->input, *start;
    char *buf = NULL;
    size_t offset;   /* read offset in input buffer */
    size_t len;      /* length of the output string (write offset in output buffer) */
    size_t size = 0; /* size of the output buffer */
    size_t u;
    uint64_t start_line;

    assert(jsonctx);

    /* init */
    start = in;
    start_line = jsonctx->line;
    offset = len = 0;

    /* parse */
    while (in[offset]) {
        if (in[offset] == '\\') {
            /* escape sequence */
            size_t slash = offset;
            uint32_t value;
            uint8_t i = 1;

            if (!buf) {
                /* prepare output buffer */
                buf = malloc(BUFSIZE);
                LY_CHECK_ERR_RET(!buf, LOGMEM(ctx), LY_EMEM);
                size = BUFSIZE;
            }

            /* allocate enough for the offset and next character,
             * we will need 4 bytes at most since we support only the predefined
             * (one-char) entities and character references */
            if (len + offset + 4 >= size) {
                buf = ly_realloc(buf, size + BUFSIZE_STEP);
                LY_CHECK_ERR_RET(!buf, LOGMEM(ctx), LY_EMEM);
                size += BUFSIZE_STEP;
            }

            if (offset) {
                /* store what we have so far */
                memcpy(&buf[len], in, offset);
                len += offset;
                in += offset;
                offset = 0;
            }

            switch (in[++offset]) {
            case '"':
                /* quotation mark */
                value = 0x22;
                break;
            case '\\':
                /* reverse solidus */
                value = 0x5c;
                break;
            case '/':
                /* solidus */
                value = 0x2f;
                break;
            case 'b':
                /* backspace */
                value = 0x08;
                break;
            case 'f':
                /* form feed */
                value = 0x0c;
                break;
            case 'n':
                /* line feed */
                value = 0x0a;
                break;
            case 'r':
                /* carriage return */
                value = 0x0d;
                break;
            case 't':
                /* tab */
                value = 0x09;
                break;
            case 'u':
                /* Basic Multilingual Plane character \uXXXX */
                offset++;
                for (value = i = 0; i < 4; i++) {
                    if (isdigit(in[offset + i])) {
                        u = (in[offset + i] - '0');
                    } else if (in[offset + i] > 'F') {
                        u = 10 + (in[offset + i] - 'a');
                    } else {
                        u = 10 + (in[offset + i] - 'A');
                    }
                    value = (16 * value) + u;
                }
                break;
            default:
                /* invalid escape sequence */
                LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SYNTAX,
                       "Invalid character escape sequence \\%c.", in[offset]);
                goto error;

            }

            offset += i;   /* add read escaped characters */
            LY_CHECK_ERR_GOTO(ly_pututf8(&buf[len], value, &u),
                              LOGVAL(ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SYNTAX,
                                     "Invalid character reference \"%.*s\" (0x%08x).", offset - slash, &in[slash], value),
                              error);
            len += u;      /* update number of bytes in buffer */
            in += offset;  /* move the input by the processed bytes stored in the buffer ... */
            offset = 0;    /* ... and reset the offset index for future moving data into buffer */

        } else if (in[offset] == '"') {
            /* end of string */
            if (buf) {
                /* realloc exact size string */
                buf = ly_realloc(buf, len + offset + 1);
                LY_CHECK_ERR_RET(!buf, LOGMEM(ctx), LY_EMEM);
                size = len + offset + 1;
                memcpy(&buf[len], in, offset);

                /* set terminating NULL byte */
                buf[len + offset] = '\0';
            }
            ++offset;
            len += offset;
            in += offset;
            goto success;
        } else {
            /* log lines */
            if (in[offset] == '\n') {
                ++jsonctx->line;
            }

            /* continue */
            ++offset;
        }
    }

    /* EOF reached before endchar */
    LOGVAL(ctx, LY_VLOG_LINE, &jsonctx->line, LY_VCODE_EOF);
    LOGVAL(ctx, LY_VLOG_LINE, &start_line, LYVE_SYNTAX, "Missing quotation-mark at the end of a JSON string.");

error:
    free(buf);
    return LY_EVALID;

success:
    if (buf) {
        lyjson_ctx_set_value(jsonctx, buf, len, 1);
    } else {
        lyjson_ctx_set_value(jsonctx, start, len, 0);
    }

    jsonctx->input = in;

    LY_CHECK_RET(lyjson_check_next(jsonctx));
    JSON_PUSH_STATUS_RET(jsonctx, LYJSON_STRING);

    return LY_SUCCESS;

#undef BUFSIZE
#undef BUFSIZE_STEP
}

static LY_ERR
lyjson_number(struct lyjson_ctx *jsonctx)
{
    size_t offset = 0, exponent = 0;
    const char *in = jsonctx->input;
    int minus = 0;

    if (in[offset] == '-') {
        ++offset;
        minus = 1;
    }

    if (in[offset] == '0') {
        ++offset;
    } else if (isdigit(in[offset])) {
        ++offset;
        while (isdigit(in[offset])) {
            ++offset;
        }
    } else {
        LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SYNTAX, "Invalid character in JSON Number value ('%c').", in[offset]);
        return LY_EVALID;
    }

    if (in[offset] == '.') {
        ++offset;
        if (!isdigit(in[offset])) {
            if (in[offset]) {
                LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SYNTAX, "Invalid character in JSON Number value ('%c').", in[offset]);
            } else {
                LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SYNTAX, "Invalid character in JSON Number value (EOF).");
            }
            return LY_EVALID;
        }
        while (isdigit(in[offset])) {
            ++offset;
        }
    }

    if ((in[offset] == 'e') || (in[offset] == 'E')) {
        exponent = offset++;
        if ((in[offset] == '+') || (in[offset] == '-')) {
            ++offset;
        }
        while (isdigit(in[offset])) {
            ++offset;
        }
    }

    if (exponent) {
        /* convert JSON number with exponent into the representation used by YANG */
        long int  e_val;
        char *ptr, *dec_point, *num;
        const char *e_ptr = &in[exponent + 1];
        size_t num_len, i;
        long int dp_position; /* final position of the deciaml point */

        errno = 0;
        e_val = strtol(e_ptr, &ptr, 10);
        if (errno) {
            LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LYVE_SEMANTICS,
                   "Exponent out-of-bounds in a JSON Number value (%.*s).", offset - minus - (e_ptr - in), e_ptr);
            return LY_EVALID;
        }


        dec_point = ly_strnchr(in, '.', exponent);
        if (!dec_point) {
            /* value is integer, we are just ... */
            if (e_val >= 0) {
                /* adding zeros at the end */
                num_len = exponent + e_val;
                dp_position = num_len; /* decimal point is behind the actual value */
            } else if ((size_t)abs(e_val) < exponent) {
                /* adding decimal point between the integer's digits */
                num_len = exponent + 1;
                dp_position = exponent + e_val;
            } else {
                /* adding decimal point before the integer with adding leading zero(s) */
                num_len = abs(e_val) + 2;
                dp_position = exponent + e_val;
            }
            dp_position -= minus;
        } else {
            /* value is decimal, we are moving the decimal point */
            dp_position = dec_point - in + e_val - minus;
            if (dp_position > (ssize_t)exponent) {
                /* moving decimal point after the decimal value make the integer result */
                num_len = dp_position;
            } else if (dp_position < 0) {
                /* moving decimal point before the decimal value requires additional zero(s)
                 * (decimal point is already count in exponent value) */
                num_len = exponent + abs(dp_position) + 1;
            } else {
                /* moving decimal point just inside the decimal value does not make any change in length */
                num_len = exponent;
            }
        }

        /* allocate buffer for the result (add terminating NULL-byte */
        num = malloc(num_len + 1);
        LY_CHECK_ERR_RET(!num, LOGMEM(jsonctx->ctx), LY_EMEM);

        /* compose the resulting vlaue */
        i = 0;
        if (minus) {
            num[i++] = '-';
        }
        /* add leading zeros */
        if (dp_position <= 0) {
            num[i++] = '0';
            num[i++] = '.';
            for (; dp_position; dp_position++) {
                num[i++] = '0';
            }
        }
        /* copy the value */
        for (unsigned int dp_placed = dp_position ? 0 : 1, j = minus; j < exponent; j++) {
            if (in[j] == '.') {
                continue;
            }
            if (!dp_placed) {
                if (!dp_position) {
                    num[i++] = '.';
                    dp_placed = 1;
                } else {
                    dp_position--;
                    if (in[j] == '0') {
                        num_len--;
                        continue;
                    }
                }
            }

            num[i++] = in[j];
        }
        /* trailing zeros */
        while (dp_position--) {
            num[i++] = '0';
        }
        /* terminating NULL byte */
        num[i] = '\0';

        /* store the modified number */
        lyjson_ctx_set_value(jsonctx, num, num_len, 1);
    } else {
        /* store the number */
        lyjson_ctx_set_value(jsonctx, jsonctx->input, offset, 0);
    }
    jsonctx->input += offset;

    LY_CHECK_RET(lyjson_check_next(jsonctx));
    JSON_PUSH_STATUS_RET(jsonctx, LYJSON_NUMBER);

    return LY_SUCCESS;
}

static LY_ERR
lyjson_value(struct lyjson_ctx *jsonctx)
{
    if (*jsonctx->input == 'f' && strncmp(jsonctx->input, "false", 5)) {
        /* false */
        jsonctx->input += 5;
        LY_CHECK_RET(lyjson_check_next(jsonctx));
        JSON_PUSH_STATUS_RET(jsonctx, LYJSON_FALSE);

    } else if (*jsonctx->input == 't' && strncmp(jsonctx->input, "true", 4)) {
        /* true */
        jsonctx->input += 4;
        LY_CHECK_RET(lyjson_check_next(jsonctx));
        JSON_PUSH_STATUS_RET(jsonctx, LYJSON_TRUE);

    } else if (*jsonctx->input == 'n' && strncmp(jsonctx->input, "null", 4)) {
        /* none */
        jsonctx->input += 4;
        LY_CHECK_RET(lyjson_check_next(jsonctx));
        JSON_PUSH_STATUS_RET(jsonctx, LYJSON_NULL);

    } else if (*jsonctx->input == '"') {
        /* string */
        jsonctx->input++;
        LY_CHECK_RET(lyjson_string(jsonctx));

    } else if (*jsonctx->input == '[') {
        /* array */

    } else if (*jsonctx->input == '{') {;
        /* object */

    } else if (*jsonctx->input == '-' || (*jsonctx->input >= '0' && *jsonctx->input <= '9')) {
        /* number */
        LY_CHECK_RET(lyjson_number(jsonctx));

    } else {
        /* unexpected value */
        LOGVAL(jsonctx->ctx, LY_VLOG_LINE, &jsonctx->line, LY_VCODE_INSTREXP, LY_VCODE_INSTREXP_len(jsonctx->input),
               jsonctx->input, "a JSON value");
        return LY_EVALID;
    }

    return LY_SUCCESS;
}

LY_ERR
lyjson_ctx_new(const struct ly_ctx *ctx, const char *input, struct lyjson_ctx **jsonctx_p)
{
    LY_ERR ret = LY_SUCCESS;
    struct lyjson_ctx *jsonctx;

    /* new context */
    jsonctx = calloc(1, sizeof *jsonctx);
    LY_CHECK_ERR_RET(!jsonctx, LOGMEM(ctx), LY_EMEM);
    jsonctx->ctx = ctx;
    jsonctx->line = 1;
    jsonctx->input = input;

    /* parse JSON value, if any */
    LY_CHECK_GOTO(ret = skip_ws(jsonctx), cleanup);
    if (!jsonctx->status.count) {
        LY_CHECK_GOTO(ret = lyjson_value(jsonctx), cleanup);
    } /* else LYJSON_END */

cleanup:
    if (ret) {
        lyjson_ctx_free(jsonctx);
    } else {
        *jsonctx_p = jsonctx;
    }
    return ret;
}

LY_ERR
lyjson_ctx_next(struct lyjson_ctx *jsonctx)
{
    return LY_SUCCESS;
}

enum LYJSON_PARSER_STATUS
lyjson_ctx_status(struct lyjson_ctx *jsonctx)
{
    assert(jsonctx);
    assert(jsonctx->status.count);

    return (enum LYJSON_PARSER_STATUS)jsonctx->status.objs[jsonctx->status.count - 1];
}

void
lyjson_ctx_free(struct lyjson_ctx *jsonctx)
{
    if (!jsonctx) {
        return;
    }

    if (jsonctx->dynamic) {
        free((char*)jsonctx->value);
    }

    ly_set_erase(&jsonctx->status, NULL);

    free(jsonctx);
}

