/*
 * @file json.c
 * @author: Radek Krejci <rkrejci@cesnet.cz>
 * @brief unit tests for a generic JSON parser
 *
 * Copyright (c) 2020 CESNET, z.s.p.o.
 *
 * This source code is licensed under BSD 3-Clause License (the "License").
 * You may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://opensource.org/licenses/BSD-3-Clause
 */

#define _DEFAULT_SOURCE
#define _GNU_SOURCE

#include <stdarg.h>
#include <stddef.h>
#include <setjmp.h>
#include <cmocka.h>

#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include "json.h"
#include "context.h"

void *testfunc = NULL;

static int
setup(void **state)
{
    if (ly_ctx_new(NULL, 0, (struct ly_ctx **)state)) {
        return 1;
    }

    return 0;
}

static int
teardown(void **state)
{
    ly_ctx_destroy(*state, NULL);
    return 0;
}

#define BUFSIZE 1024
char logbuf[BUFSIZE] = {0};

/* set to 0 to printing error messages to stderr instead of checking them in code */
#define ENABLE_LOGGER_CHECKING 1

static void
logger(LY_LOG_LEVEL level, const char *msg, const char *path)
{
    (void) level; /* unused */

    if (path) {
        snprintf(logbuf, BUFSIZE - 1, "%s %s", msg, path);
    } else {
        strncpy(logbuf, msg, BUFSIZE - 1);
    }
}

static int
logger_setup(void **state)
{
    (void) state; /* unused */
#if ENABLE_LOGGER_CHECKING
    ly_set_log_clb(logger, 1);
#endif
    return 0;
}

static int
logger_teardown(void **state)
{
    (void) state; /* unused */
#if ENABLE_LOGGER_CHECKING
    if (testfunc) {
        fprintf(stderr, "%s\n", logbuf);
    }
#endif
    return 0;
}

void
logbuf_clean(void)
{
    logbuf[0] = '\0';
}

#if ENABLE_LOGGER_CHECKING
#   define logbuf_assert(str) assert_string_equal(logbuf, str)
#else
#   define logbuf_assert(str)
#endif



static void
test_general(void **state)
{
    struct lyjson_ctx *jsonctx;
    const char *str;

    testfunc = test_general;

    /* empty */
    str = "";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_END, lyjson_ctx_status(jsonctx));
    lyjson_ctx_free(jsonctx);

    str = "  \n\t \n";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_END, lyjson_ctx_status(jsonctx));
    lyjson_ctx_free(jsonctx);

    testfunc = NULL;
}

static void
test_number(void **state)
{
    struct lyjson_ctx *jsonctx;
    const char *str;

    testfunc = test_number;

    /* simple value */
    str = "11";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("11", jsonctx->value);
    assert_int_equal(2, jsonctx->value_len);
    assert_int_equal(0, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* fraction number */
    str = "37.7668";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("37.7668", jsonctx->value);
    assert_int_equal(7, jsonctx->value_len);
    assert_int_equal(0, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* negative number */
    str = "-122.3959";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("-122.3959", jsonctx->value);
    assert_int_equal(9, jsonctx->value_len);
    assert_int_equal(0, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* exp number */
    str = "1E10";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("10000000000", jsonctx->value);
    assert_int_equal(11, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    str = "15E-1";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("1.5", jsonctx->value);
    assert_int_equal(3, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    str = "15E-3";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("0.015", jsonctx->value);
    assert_int_equal(5, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* exp fraction number */
    str = "1.1e3";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("1100", jsonctx->value);
    assert_int_equal(4, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* negative exp fraction number */
    str = "1.1e-3";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("0.0011", jsonctx->value);
    assert_int_equal(6, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* exp negative fraction number */
    str = "-0.11e3";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("-110", jsonctx->value);
    assert_int_equal(4, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    /* negative exp negative fraction number */
    str = "-3.14e-3";
    assert_int_equal(LY_SUCCESS, lyjson_ctx_new(*state, str, &jsonctx));
    assert_int_equal(LYJSON_NUMBER, lyjson_ctx_status(jsonctx));
    assert_string_equal("-0.00314", jsonctx->value);
    assert_int_equal(8, jsonctx->value_len);
    assert_int_equal(1, jsonctx->dynamic);
    lyjson_ctx_free(jsonctx);

    testfunc = NULL;
}

#if 0

static void
test_element(void **state)
{
    struct lyxml_ctx *xmlctx;
    const char *str;

    /* empty */
    str = "";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    assert_true(xmlctx->input[0] == '\0');
    lyxml_ctx_free(xmlctx);

    /* end element */
    str = "</element>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Stray closing element tag (\"element\"). Line number 1.");

    /* no element */
    //logbuf_clean();
    str = "no data present";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Invalid character sequence \"no data present\", expected element tag start ('<'). Line number 1.");

    /* not supported DOCTYPE */
    str = "<!DOCTYPE greeting SYSTEM \"hello.dtd\"><greeting/>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Document Type Declaration not supported. Line number 1.");

    /* invalid XML */
    str = "<!NONSENSE/>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Unknown XML section \"<!NONSENSE/>\". Line number 1.");

    /* unqualified element */
    str = "  <  element/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_null(xmlctx->prefix);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_int_equal(1, xmlctx->elements.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("", xmlctx->value, xmlctx->value_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* element with attribute */
    str = "  <  element attr=\'x\'/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);
    assert_int_equal(1, xmlctx->elements.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTRIBUTE, xmlctx->status);
    assert_true(!strncmp("attr", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_int_equal(1, xmlctx->elements.count);
    assert_true(!strncmp("x", xmlctx->value, xmlctx->value_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);
    assert_int_equal(0, xmlctx->elements.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* headers and comments */
    str = "<?xml version=\"1.0\"?>  <!-- comment --> <![CDATA[<greeting>Hello, world!</greeting>]]> <?TEST xxx?> <element/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);
    assert_int_equal(1, xmlctx->elements.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* separate opening and closing tags, neamespaced parsed internally */
    str = "<element xmlns=\"urn\"></element>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);
    assert_int_equal(1, xmlctx->elements.count);
    assert_int_equal(1, xmlctx->ns.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);
    assert_int_equal(0, xmlctx->elements.count);
    assert_int_equal(0, xmlctx->ns.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* qualified element */
    str = "  <  yin:element/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_true(!strncmp("yin", xmlctx->prefix, xmlctx->prefix_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* non-matching closing tag */
    str = "<yin:element xmlns=\"urn\"></element>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("element", xmlctx->name, xmlctx->name_len));
    assert_true(!strncmp("yin", xmlctx->prefix, xmlctx->prefix_len));
    assert_int_equal(1, xmlctx->elements.count);
    assert_int_equal(1, xmlctx->ns.count);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Opening (\"yin:element\") and closing (\"element\") elements tag mismatch. Line number 1.");

    /* just replace closing tag */
    xmlctx->input = "</yin:element/>";
    xmlctx->status = LYXML_ELEM_CONTENT;
    xmlctx->dynamic = 0;
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character sequence \"/>\", expected element tag termination ('>'). Line number 1.");
    lyxml_ctx_free(xmlctx);

    /* UTF8 characters */
    str = "<𠜎€𠜎Øn:𠜎€𠜎Øn/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_true(!strncmp("𠜎€𠜎Øn", xmlctx->name, xmlctx->name_len));
    assert_true(!strncmp("𠜎€𠜎Øn", xmlctx->prefix, xmlctx->prefix_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* invalid UTF-8 characters */
    str = "<¢:element>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Identifier \"¢:element>\" starts with an invalid character. Line number 1.");

    str = "<yin:c⁐element>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character sequence \"⁐element>\", expected element tag end ('>' or '/>') or an attribute. Line number 1.");
    lyxml_ctx_free(xmlctx);

    /* mixed content */
    str = "<a>text <b>x</b></a>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("a", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("text ", xmlctx->value, xmlctx->value_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("b", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("x", xmlctx->value, xmlctx->value_len));

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    lyxml_ctx_free(xmlctx);

    /* tag mismatch */
    str = "<a>text</b>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_true(!strncmp("a", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("text", xmlctx->value, xmlctx->value_len));

    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Opening (\"a\") and closing (\"b\") elements tag mismatch. Line number 1.");
    lyxml_ctx_free(xmlctx);
}

static void
test_attribute(void **state)
{
    const char *str;
    struct lyxml_ctx *xmlctx;
    struct lyxml_ns *ns;

    /* not an attribute */
    str = "<e unknown/>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Invalid character sequence \"/>\", expected '='. Line number 1.");

    str = "<e xxx=/>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Invalid character sequence \"/>\", expected either single or double quotation mark. Line number 1.");

    str = "<e xxx\n = yyy/>";
    assert_int_equal(LY_EVALID, lyxml_ctx_new(*state, str, &xmlctx));
    logbuf_assert("Invalid character sequence \"yyy/>\", expected either single or double quotation mark. Line number 2.");

    /* valid attribute */
    str = "<e attr=\"val\"";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTRIBUTE, xmlctx->status);
    assert_true(!strncmp("attr", xmlctx->name, xmlctx->name_len));
    assert_null(xmlctx->prefix);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_true(!strncmp("val", xmlctx->name, xmlctx->name_len));
    assert_int_equal(xmlctx->ws_only, 0);
    assert_int_equal(xmlctx->dynamic, 0);
    lyxml_ctx_free(xmlctx);

    /* valid namespace with prefix */
    str = "<e xmlns:nc\n = \'urn\'/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_int_equal(1, xmlctx->ns.count);
    ns = (struct lyxml_ns *)xmlctx->ns.objs[0];
    assert_string_equal(ns->prefix, "nc");
    assert_string_equal(ns->uri, "urn");
    lyxml_ctx_free(xmlctx);
}

static void
test_text(void **state)
{
    const char *str;
    struct lyxml_ctx *xmlctx;

    /* empty attribute value */
    str = "<e a=\"\"";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTRIBUTE, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_true(!strncmp("", xmlctx->value, xmlctx->value_len));
    assert_int_equal(xmlctx->ws_only, 1);
    assert_int_equal(xmlctx->dynamic, 0);

    /* empty value but in single quotes */
    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'\'";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_true(!strncmp("", xmlctx->value, xmlctx->value_len));
    assert_int_equal(xmlctx->ws_only, 1);
    assert_int_equal(xmlctx->dynamic, 0);

    /* empty element content - only formating before defining child */
    xmlctx->status = LYXML_ELEMENT;
    xmlctx->input = ">\n  <y>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("\n  ", xmlctx->value, xmlctx->value_len));
    assert_int_equal(xmlctx->ws_only, 1);
    assert_int_equal(xmlctx->dynamic, 0);

    /* empty element content is invalid - missing content terminating character < */
    xmlctx->status = LYXML_ELEM_CONTENT;
    xmlctx->input = "";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Unexpected end-of-input. Line number 2.");

    xmlctx->status = LYXML_ELEM_CONTENT;
    xmlctx->input = "xxx";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character sequence \"xxx\", expected element tag start ('<'). Line number 2.");

    lyxml_ctx_free(xmlctx);

    /* valid strings */
    str = "<a>€𠜎Øn \n&lt;&amp;&quot;&apos;&gt; &#82;&#x4f;&#x4B;</a>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_true(!strncmp("€𠜎Øn \n<&\"\'> ROK", xmlctx->value, xmlctx->value_len));
    assert_int_equal(xmlctx->ws_only, 0);
    assert_int_equal(xmlctx->dynamic, 1);
    free((char *)xmlctx->value);

    /* test using n-bytes UTF8 hexadecimal code points */
    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'&#x0024;&#x00A2;&#x20ac;&#x10348;\'";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_true(!strncmp("$¢€𐍈", xmlctx->value, xmlctx->value_len));
    assert_int_equal(xmlctx->ws_only, 0);
    assert_int_equal(xmlctx->dynamic, 1);
    free((char *)xmlctx->value);

    /* invalid characters in string */
    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'&#x52\'";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character sequence \"'\", expected ;. Line number 2.");

    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\"&#82\"";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character sequence \"\"\", expected ;. Line number 2.");

    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\"&nonsense;\"";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Entity reference \"&nonsense;\" not supported, only predefined references allowed. Line number 2.");

    xmlctx->status = LYXML_ELEMENT;
    xmlctx->input = ">&#o122;";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character reference \"&#o122;\". Line number 2.");

    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'&#x06;\'";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character reference \"&#x06;\'\" (0x00000006). Line number 2.");

    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'&#xfdd0;\'";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character reference \"&#xfdd0;\'\" (0x0000fdd0). Line number 2.");

    xmlctx->status = LYXML_ATTRIBUTE;
    xmlctx->input = "=\'&#xffff;\'";
    assert_int_equal(LY_EVALID, lyxml_ctx_next(xmlctx));
    logbuf_assert("Invalid character reference \"&#xffff;\'\" (0x0000ffff). Line number 2.");

    lyxml_ctx_free(xmlctx);
}

static void
test_ns(void **state)
{
    const char *str;
    struct lyxml_ctx *xmlctx;
    const struct lyxml_ns *ns;

    /* opening element1 */
    str = "<element1/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));

    /* processing namespace definitions */
    assert_int_equal(LY_SUCCESS, lyxml_ns_add(xmlctx, NULL, 0, strdup("urn:default")));
    assert_int_equal(LY_SUCCESS, lyxml_ns_add(xmlctx, "nc", 2, strdup("urn:nc1")));
    /* simulate adding open element2 into context */
    xmlctx->elements.count++;
    /* processing namespace definitions */
    assert_int_equal(LY_SUCCESS, lyxml_ns_add(xmlctx, "nc", 2, strdup("urn:nc2")));
    assert_int_equal(3, xmlctx->ns.count);
    assert_int_not_equal(0, xmlctx->ns.size);

    ns = lyxml_ns_get(xmlctx, NULL, 0);
    assert_non_null(ns);
    assert_null(ns->prefix);
    assert_string_equal("urn:default", ns->uri);

    ns = lyxml_ns_get(xmlctx, "nc", 2);
    assert_non_null(ns);
    assert_string_equal("nc", ns->prefix);
    assert_string_equal("urn:nc2", ns->uri);

    /* simulate closing element2 */
    xmlctx->elements.count--;
    lyxml_ns_rm(xmlctx);
    assert_int_equal(2, xmlctx->ns.count);

    ns = lyxml_ns_get(xmlctx, "nc", 2);
    assert_non_null(ns);
    assert_string_equal("nc", ns->prefix);
    assert_string_equal("urn:nc1", ns->uri);

    /* close element1 */
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(0, xmlctx->ns.count);

    assert_null(lyxml_ns_get(xmlctx, "nc", 2));
    assert_null(lyxml_ns_get(xmlctx, NULL, 0));

    lyxml_ctx_free(xmlctx);
}

static void
test_ns2(void **state)
{
    const char *str;
    struct lyxml_ctx *xmlctx;

    /* opening element1 */
    str = "<element1/>";
    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, str, &xmlctx));

    /* default namespace defined in parent element1 */
    assert_int_equal(LY_SUCCESS, lyxml_ns_add(xmlctx, NULL, 0, strdup("urn:default")));
    assert_int_equal(1, xmlctx->ns.count);
    /* going into child element1 */
    /* simulate adding open element1 into context */
    xmlctx->elements.count++;
    /* no namespace defined, going out (first, simulate closing of so far open element) */
    xmlctx->elements.count--;
    lyxml_ns_rm(xmlctx);
    assert_int_equal(1, xmlctx->ns.count);

    /* nothing else, going out of the parent element1 */
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(0, xmlctx->ns.count);

    lyxml_ctx_free(xmlctx);
}

static void
test_simple_xml(void **state)
{
    struct lyxml_ctx *xmlctx;
    const char *test_input = "<elem1 attr1=\"value\"> <elem2 attr2=\"value\" /> </elem1>";

    assert_int_equal(LY_SUCCESS, lyxml_ctx_new(*state, test_input, &xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_string_equal(xmlctx->input, "attr1=\"value\"> <elem2 attr2=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTRIBUTE, xmlctx->status);
    assert_string_equal(xmlctx->input, "=\"value\"> <elem2 attr2=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_string_equal(xmlctx->input, "> <elem2 attr2=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_string_equal(xmlctx->input, "<elem2 attr2=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEMENT, xmlctx->status);
    assert_string_equal(xmlctx->input, "attr2=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTRIBUTE, xmlctx->status);
    assert_string_equal(xmlctx->input, "=\"value\" /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ATTR_CONTENT, xmlctx->status);
    assert_string_equal(xmlctx->input, " /> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CONTENT, xmlctx->status);
    assert_string_equal(xmlctx->input, "/> </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);
    assert_string_equal(xmlctx->input, " </elem1>");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_ELEM_CLOSE, xmlctx->status);
    assert_string_equal(xmlctx->input, "");

    assert_int_equal(LY_SUCCESS, lyxml_ctx_next(xmlctx));
    assert_int_equal(LYXML_END, xmlctx->status);
    assert_string_equal(xmlctx->input, "");

    lyxml_ctx_free(xmlctx);
}

#endif

int main(void)
{
    const struct CMUnitTest tests[] = {
        cmocka_unit_test_setup_teardown(test_general, logger_setup, logger_teardown),
        cmocka_unit_test_setup_teardown(test_number, logger_setup, logger_teardown),
    };

    return cmocka_run_group_tests(tests, setup, teardown);
}
