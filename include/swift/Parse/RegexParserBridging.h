//===-- RegexParserBridging.h --- Regex parser interface -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//


#ifndef REGEX_PARSER_BRIDGING
#define REGEX_PARSER_BRIDGING

#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/// Attempt to lex a regex literal string. Takes the following arguments:
///
/// - CurPtrPtr: A pointer to the current pointer of lexer, which should be the
///              start of the literal. This will be advanced to the point at
///              which the lexer should resume, or will remain the same if this
///              is not a regex literal.
/// - BufferEnd: A pointer to the end of the buffer, which should not be lexed
///              past.
/// - ErrorOut: If an error is encountered, this will be set to the error
///             string.
///
/// Returns: A bool indicating whether lexing was completely erroneous, and
///          cannot be recovered from, or false if there either was no error,
///          or there was a recoverable error.
typedef bool (* RegexLiteralLexingFn)(/*CurPtrPtr*/ const char **,
                                      /*BufferEnd*/ const char *,
                                      /*ErrorOut*/ const char **);
void Parser_registerRegexLiteralLexingFn(RegexLiteralLexingFn fn);

/// Parse a regex literal string. Takes the following arguments:
///
/// - InputPtr: A null-terminated C string of the regex literal.
/// - ErrorOut: A buffer accepting an error string upon error.
/// - VersionOut: A buffer accepting a regex literal format version.
/// - CaptureStructureOut: A buffer accepting a byte sequence representing the
///                        capture structure of the literal.
/// - CaptureStructureSize: The size of the capture structure buffer. Must be
///                         greater than or equal to `strlen(InputPtr) + 3`.
typedef void(* RegexLiteralParsingFn)(/*InputPtr*/ const char *,
                                      /*ErrorOut*/ const char **,
                                      /*VersionOut*/ unsigned *,
                                      /*CaptureStructureOut*/ void *,
                                      /*CaptureStructureSize*/ unsigned);
void Parser_registerRegexLiteralParsingFn(RegexLiteralParsingFn fn);

#ifdef __cplusplus
} // extern "C"
#endif

#endif // REGEX_PARSER_BRIDGING
