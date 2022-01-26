//===--- ParseRegex.cpp - Regular expression literal parsing --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// Regular expression literal parsing
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/Parser.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/ParsedSyntaxRecorder.h"
#include "swift/Parse/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxKind.h"

// Regex parser delivered via Swift modules.
#include "swift/Parse/ExperimentalRegexBridging.h"
static RegexLiteralParsingFn regexLiteralParsingFn = nullptr;
void Parser_registerRegexLiteralParsingFn(RegexLiteralParsingFn fn) {
  regexLiteralParsingFn = fn;
}

using namespace swift;
using namespace swift::syntax;

ParserResult<Expr> Parser::parseExprRegexLiteral() {
  diagnose(Tok, diag::regex_literal_parsing_error, "found regex literal");
  auto loc = consumeToken();
  return makeParserResult(new (Context) ErrorExpr(loc));
}
