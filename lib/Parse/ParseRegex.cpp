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

// Regex parser delivered via libSwift
#include "swift/Parse/ExperimentalRegexBridging.h"
static ParseRegexStrawperson parseRegexStrawperson = nullptr;
void Parser_registerParseRegexStrawperson(ParseRegexStrawperson fn) {
  parseRegexStrawperson = fn;
}
// Exposes the presence of the regex parsing function to the lexer.
bool Parser_hasParseRegexStrawperson() {
  return parseRegexStrawperson != nullptr;
}

using namespace swift;
using namespace swift::syntax;

ParserResult<Expr> Parser::parseExprRegexLiteral() {
  assert(Tok.is(tok::regex_literal));
  assert(parseRegexStrawperson);

  SyntaxParsingContext LocalContext(SyntaxContext,
                                    SyntaxKind::RegexLiteralExpr);
  // Strip off delimiters.
  auto rawText = Tok.getText();
  assert(rawText.front() == '\'' && rawText.back() == '\'');
  auto regexText = rawText.slice(1, rawText.size() - 1);

  // Let the Swift library parse the contents, returning an error, or null if
  // successful.
  // TODO: We need to be able to pass back a source location to emit the error
  // at.
  auto *errorStr = parseRegexStrawperson(regexText.str().c_str());
  if (errorStr)
    diagnose(Tok, diag::regex_literal_parsing_error, errorStr);

  auto loc = consumeToken();
  return makeParserResult(
      RegexLiteralExpr::createParsed(Context, loc, regexText));
}
