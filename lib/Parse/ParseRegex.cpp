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
#include "swift/Parse/RegexParserBridging.h"
static RegexLiteralParsingFn regexLiteralParsingFn = nullptr;
void Parser_registerRegexLiteralParsingFn(RegexLiteralParsingFn fn) {
  regexLiteralParsingFn = fn;
}

using namespace swift;
using namespace swift::syntax;

ParserResult<Expr> Parser::parseExprRegexLiteral() {
  assert(Tok.is(tok::regex_literal));
  assert(regexLiteralParsingFn);

  SyntaxParsingContext LocalContext(SyntaxContext,
                                    SyntaxKind::RegexLiteralExpr);

  auto regexText = Tok.getText();

  // Let the Swift library parse the contents, returning an error, or null if
  // successful.
  // TODO: We need to be able to pass back a source location to emit the error
  // at.
  const char *errorStr = nullptr;
  unsigned version;
  auto capturesBuf = Context.AllocateUninitialized<uint8_t>(
      RegexLiteralExpr::getCaptureStructureSerializationAllocationSize(
          regexText.size()));
  regexLiteralParsingFn(regexText.str().c_str(), &errorStr, &version,
                        /*captureStructureOut*/ capturesBuf.data(),
                        /*captureStructureSize*/ capturesBuf.size());
  auto loc = consumeToken();
  if (errorStr) {
    diagnose(loc, diag::regex_literal_parsing_error, errorStr);
    return makeParserResult(new (Context) ErrorExpr(loc));
  }
  return makeParserResult(RegexLiteralExpr::createParsed(
      Context, loc, regexText, version, capturesBuf));
}
