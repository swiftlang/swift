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

#include "swift/AST/CASTBridging.h"
#include "swift/AST/DiagnosticsParse.h"
#include "swift/Parse/Parser.h"

extern "C" bool swift_ASTGen_parseRegexLiteral(
    BridgedString input, size_t * versionOut,
    void * UnsafeMutableRawPointer, size_t captureStructureSize,
    BridgedSourceLoc diagLoc, void * diagEngine);

using namespace swift;

ParserResult<Expr> Parser::parseExprRegexLiteral() {
  assert(Tok.is(tok::regex_literal));

#if SWIFT_BUILD_REGEX_PARSER_IN_COMPILER
  auto regexText = Tok.getText();

  // Let the Swift library parse the contents, returning an error, or null if
  // successful.
  size_t version = 0;
  auto capturesBuf = Context.AllocateUninitialized<uint8_t>(
      RegexLiteralExpr::getCaptureStructureSerializationAllocationSize(
          regexText.size()));
  bool hadError = swift_ASTGen_parseRegexLiteral(
      {(const unsigned char *)regexText.data(), SwiftInt(regexText.size())},
      /*versionOut=*/&version,
      /*captureStructureOut=*/capturesBuf.data(),
      /*captureStructureSize=*/capturesBuf.size(),
      /*diagBaseLoc=*/{Tok.getLoc().getOpaquePointerValue()}, &Diags);
  auto loc = consumeToken();
  SourceMgr.recordRegexLiteralStartLoc(loc);

  if (hadError) {
    return makeParserResult(new (Context) ErrorExpr(loc));
  }
  assert(version >= 1);
  return makeParserResult(RegexLiteralExpr::createParsed(
      Context, loc, regexText, version, capturesBuf));
#else
  llvm_unreachable("Lexer should not emit tok::regex_literal");
#endif
}
