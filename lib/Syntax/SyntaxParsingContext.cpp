//===--- SyntaxParsingContext.cpp - Syntax Tree Parsing Support------------===//
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

#include "swift/AST/Module.h"
#include "swift/Basic/Defer.h"
#include "swift/Parse/Parser.h"
#include "swift/Syntax/TokenSyntax.h"
#include "swift/Syntax/SyntaxParsingContext.h"
#include "swift/Syntax/SyntaxFactory.h"

using namespace swift;
using namespace swift::syntax;

namespace {
static TokenSyntax getTokenAtLocation(ArrayRef<RawTokenInfo> Tokens,
                                      SourceLoc Loc) {
  auto It = std::lower_bound(Tokens.begin(), Tokens.end(), Loc,
   [](const RawTokenInfo &Info, SourceLoc Loc) {
     return Info.Loc.getOpaquePointerValue() < Loc.getOpaquePointerValue();
   });
  assert(It->Loc == Loc);
  return make<TokenSyntax>(It->Token);
}
}

EnabledSyntaxParsingContext::
EnabledSyntaxParsingContext(SourceFile &File, unsigned BufferID): File(File) {
  populateTokenSyntaxMap(File.getASTContext().LangOpts,
                         File.getASTContext().SourceMgr,
                         BufferID, File.AllRawTokenSyntax);
}

EnabledSyntaxParsingContext::~EnabledSyntaxParsingContext() {
  std::transform(PendingSyntax.begin(), PendingSyntax.end(),
                 std::back_inserter(File.SyntaxNodes),
                 [](Syntax &S) { return make<Syntax>(S.getRaw()); });
}

Optional<TokenSyntax> EnabledSyntaxParsingContext::checkBackToken(tok Kind) {
  if (PendingSyntax.empty())
    return None;
  auto Back = PendingSyntax.back().getAs<TokenSyntax>();
  if (Back.hasValue() && (*Back).getTokenKind() == Kind) {
    PendingSyntax.pop_back();
    return Back;
  }
  return None;
}

void EnabledSyntaxParsingContext::addTokenSyntax(SourceLoc Loc) {
  PendingSyntax.emplace_back(getTokenAtLocation(File.getSyntaxTokens(), Loc));
}

void EnabledSyntaxParsingContext::makeIntegerLiteralExp() {
  auto Digit = *PendingSyntax.back().getAs<TokenSyntax>();
  PendingSyntax.pop_back();
  PendingSyntax.push_back(SyntaxFactory::makeIntegerLiteralExpr(
    checkBackToken(tok::oper_prefix), Digit));
}

