//===--- ASTGen.cpp -------------------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/Parse/ASTGen.h"
#include "swift/AST/TypeRepr.h"

using namespace swift;
using namespace swift::syntax;

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig, ASTContext &Context) {
  char *start = static_cast<char *>(Context.Allocate(Orig.size(), 1));
  char *p = start;

  if (p) {
    for (char c : Orig) {
      if (c != '_') {
        *p++ = c;
      }
    }
  }

  return StringRef(start, p - start);
}

//===----------------------------------------------------------------------===//
// MARK: - Private

DeclNameRef ASTGen::generateDeclNameRef(const syntax::DeclNameSyntax &DeclNameSyntax,
                                        const SourceLoc &Loc) {
  auto baseName = DeclNameSyntax.getDeclBaseName().castTo<TokenSyntax>();
  DeclBaseName declBaseName;
  switch (baseName.getTokenKind()) {
  case tok::kw_init:
    declBaseName = DeclBaseName::createConstructor();
    break;
  case tok::kw_deinit:
    declBaseName = DeclBaseName::createDestructor();
    break;
  case tok::kw_subscript:
    declBaseName = DeclBaseName::createSubscript();
    break;
  default:
    declBaseName =
        DeclBaseName(Context.getIdentifier(baseName.getIdentifierText()));
    break;
  }
  if (DeclNameSyntax.getDeclNameArguments().hasValue()) {
    SmallVector<Identifier, 2> argumentLabels;
    auto arguments = DeclNameSyntax.getDeclNameArguments()->getArguments();
    for (auto arg : arguments) {
      auto argName = arg.getName().getIdentifierText();
      argumentLabels.push_back(Context.getIdentifier(argName));
    }
    return DeclNameRef(DeclName(Context, declBaseName, argumentLabels));
  } else {
    return DeclNameRef(declBaseName);
  }
}

StringRef ASTGen::copyAndStripUnderscores(StringRef Orig) {
  return copyAndStripUnderscores(Orig, Context);
}

SourceLoc ASTGen::advanceLocBegin(const SourceLoc &Loc, const Syntax &Node) {
  return Loc.getAdvancedLoc(Node.getAbsolutePosition().getOffset());
}

SourceLoc ASTGen::advanceLocEnd(const SourceLoc &Loc, const Syntax &Node) {
  if (!Node.isMissing()) {
    // NOTE: We cannot use 'getLastToken()' because it doesn't take string
    // literal expressions into account.
    if (Node.isToken() || Node.is<StringLiteralExprSyntax>())
      return advanceLocBegin(Loc, Node);
    for (size_t I = Node.getNumChildren(); I != 0; --I)
      if (auto Child = Node.getChild(I - 1))
        return advanceLocEnd(Loc, *Child);
  }
  if (auto Prev = Node.getPreviousNode())
    return advanceLocEnd(Loc, *Prev);
  assert(false && "No tokens in tree?");
  return Loc;
}
