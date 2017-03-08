//===--- Format.cpp - Declaration Syntax Formatting Impl. -----------------===//
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

#include "swift/Syntax/Format.h"
#include "swift/Syntax/DeclSyntax.h"
#include "swift/Syntax/ExprSyntax.h"

using namespace swift;
using namespace swift::syntax;
using llvm::cast;

StructDeclSyntax
FormatSyntaxRewriter::rewriteStructDecl(StructDeclSyntax Struct) {
  return Struct;
}

Syntax syntax::format(Syntax Tree) {
  switch (Tree.getKind()) {
  case SyntaxKind::StructDecl: {
    auto Struct = Tree.castTo<StructDeclSyntax>();
    auto LeftBrace = Struct.getLeftBraceToken();
    if (!LeftBrace->LeadingTrivia.contains(TriviaKind::Newline)) {
      auto NewLeading = Trivia::newlines(1) + LeftBrace->LeadingTrivia;

      const auto NewMembers =
        format(Struct.getMembers()).castTo<DeclMembersSyntax>();

      LeftBrace = LeftBrace->withLeadingTrivia(NewLeading);

      return Struct.withMembers(NewMembers).withLeftBrace(LeftBrace);
    }
    break;
  }
  case SyntaxKind::DeclMembers: {
    auto Members = Tree.castTo<DeclMembersSyntax>();
    // TODO
    break;
  }
  default:
    return Tree;
  }

  return Tree;
}
