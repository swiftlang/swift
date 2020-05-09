//===--- SyntaxASTMap.cpp - Syntax -> AST Map manager ---------------------===//
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

#include "swift/AST/SyntaxASTMap.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"
#include "swift/Syntax/Syntax.h"

using namespace swift;
using namespace swift::syntax;

void
SyntaxASTMap::recordSyntaxMapping(RC<syntax::SyntaxData> FromNode,
                                  ASTNode ToNode) {
  if (FromNode->getKind() == SyntaxKind::Unknown) {
    return;
  }

  SyntaxMap[FromNode] = ToNode;
}


llvm::Optional<ASTNode>
SyntaxASTMap::getNodeForSyntax(syntax::Syntax SyntaxNode) const {
  auto Found = SyntaxMap.find(SyntaxNode.Root);
  if (Found == SyntaxMap.end()) {
    return None;
  }
  return Found->getSecond();
}

void SyntaxASTMap::clearSyntaxMap() {
  SyntaxMap.shrink_and_clear();
}

void SyntaxASTMap::dumpSyntaxMap() const {
  for (const auto &SyntaxAndSemaNode : SyntaxMap) {
    auto SyntaxNode = SyntaxAndSemaNode.getFirst();
    auto SemanticNode = SyntaxAndSemaNode.getSecond();

    llvm::errs() << "\n=====================================================\n";
    SyntaxNode->dump(llvm::errs());
    llvm::errs() << "\n\n---- Maps to semantic node: ----\n\n";

    if (SemanticNode.is<Expr *>()) {
      SemanticNode.get<Expr *>()->dump(llvm::errs());
      llvm::errs() << "\n";
    } else if (SemanticNode.is<Decl *>()) {
      SemanticNode.get<Decl *>()->dump(llvm::errs());
    } else if (SemanticNode.is<Expr *>()) {
      SemanticNode.get<Expr *>()->dump(llvm::errs());
      llvm::errs() << "\n";
    } else {
      llvm_unreachable("ASTNode has pointer to unknown thing!");
    }
    llvm::errs() << "\n=====================================================\n";
  }
}
