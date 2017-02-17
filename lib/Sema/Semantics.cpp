#include "swift/Sema/Semantics.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Stmt.h"

using namespace swift;
using namespace swift::sema;
using namespace swift::syntax;

void
Semantics::recordSyntaxMapping(syntax::RC<syntax::SyntaxData> FromNode,
                                   ASTNode ToNode) {
  if (FromNode->getKind() == SyntaxKind::Unknown) {
    return;
  }

  SyntaxMap[FromNode] = ToNode;
}


llvm::Optional<ASTNode>
Semantics::getNodeForSyntax(syntax::Syntax SyntaxNode) const {
  auto Found = SyntaxMap.find(SyntaxNode.Root);
  if (Found == SyntaxMap.end()) {
    return None;
  }
  return Found->getSecond();
}

void Semantics::clearSyntaxMap() {
  SyntaxMap.shrink_and_clear();
}

void Semantics::dumpSyntaxMap() const {
  for (const auto &SyntaxAndSemaNode : SyntaxMap) {
    auto SyntaxNode = SyntaxAndSemaNode.getFirst();
    auto SemanticNode = SyntaxAndSemaNode.getSecond();

    llvm::errs() << "\n=====================================================\n";
    SyntaxNode->dump(llvm::errs());
    llvm::errs() << "\n\n---- Maps to semantic node: ----\n\n";

    if (SemanticNode.is<Expr *>()) {
      SemanticNode.get<Expr *>()->dump(llvm::errs());
    } else if (SemanticNode.is<Decl *>()) {
      SemanticNode.get<Decl *>()->dump(llvm::errs());
    } else if (SemanticNode.is<Expr *>()) {
      SemanticNode.get<Expr *>()->dump(llvm::errs());
    } else {
      llvm_unreachable("ASTNode has pointer to unknown thing!");
    }
    llvm::errs() << "\n=====================================================\n";
  }
}
