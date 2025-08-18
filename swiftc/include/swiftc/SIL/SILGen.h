#ifndef SWIFTC_SIL_SILGEN_H
#define SWIFTC_SIL_SILGEN_H

#include "swiftc/Basic/LLVM.h"
#include "swiftc/Basic/Diagnostic.h"
#include "swiftc/AST/ASTNode.h"
#include "swiftc/AST/Expr.h"
#include "swiftc/AST/Stmt.h"
#include "swiftc/AST/Decl.h"
#include "swiftc/SIL/SILFunction.h"
#include "swiftc/SIL/SILInstruction.h"
#include <memory>

namespace swiftc {

/// SIL generation from AST.
class SILGen {
  DiagnosticEngine& Diags;
  std::unique_ptr<SILModule> Module;
  SILFunction* CurrentFunction;
  SILBasicBlock* CurrentBlock;

public:
  SILGen(DiagnosticEngine& diags);

  /// Generate SIL for a list of top-level declarations.
  std::unique_ptr<SILModule> generateSIL(std::vector<std::unique_ptr<Decl>>& decls);

private:
  /// Generate SIL for a declaration.
  void generateDecl(Decl* decl);

  /// Generate SIL for a function declaration.
  void generateFuncDecl(FuncDecl* funcDecl);

  /// Generate SIL for a statement.
  SILValue generateStmt(Stmt* stmt);

  /// Generate SIL for an expression.
  SILValue generateExpr(Expr* expr);

  /// Create a new basic block.
  SILBasicBlock* createBasicBlock(StringRef label = "");

  /// Emit an instruction to the current basic block.
  template<typename T, typename... Args>
  T* emitInst(Args&&... args) {
    auto inst = std::make_unique<T>(std::forward<Args>(args)...);
    T* result = inst.get();
    CurrentBlock->addInstruction(std::move(inst));
    return result;
  }
};

} // namespace swiftc

#endif // SWIFTC_SIL_SILGEN_H