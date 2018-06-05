//===--- TypeCheckCompilerEvaluable.cpp - Check compiler evaluability -----===//
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
//
// SWIFT_ENABLE_TENSORFLOW
// Checks that function bodies follow rules for compiler evaluable functions.
//
//===----------------------------------------------------------------------===//

#include "TypeChecker.h"
#include "swift/AST/ASTWalker.h"
#include "swift/AST/Attr.h"
#include "swift/AST/Decl.h"

#include "llvm/Support/Debug.h"

using namespace swift;

namespace {

/// Checks that the body of a function is compiler evaluable.
/// Currently a skeleton implementation that only rejects while loops.
/// TODO(marcrasi): Fill in a real implementation.
class CheckCompilerEvaluableBody : public ASTWalker {
  TypeChecker &TC;
  bool compilerEvaluable = true;

 public:
  CheckCompilerEvaluableBody(TypeChecker &TC) : TC(TC) {}

  std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
    if (S->getKind() == StmtKind::While) {
      TC.diagnose(S->getStartLoc(), diag::compiler_evaluable_loop);
      compilerEvaluable = false;
      return {false, S};
    }
    return {true, S};
  }

  bool getCompilerEvaluable() const { return compilerEvaluable; }
};

}  // namespace

/// If the function has a valid @compilerEvaluable attribute, checks that the
/// function body follows all the rules for compiler evaluable functions.
///
/// The function body must already be type checked.
void TypeChecker::checkFunctionBodyCompilerEvaluable(AbstractFunctionDecl *D) {
  auto compilerEvaluableAttr =
      D->getAttrs().getAttribute<CompilerEvaluableAttr>();
  if (!compilerEvaluableAttr || !compilerEvaluableAttr->isValid()) return;

  assert(D->getBodyKind() == AbstractFunctionDecl::BodyKind::TypeChecked &&
         "cannot check @compilerEvaluable body that is not type checked");

  CheckCompilerEvaluableBody Checker(*this);
  D->getBody()->walk(Checker);
  if (!Checker.getCompilerEvaluable()) {
    compilerEvaluableAttr->setInvalid();
  }
}
