//===--- CSClosure.cpp - Closures -----------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file implements constraint generation and solution application for
// closures. It provides part of the implementation of the ConstraintSystem
// class.
//
//===----------------------------------------------------------------------===//

#include "ConstraintSystem.h"
using namespace swift;
using namespace swift::constraints;

namespace {

/// Statement visitor that generates constraints for a given closure body.
class ClosureConstraintGenerator
    : public StmtVisitor<ClosureConstraintGenerator, void> {
  friend StmtVisitor<ClosureConstraintGenerator, void>;

  ConstraintSystem &cs;
  ClosureExpr *closure;
  Type closureResultType;

public:
  /// Whether an error was encountered while generating constraints.
  bool hadError = false;

  ClosureConstraintGenerator(ConstraintSystem &cs, ClosureExpr *closure,
                             Type closureResultType)
    : cs(cs), closure(closure), closureResultType(closureResultType) { }

private:
  void visitDecl(Decl *decl) {
    // Just ignore #if; the chosen children should appear in the
    // surrounding context.  This isn't good for source tools but it
    // at least works.
    if (isa<IfConfigDecl>(decl))
      return;

    // Skip #warning/#error; we'll handle them when applying the closure.
    if (isa<PoundDiagnosticDecl>(decl))
      return;

    // Ignore variable declarations, because they're always handled within
    // their enclosing pattern bindings.
    if (isa<VarDecl>(decl))
      return;

    llvm_unreachable("Unimplemented case for closure body");
  }

  void visitBraceStmt(BraceStmt *braceStmt) {
    for (auto node : braceStmt->getElements()) {
      if (auto expr = node.dyn_cast<Expr *>()) {
        auto generatedExpr = cs.generateConstraints(
            expr, closure, /*isInputExpression=*/false);
        if (!generatedExpr) {
          hadError = true;
        }
      } else if (auto stmt = node.dyn_cast<Stmt *>()) {
        visit(stmt);
      } else {
        visitDecl(node.get<Decl *>());
      }
    }
  }

  void visitReturnStmt(ReturnStmt *returnStmt) {
    auto expr = returnStmt->getResult();

    // FIXME: Implies Void return?
    if (!expr)
      return;

    expr = cs.generateConstraints(expr, closure, /*isInputExpression=*/false);
    if (!expr) {
      hadError = true;
      return;
    }

    // FIXME: Locator should point at the return statement?
    bool hasReturn = hasExplicitResult(closure);
    cs.addConstraint(
        ConstraintKind::Conversion, cs.getType(expr),
        closureResultType,
        cs.getConstraintLocator(
           closure, LocatorPathElt::ClosureBody(hasReturn)));
  }

#define UNSUPPORTED_STMT(STMT) void visit##STMT##Stmt(STMT##Stmt *) { \
      llvm_unreachable("Unsupported statement kind " #STMT);          \
  }
  UNSUPPORTED_STMT(Yield)
  UNSUPPORTED_STMT(Defer)
  UNSUPPORTED_STMT(If)
  UNSUPPORTED_STMT(Guard)
  UNSUPPORTED_STMT(While)
  UNSUPPORTED_STMT(Do)
  UNSUPPORTED_STMT(DoCatch)
  UNSUPPORTED_STMT(RepeatWhile)
  UNSUPPORTED_STMT(ForEach)
  UNSUPPORTED_STMT(Switch)
  UNSUPPORTED_STMT(Case)
  UNSUPPORTED_STMT(Break)
  UNSUPPORTED_STMT(Continue)
  UNSUPPORTED_STMT(Fallthrough)
  UNSUPPORTED_STMT(Fail)
  UNSUPPORTED_STMT(Throw)
  UNSUPPORTED_STMT(PoundAssert)
#undef UNSUPPORTED_STMT
};

}

bool ConstraintSystem::generateConstraints(
    ClosureExpr *closure, Type resultType) {
  ClosureConstraintGenerator generator(*this, closure, resultType);
  generator.visit(closure->getBody());
  return generator.hadError;
}
