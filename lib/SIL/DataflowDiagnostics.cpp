//===--- Checker.cpp - Analyzes and checking of Swift SIL Code ------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#include "swift/AST/ASTContext.h"
#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/Diagnostics.h"
#include "swift/AST/Expr.h"
#include "swift/SIL/SILFunction.h"
#include "swift/SIL/SILLocation.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"

#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"

using namespace swift;

namespace {

template<typename...T, typename...U>
void diagnose(ASTContext &Context, SourceLoc loc, Diag<T...> diag,
              U &&...args) {
  Context.Diags.diagnose(loc,
                         diag, std::forward<U>(args)...);
}

static void diagnoseUnreachable(const SILInstruction *I,
                                ASTContext &Context) {
  if (auto *UI = dyn_cast<UnreachableInst>(I)){
    const SILBasicBlock *BB = UI->getParent();
    const SILFunction *F = BB->getParent();

    // The most common case of getting an unreachable instruction is a
    // missing return statement.
    SILLocation FLoc = F->getLocation();

    Type ResTy;

    // Should be an Expr as it's the parent of the basic block.
    if (const FuncExpr *FExpr = FLoc.getAs<FuncExpr>())
      ResTy = FExpr->getResultType(Context);
    else {
      // FIXME: Not all closure types have the result type getter right
      // now.
      return;
    }

    // If the function does not return void, issue the diagnostic.
    if (!ResTy->isVoid()) {
      SILLocation L = UI->getLoc();
      if (L)
        diagnose(Context,
                 L.getEndSourceLoc(),
                 diag::missing_return, ResTy.getString());
    }
  }
}
}; // end of anonymous namespace

void SILModule::check() const {
  const SILModule *M = this;
  for (auto &Fn : *M)
    for (auto &BB : Fn)
      for (auto &I : BB)
        diagnoseUnreachable(&I, getASTContext());
}
