//===--- GenClosure.cpp - Miscellaneous IR Generation for Expressions -----===//
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
//
//  This file implements IR generation for closures, e.g. ImplicitClosureExpr.
//
//===----------------------------------------------------------------------===//

#include "swift/AST/ASTWalker.h"
#include "swift/AST/Types.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/Basic/Optional.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"
#include "llvm/Target/TargetData.h"

#include "Cleanup.h"
#include "GenType.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "JumpDest.h"
#include "LValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

namespace {
  class FindCapturedVars : public ASTWalker {
    IRGenFunction &IGF;
    bool FoundVar;

  public:
    bool walkToExprPre(Expr *E) {
      if (DeclRefExpr *DRE = dyn_cast<DeclRefExpr>(E)) {
        if (DRE->getDecl()->getKind() == DeclKind::Var &&
            DRE->getDecl()->getDeclContext()->isLocalContext()) {
          IGF.IGM.unimplemented(E->getLoc(), "cannot capture local var yet");
          FoundVar = true;
        }
      }
      return true;
    }

    FindCapturedVars(IRGenFunction &igf) : IGF(igf), FoundVar(false) {}

    bool doWalk(Expr *E) {
      E->walk(*this);
      return FoundVar;
    }
  };
}

void IRGenFunction::emitClosure(ClosureExpr *E,
                                        Explosion &explosion) {
  if (FindCapturedVars(*this).doWalk(E->getBody()))
    return emitFakeExplosion(getFragileTypeInfo(E->getType()), explosion);

  llvm::FunctionType *fnType =
      IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0, true);
  llvm::Function *Func =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGM.Module);
  IRGenFunction(IGM, 0, ExplosionKind::Minimal, 0, Func, Prologue::Bare)
      .emitClosureBody(E);
  explosion.add(Builder.CreateBitCast(Func, IGM.Int8PtrTy));
  explosion.add(IGM.RefCountedNull);
}

void IRGenFunction::emitClosureBody(ClosureExpr *E) {
  // Emit $0..$n
  Explosion values = collectParameters();
  FunctionType *FT = E->getType()->getAs<FunctionType>();
  TupleType *FuncInputTT = dyn_cast<TupleType>(FT->getInput().getPointer());
  if (FuncInputTT) {
    unsigned NumInputArgs = FuncInputTT->getFields().size();
    for (unsigned i = 0; i < NumInputArgs; i++) {
      Type ArgType = FuncInputTT->getElementType(i);
      ClosureParams.push_back(getAddrForParameter(ArgType, "$" + Twine(i),
                                                  /*isByref*/false, values));
    }
  } else {
    ClosureParams.push_back(getAddrForParameter(FT->getInput(), "$0",
                                                /*isByref*/false, values));
  }

  // FIXME: Need to set up captures.

  // Emit the body of the closure
  FullExpr fullExpr(*this);
  Explosion result(CurExplosionLevel);
  emitRValue(E->getBody(), result);
  emitScalarReturn(result);
}
