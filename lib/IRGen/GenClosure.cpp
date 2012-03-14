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
#include "RValue.h"
#include "Explosion.h"

using namespace swift;
using namespace irgen;

void IRGenFunction::emitExplodedClosure(ClosureExpr *E,
                                        Explosion &explosion) {
  llvm::FunctionType *fnType =
      IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0, true);
  llvm::Function *Func =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGM.Module);
  IRGenFunction(IGM, 0, ExplosionKind::Minimal, 0, Func, Prologue::Bare)
      .emitClosureBody(E);
  explosion.add(Builder.CreateBitCast(Func, IGM.Int8PtrTy));
  explosion.add(llvm::UndefValue::get(IGM.Int8PtrTy));
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
      std::string Name("$");
      Name += '0' + i;
      ClosureParams.push_back(getAddrForParameter(ArgType, Name,
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
  emitExplodedRValue(E->getBody(), result);
  emitScalarReturn(result);
}
