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

#include "swift/AST/Expr.h"
#include "swift/AST/Stmt.h"
#include "llvm/DerivedTypes.h"
#include "llvm/Function.h"

#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "Explosion.h"

#include "GenClosure.h"

using namespace swift;
using namespace irgen;

static void emitClosureBody(IRGenFunction &IGF, ClosureExpr *E) {
  // FIXME: Need to set up captures.

  // Emit the body of the closure as if it were a single return
  // statement.
  ReturnStmt ret(SourceLoc(), E->getBody());
  IGF.emitStmt(&ret);
}

void swift::irgen::emitClosure(IRGenFunction &IGF, ClosureExpr *E,
                               Explosion &explosion) {
  if (!E->getCaptures().empty()) {
    IGF.unimplemented(E->getLoc(), "cannot capture local vars yet");
    return IGF.emitFakeExplosion(IGF.getFragileTypeInfo(E->getType()),
                                 explosion);
  }

  // Create the IR function.
  llvm::FunctionType *fnType =
      IGF.IGM.getFunctionType(E->getType(), ExplosionKind::Minimal, 0, false);
  llvm::Function *fn =
      llvm::Function::Create(fnType, llvm::GlobalValue::InternalLinkage,
                             "closure", &IGF.IGM.Module);

  // Go ahead and build the explosion result now.
  explosion.add(IGF.Builder.CreateBitCast(fn, IGF.IGM.Int8PtrTy));
  explosion.add(IGF.IGM.RefCountedNull);

  IRGenFunction innerIGF(IGF.IGM, E->getType(), E->getParamPatterns(),
                         ExplosionKind::Minimal, /*uncurry level*/ 0, fn);

  emitClosureBody(innerIGF, E);
}
