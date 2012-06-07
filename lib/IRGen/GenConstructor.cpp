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
//  This file implements IR generation for constructors.
//
//===----------------------------------------------------------------------===//

#include "IRGenModule.h"
#include "IRGenFunction.h"
#include "GenFunc.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "llvm/Function.h"

using namespace swift;
using namespace irgen;

void IRGenModule::emitConstructor(ConstructorDecl *CD) {
  llvm::Function *fn = getAddrOfConstructor(CD, ExplosionKind::Minimal);

  Pattern* pats[] = { new (Context) NamedPattern(CD->getImplicitThisDecl()),
                      CD->getArguments() };
  IRGenFunction IGF(*this, CD->getType(), pats,
                    ExplosionKind::Minimal, 1, fn, Prologue::Standard);

  IGF.emitFunctionTopLevel(CD->getBody());
}

void IRGenFunction::constructObject(Address addr, ConstructorDecl *CD,
                                    Expr *input) {
  llvm::Function *fn = IGM.getAddrOfConstructor(CD, ExplosionKind::Minimal);
  Callee c = Callee::forMethod(CD->getType(), fn, ExplosionKind::Minimal, 1);

  Explosion inputE(ExplosionKind::Minimal);
  emitRValue(input, inputE);
  Explosion thisE(ExplosionKind::Minimal);
  thisE.addUnmanaged(addr.getAddress());
  Arg args[] = { Arg::forUnowned(thisE), Arg::forUnowned(inputE) };

  Explosion Result(ExplosionKind::Minimal);
  emitCall(*this, c, args, getFragileTypeInfo(TupleType::getEmpty(IGM.Context)), Result);
}
