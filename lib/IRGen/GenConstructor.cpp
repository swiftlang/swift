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
#include "swift/AST/Decl.h"
#include "swift/AST/Pattern.h"

using namespace swift;
using namespace irgen;

void IRGenModule::emitConstructor(ConstructorDecl *CD) {
  llvm_unreachable("Not yet implemented!");
  llvm::Function *fn = getAddrOfConstructor(CD, ExplosionKind::Minimal);

  Pattern* pats[] = { new (Context) NamedPattern(CD->getImplicitThisDecl()),
                      CD->getArguments() };
  IRGenFunction IGF(*this, CD->getType(), pats,
                    ExplosionKind::Minimal, 1, fn, Prologue::Standard);

  IGF.emitFunctionTopLevel(CD->getBody());
}
