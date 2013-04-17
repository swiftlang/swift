//===--- GenConstructor.cpp - IR Generation for Constructors --------------===//
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
#include "Callee.h"
#include "GenInit.h"
#include "TypeInfo.h"
#include "swift/AST/Decl.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Pattern.h"
#include "swift/AST/Types.h"
#include "llvm/IR/Function.h"
using namespace swift;
using namespace irgen;

void IRGenFunction::emitConstructorBody(ConstructorDecl *CD) {
  // FIXME: Member init here?

  // Emit explicit body, if present.
  if (CD->getBody()) {
    // From the body's point of view, there is no return slot.
    Address ThisReturnSlot = ReturnSlot;
    ReturnSlot = Address();
    
    emitFunctionTopLevel(CD->getBody());

    ReturnSlot = ThisReturnSlot;
  }
  
  // Return "this".
  auto thisDecl = CD->getImplicitThisDecl();
  const TypeInfo &type = getFragileTypeInfo(thisDecl->getType());
  OwnedAddress addr = getLocalVar(thisDecl);
  type.initializeWithCopy(*this, ReturnSlot, addr);
  emitBranch(JumpDest(ReturnBB, Cleanups.stable_end()));
  Builder.ClearInsertionPoint();
}

