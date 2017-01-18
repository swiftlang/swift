//===--- GenInit.cpp - IR Generation for Initialization -------------------===//
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
//  This file implements IR generation for the initialization of
//  local and global variables.
//
//
//===----------------------------------------------------------------------===//

#include "swift/AST/Pattern.h"
#include "swift/SIL/SILDeclRef.h"
#include "swift/SIL/SILGlobalVariable.h"
#include "llvm/IR/GlobalVariable.h"

#include "DebugTypeInfo.h"
#include "Explosion.h"
#include "GenHeap.h"
#include "GenTuple.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "FixedTypeInfo.h"

using namespace swift;
using namespace irgen;

/// Emit a global variable.
Address IRGenModule::emitSILGlobalVariable(SILGlobalVariable *var) {
  auto &ti = getTypeInfo(var->getLoweredType());
  
  // If the variable is empty in all resilience domains, don't actually emit it;
  // just return undef.
  if (ti.isKnownEmpty(ResilienceExpansion::Minimal)) {
    if (DebugInfo && var->getDecl()) {
      auto DbgTy = DebugTypeInfo::getGlobal(var, Int8Ty, Size(0), Alignment(1));
      DebugInfo->emitGlobalVariableDeclaration(
          nullptr, var->getDecl()->getName().str(), "", DbgTy,
          var->getLinkage() != SILLinkage::Public, SILLocation(var->getDecl()));
    }
    return ti.getUndefAddress();
  }

  /// Get the global variable.
  Address addr = getAddrOfSILGlobalVariable(var, ti,
                     var->isDefinition() ? ForDefinition : NotForDefinition);
  
  /// Add a zero initializer.
  if (var->isDefinition()) {
    auto gvar = cast<llvm::GlobalVariable>(addr.getAddress());
    gvar->setInitializer(llvm::Constant::getNullValue(gvar->getValueType()));
  }

  return addr;
}

StackAddress FixedTypeInfo::allocateStack(IRGenFunction &IGF, SILType T,
                                          bool isEntryBlock,
                                          const Twine &name) const {
  // If the type is known to be empty, don't actually allocate anything.
  if (isKnownEmpty(ResilienceExpansion::Maximal)) {
    auto addr = getUndefAddress();
    return { addr };
  }

  Address alloca =
    IGF.createAlloca(getStorageType(), getFixedAlignment(), name);
  IGF.Builder.CreateLifetimeStart(alloca, getFixedSize());
  
  return { alloca };
}

void FixedTypeInfo::destroyStack(IRGenFunction &IGF, StackAddress addr,
                                 SILType T) const {
  destroy(IGF, addr.getAddress(), T);
  FixedTypeInfo::deallocateStack(IGF, addr, T);
}

void FixedTypeInfo::deallocateStack(IRGenFunction &IGF, StackAddress addr,
                                    SILType T) const {
  if (isKnownEmpty(ResilienceExpansion::Maximal))
    return;
  IGF.Builder.CreateLifetimeEnd(addr.getAddress(), getFixedSize());
}
