//===--- GenThunk.cpp - IR Generation for Method Dispatch Thunks ----------===//
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
//  This file implements IR generation for class and protocol method dispatch
//  thunks, which are used in resilient builds to hide vtable and witness table
//  offsets from clients.
//
//===----------------------------------------------------------------------===//

#include "Callee.h"
#include "Explosion.h"
#include "GenDecl.h"
#include "GenOpaque.h"
#include "GenProto.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "ProtocolInfo.h"
#include "Signature.h"
#include "swift/IRGen/Linking.h"
#include "llvm/IR/Function.h"

using namespace swift;
using namespace irgen;

/// Find the entry point for a method dispatch thunk.
llvm::Function *
IRGenModule::getAddrOfDispatchThunk(SILDeclRef declRef,
                                    ForDefinition_t forDefinition) {
  LinkEntity entity = LinkEntity::forDispatchThunk(declRef);

  llvm::Function *&entry = GlobalFuncs[entity];
  if (entry) {
    if (forDefinition) updateLinkageForDefinition(*this, entry, entity);
    return entry;
  }

  auto fnType = getSILModule().Types.getConstantFunctionType(declRef);
  Signature signature = getSignature(fnType);
  LinkInfo link = LinkInfo::get(*this, entity, forDefinition);

  return createFunction(*this, link, signature);
}

static FunctionPointer lookupMethod(IRGenFunction &IGF,
                                    SILDeclRef declRef) {
  // Find the witness table.
  llvm::Value *wtable = (IGF.CurFn->arg_end() - 1);

  // Find the witness we're interested in.
  return emitWitnessMethodValue(IGF, wtable, declRef);
}

llvm::Function *IRGenModule::emitDispatchThunk(SILDeclRef declRef) {
  auto *f = getAddrOfDispatchThunk(declRef, ForDefinition);

  IRGenFunction IGF(*this, f);

  // Look up the method.
  auto fn = lookupMethod(IGF, declRef);

  // Call the witness, forwarding all of the parameters.
  auto params = IGF.collectParameters();
  auto result = IGF.Builder.CreateCall(fn, params.claimAll());

  // Return the result, if we have one.
  if (result->getType()->isVoidTy())
    IGF.Builder.CreateRetVoid();
  else
    IGF.Builder.CreateRet(result);

  return f;
}
