//===--- GenDistributed.cpp - IRGen for Distributed features --------------===//
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
//  This file implements IR generation for distributed actor features.
//
//===----------------------------------------------------------------------===//

#include "GenDistributed.h"

#include "BitPatternBuilder.h"
#include "ExtraInhabitants.h"
#include "GenProto.h"
#include "GenType.h"
#include "IRGenDebugInfo.h"
#include "IRGenFunction.h"
#include "IRGenModule.h"
#include "LoadableTypeInfo.h"
#include "ScalarPairTypeInfo.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/ABI/MetadataValues.h"

using namespace swift;
using namespace irgen;

namespace {

} // end anonymous namespace

void irgen::emitBuildDistributedActorIdentity(IRGenFunction &IGF,
                                              llvm::Value *actor,
                                              Explosion &out) {
  // The implementation word of a default actor is just a null pointer.
  llvm::Value *identity =
    IGF.Builder.CreatePtrToInt(actor, IGF.IGM.ExecutorFirstTy);
  llvm::Value *impl = llvm::ConstantInt::get(IGF.IGM.ExecutorSecondTy, 0);

  out.add(identity);
  out.add(impl);
}

void irgen::emitBuildDistributedActorTransport(IRGenFunction &IGF,
                                               llvm::Value *actor,
                                               Explosion &out) {

}