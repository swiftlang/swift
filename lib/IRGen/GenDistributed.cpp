//===--- GenDistributed.cpp - IRGen for distributed features --------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2020 - 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
//  This file implements IR generation for distributed features.
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

llvm::Value *irgen::emitDistributedActorInitializeRemote(
    IRGenFunction &IGF, llvm::Value *actorMetatype, Explosion &out) {
  auto fn = IGF.IGM.getDistributedActorInitializeRemoteFn();
  actorMetatype =
      IGF.Builder.CreateBitCast(actorMetatype, IGF.IGM.TypeMetadataPtrTy);

  auto call = IGF.Builder.CreateCall(fn, {actorMetatype});
  call->setCallingConv(IGF.IGM.SwiftCC);
  call->setDoesNotThrow();

  out.add(call);

  return call;
}

void irgen::emitDistributedActorDestroy(IRGenFunction &IGF,
                                                 llvm::Value *actor) {
  auto fn = IGF.IGM.getDistributedActorDestroyFn();
  actor = IGF.Builder.CreateBitCast(actor, IGF.IGM.RefCountedPtrTy);

  auto call = IGF.Builder.CreateCall(fn, {actor});
  call->setCallingConv(IGF.IGM.SwiftCC);
  call->setDoesNotThrow();
}