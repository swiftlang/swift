//===--- GenDistributed.h - IRGen for Distributed features ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines interfaces for emitting code for various distributed
// features.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENDISTRIBUTED_H
#define SWIFT_IRGEN_GENDISTRIBUTED_H

#include "swift/AST/Types.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/ApplySite.h"
#include "llvm/IR/CallingConv.h"

#include "Callee.h"
#include "GenHeap.h"
#include "IRGenModule.h"

namespace llvm {
class Value;
}

namespace swift {
class CanType;
class ProtocolConformanceRef;
class SILType;

namespace irgen {
class Explosion;
class IRGenFunction;

/// Emit the buildDistributedActorIdentity builtin.
void emitBuildDistributedActorIdentity(IRGenFunction &IGF, llvm::Value *actor,
                                       Explosion &out);

/// Emit the buildDistributedActorTransport builtin.
void emitBuildDistributedActorTransport(IRGenFunction &IGF, llvm::Value *actor,
                                        Explosion &out);

} // end namespace irgen
} // end namespace swift

#endif
