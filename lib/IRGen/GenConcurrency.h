//===--- GenConcurrency.h - IRGen for concurrency features ------*- C++ -*-===//
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
// This file defines interfaces for emitting code for various concurrency
// features.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENCONCURRENCY_H
#define SWIFT_IRGEN_GENCONCURRENCY_H

namespace llvm {
class Value;
}

namespace swift {
class SILType;

namespace irgen {
class Explosion;
class IRGenFunction;

/// Emit the buildSerialExecutorRef builtin.
void emitBuildSerialExecutorRef(IRGenFunction &IGF, llvm::Value *actor,
                                SILType actorType, Explosion &out);

/// Emit the getCurrentExecutor builtin.
void emitGetCurrentExecutor(IRGenFunction &IGF, Explosion &out);

} // end namespace irgen
} // end namespace swift

#endif
