//===--- Condition.h - Condition Expression Emission ------------*- C++ -*-===//
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
// A helper structure for emitting condition expressions.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_CONDITION_H
#define SWIFT_IRGEN_CONDITION_H

#include "IRGen.h"

namespace llvm {
  class BasicBlock;
}

namespace swift {
namespace irgen {
  class IRGenFunction;

/// A condition is the result of evaluating a boolean expression as
/// control flow.
class Condition {
  // The blocks responsible for executing the true and false
  // conditions.  A block is non-null if that branch is possible, but
  // it's only an independent block if both branches are possible.
  llvm::BasicBlock *TrueBB;
  llvm::BasicBlock *FalseBB;

  /// The continuation block if both branches are possible.
  llvm::BasicBlock *ContBB;

public:
  Condition(llvm::BasicBlock *TrueBB, llvm::BasicBlock *FalseBB,
            llvm::BasicBlock *ContBB)
    : TrueBB(TrueBB), FalseBB(FalseBB), ContBB(ContBB) {}

  bool hasTrue() const { return TrueBB; }
  bool hasFalse() const { return FalseBB; }

  /// Begin the emission of the true block.  This should only be
  /// called if hasTrue() returns true.
  void enterTrue(IRGenFunction &IGF);

  /// End the emission of the true block.  This must be called after
  /// enterTrue but before anything else on this Condition.
  void exitTrue(IRGenFunction &IGF);

  /// Begin the emission of the false block.  This should only be
  /// called if hasFalse() returns true.
  void enterFalse(IRGenFunction &IGF);

  /// End the emission of the true block.  This must be called after
  /// exitFalse but before anything else on this Condition.
  void exitFalse(IRGenFunction &IGF);

  /// Complete this conditional execution.  This should be called
  /// only after all other calls on this Condition have been made.
  void complete(IRGenFunction &IGF);
};

} // end namespace irgen
} // end namespace swift

#endif
