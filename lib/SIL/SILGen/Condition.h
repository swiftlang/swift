//===--- Condition.h - Defines the SILGen Condition class -------*- C++ -*-===//
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
// This file defines the Condition class, used by SIL Generation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_LOWERING_CONDITION_H
#define SWIFT_SIL_LOWERING_CONDITION_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Compiler.h"

namespace swift {
  class SILBuilder;
  class BasicBlock;
  class Value;
  
namespace Lowering {

/// A condition is the result of evaluating a boolean expression as
/// control flow.
class LLVM_LIBRARY_VISIBILITY Condition {
  // The blocks responsible for executing the true and false conditions.  A
  // block is non-null if that branch is possible, but it's only an independent
  // block if both branches are possible.
  BasicBlock *TrueBB;
  BasicBlock *FalseBB;
  
  /// ContBB - The continuation block if both branches are possible.
  BasicBlock *ContBB;
  
public:
  Condition(BasicBlock *TrueBB, BasicBlock *FalseBB, BasicBlock *ContBB)
    : TrueBB(TrueBB), FalseBB(FalseBB), ContBB(ContBB)
  {
  }
  
  bool hasTrue() const { return TrueBB; }
  bool hasFalse() const { return FalseBB; }
  
  /// enterTrue - Begin the emission of the true block.  This should only be
  /// called if hasTrue() returns true.
  void enterTrue(SILBuilder &B);
  
  /// exitTrue - End the emission of the true block.  This must be called after
  /// enterTrue but before anything else on this Condition.
  void exitTrue(SILBuilder &B, llvm::ArrayRef<Value> Args = {});
  
  /// enterFalse - Begin the emission of the false block.  This should only be
  /// called if hasFalse() returns true.
  void enterFalse(SILBuilder &B);
  
  /// exitFalse - End the emission of the true block.  This must be called after
  /// enterFalse but before anything else on this Condition.
  void exitFalse(SILBuilder &B, llvm::ArrayRef<Value> Args = {});
  
  /// complete - Complete this conditional execution.  This should be called
  /// only after all other calls on this Condition have been made.
  BasicBlock *complete(SILBuilder &B);
};

} // end namespace Lowering
} // end namespace swift
  
#endif
