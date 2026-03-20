//===--- StackAllocation.h - Utility for stack allocations ------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines classes for working with stack allocations and
// deallocations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_STACKALLOCATION_H
#define SWIFT_SIL_STACKALLOCATION_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

enum class StackAllocationKind : uint8_t {
  /// An AllocStackInst. The deallocation is a DeallocStackInst.
  AllocStack,

  /// An AllocPackInst. The deallocation is a DeallocPackInst.
  AllocPack,

  /// An AllocPackMetadataInst. The deallocation is a DeallocPackMetadataInst.
  AllocPackMetadata,

  /// An on-stack AllocRefInst. The deallocation is DeallocStackRefInst.
  AllocRef,

  /// An on-stack AllocRefDynamicInst. The deallocation is a
  /// DeallocStackRefInst.
  AllocRefDynamic,

  /// An on-stack PartialApplyInst outside of OSSA. The deallocation is a
  /// DeallocStackInst.
  PartialApply,

  /// A callee-allocated BeginApplyInst. The deallocation is a
  /// DeallocStackInst.
  CalleeAllocatedBeginApply,

  /// A BuiltinInst for the StackAlloc builtin. The deallocation is
  /// a BuiltinInst for the StackDealloc builtin.
  BuiltinStackAlloc,

  /// A BuiltinInst for the UnprotectedStackAlloc builtin. The deallocation is
  /// a BuiltinInst for the StackDealloc builtin.
  BuiltinUnprotectedStackAlloc,

  /// A BuiltinInst for the StartAsyncLetWithLocalBuffer builtin. The
  /// deallocation is a BuiltinInst for the FinishAsyncLet builtin.
  BuiltinStartAsyncLet,

  /// A BuiltinInst for the AddTaskLocalValue builtin. The deallocation
  /// is a BuiltinInst for the RemoveTaskLocalValue builtin.
  BuiltinAddTaskLocalValue,

  /// A BuiltinInst for the TaskAddPriorityEscalationHandler builtin. The
  /// deallocation is a BuiltinInst for the TaskRemovePriorityEscalationHandler
  /// builtin.
  BuiltinTaskAddPriorityEscalationHandler,

  /// A BuiltinInst for the TaskAddCancellationHandler builtin. The
  /// deallocation is a BuiltinInst for the TaskRemoveCancellationHandler
  /// builtin.
  BuiltinTaskAddCancellationHandler,
};

class StackAllocation {
  SILValue value;
  StackAllocationKind kind;

  StackAllocation(SILValue value, StackAllocationKind kind)
    : value(value), kind(kind) {}

public:
  // This should only be used by the core implementation.
  static StackAllocation getUnchecked(SILValue value,
                                      StackAllocationKind kind) {
    return {value, kind};
  }

  StackAllocationKind getKind() const {
    return kind;
  }

  /// The value which abstractly represents the stack allocation. Most of
  /// the allocation kinds are the results of single-value instructions, but
  /// for the values that are not, this is the value that is used by all the
  /// deallocations.
  SILValue getValue() const {
    return value;
  }

  SILInstruction *getInstruction() const {
    if (kind != StackAllocationKind::CalleeAllocatedBeginApply)
      return cast<SingleValueInstruction>(value);
    return cast<MultipleValueInstructionResult>(value)->getParent();
  }

  StackAllocationIsNested_t isNested() const {
    return getInstruction()->isStackAllocationNested();
  }
};

class StackDeallocation {
  SILValue alloc;
  SILInstruction *dealloc;
  StackAllocationKind kind;

  StackDeallocation(SILValue alloc, SILInstruction *dealloc,
                    StackAllocationKind kind)
    : alloc(alloc), dealloc(dealloc), kind(kind) {}

public:
  // This should only be used by the core implementation.
  static StackDeallocation getUnchecked(SILValue alloc,
                                        const SILInstruction *dealloc,
                                        StackAllocationKind kind) {
    return {alloc, const_cast<SILInstruction*>(dealloc), kind};
  }

  /// Return the kind of allocation that this deallocates.
  StackAllocationKind getKind() const {
    return kind;
  }

  /// Return the deallocating instruction.
  SILInstruction *getInstruction() const {
    return dealloc;
  }

  /// Return the StackAllocation for this deallocation.
  /// The kind is always the same.
  StackAllocation getAllocation() const {
    return StackAllocation::getUnchecked(alloc, kind);
  }

  /// Find the allocation for the given deallocation.
  static SILValue getAllocationOperand(const SILInstruction *dealloc) {
    // NOTE: If you're adding a new kind of deallocating instruction,
    // there are several places scattered around the SIL optimizer which
    // assume that the allocating instruction of a deallocating instruction
    // is referenced by operand 0. Keep that true if you can.
    auto op = dealloc->getOperand(0);
    while (auto *mvi = dyn_cast<MoveValueInst>(op)) {
      op = mvi->getOperand();
    }
    return op;
  }
};

}

#endif
