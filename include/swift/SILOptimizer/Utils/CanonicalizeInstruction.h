//===-- CanonicalizeInstruction.h - canonical SIL peepholes -----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// SSA-peephole transformations that yield a more canonical SIL representation.
///
/// Unlike simplifyInstruction, these transformations may effect any
/// instruction, not only single-values, and may arbitrarily generate new SIL
/// instructions.
///
/// Unlike SILCombine, these peepholes must work on 'raw' SIL form and should be
/// limited to those necessary to aid in diagnostics and other mandatory
/// pipelin/e passes. Optimization may only be done to the extent that it
/// neither interferes with diagnostics nor increases compile time.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_CANONICALIZEINSTRUCTION_H
#define SWIFT_SILOPTIMIZER_UTILS_CANONICALIZEINSTRUCTION_H

#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// Abstract base class. Implements all canonicalization transforms. Extended by
/// passes to be notified of each SIL modification.
struct CanonicalizeInstruction {
  // May be overriden by passes.
  static constexpr const char *defaultDebugType = "sil-canonicalize";
  const char *debugType = defaultDebugType;
  DeadEndBlocks &deadEndBlocks;
  InstructionDeleter &deleter;
  bool changed = false;

  CanonicalizeInstruction(const char *passDebugType,
                          DeadEndBlocks &deadEndBlocks,
                          InstructionDeleter &deleter)
      : deadEndBlocks(deadEndBlocks), deleter(deleter) {
#ifndef NDEBUG
    if (llvm::DebugFlag && !llvm::isCurrentDebugType(debugType))
      debugType = passDebugType;
#endif
  }

  const SILFunction *getFunction() const { return deadEndBlocks.getFunction(); }

  InstModCallbacks &getCallbacks() { return deleter.getCallbacks(); }

  void killInstruction(SILInstruction *inst) {
    deleter.getCallbacks().deleteInst(inst);
  }

  /// Rewrite this instruction, based on its operands and uses, into a more
  /// canonical representation.
  ///
  /// Returns true if canonicalization changed, added, or deleted instructions.
  ///
  /// Warning: \p inst may already be deleted upon return.
  bool canonicalize(SILInstruction *inst);

  /// Record a newly generated instruction.
  void notifyNewInstruction(SILInstruction *inst) {
    deleter.getCallbacks().createdNewInst(inst);
  }
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_CANONICALIZEINSTRUCTION_H
