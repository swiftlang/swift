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

#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "llvm/Support/Debug.h"

namespace swift {

/// Abstract base class. Implements all canonicalization transforms. Extended by
/// passes to be notified of each SIL modification.
struct CanonicalizeInstruction {
  // May be overriden by passes.
  static constexpr const char *defaultDebugType = "sil-canonicalize";
  const char *debugType = defaultDebugType;

  CanonicalizeInstruction(const char *passDebugType) {
#ifndef NDEBUG
    if (llvm::DebugFlag && !llvm::isCurrentDebugType(debugType))
      debugType = passDebugType;
#endif
  }

  virtual ~CanonicalizeInstruction();

  /// Rewrite this instruction, based on its operands and uses, into a more
  /// canonical representation.
  ///
  /// Return an iterator to the next instruction or to the end of the block.
  /// The returned iterator will follow any newly added or to-be-deleted
  /// instructions, regardless of whether the pass immediately deletes the
  /// instructions or simply records them for later deletion.
  ///
  /// To (re)visit new instructions, override notifyNewInstruction().
  ///
  /// To determine if any transformation at all occurred, override
  /// notifyNewInstruction(), killInstruction(), and notifyNewUsers().
  ///
  /// Warning: If the \p inst argument is killed and the client immediately
  /// erases \p inst, then it may be an invalid pointer upon return.
  SILBasicBlock::iterator canonicalize(SILInstruction *inst);

  /// Record a newly generated instruction.
  virtual void notifyNewInstruction(SILInstruction *inst) = 0;

  /// Kill an instruction that no longer has uses, or whose side effect is now
  /// represented by a different instruction. The client can defer erasing the
  /// instruction but must eventually erase all killed instructions to restore
  /// valid SIL.
  ///
  /// This callback should not mutate any other instructions. It may only delete
  /// the given argument. It will be called separately for each end-of-scope and
  /// debug use before being called on the instruction they use.
  virtual void killInstruction(SILInstruction *inst) = 0;

  /// Record a SIL value that has acquired new users.
  virtual void notifyHasNewUsers(SILValue value) = 0;
};

} // end namespace swift

#endif // SWIFT_SILOPTIMIZER_UTILS_CANONICALIZEINSTRUCTION_H
