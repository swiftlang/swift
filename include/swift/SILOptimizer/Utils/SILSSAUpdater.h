//===--- SILSSAUpdater.h - Unstructured SSA Update Tool ---------*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILSSAUPDATER_H
#define SWIFT_SIL_SILSSAUPDATER_H

#include "llvm/Support/Allocator.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"

namespace llvm {
  template<typename T> class SSAUpdaterTraits;
  template<typename T> class SmallVectorImpl;
}

namespace swift {

class SILPHIArgument;
class SILBasicBlock;
class SILType;
class SILUndef;

/// Independent utility that canonicalizes BB arguments by reusing structurally
/// equivalent arguments and replacing the original arguments with casts.
SILInstruction *replaceBBArgWithCast(SILPHIArgument *Arg);

/// This class updates SSA for a set of SIL instructions defined in multiple
/// blocks.
class SILSSAUpdater {
  friend class llvm::SSAUpdaterTraits<SILSSAUpdater>;

  // A map of basic block to available phi value.
  // using AvailableValsTy = llvm::DenseMap<SILBasicBlock *, SILValue>;
  void *AV;

  SILType ValType;

  // The SSAUpdaterTraits specialization uses this sentinel to mark 'new' phi
  // nodes (all the incoming edge arguments have this sentinel set).
  std::unique_ptr<SILUndef, void(*)(SILUndef *)> PHISentinel;

  // If not null updated with inserted 'phi' nodes (SILArgument).
  SmallVectorImpl<SILPHIArgument *> *InsertedPHIs;

  // Not copyable.
  void operator=(const SILSSAUpdater &) = delete;
  SILSSAUpdater(const SILSSAUpdater &) = delete;

public:
  explicit SILSSAUpdater(
      SmallVectorImpl<SILPHIArgument *> *InsertedPHIs = nullptr);
  ~SILSSAUpdater();

  /// \brief Initialize for a use of a value of type.
  void Initialize(SILType T);

  bool HasValueForBlock(SILBasicBlock *BB) const;
  void AddAvailableValue(SILBasicBlock *BB, SILValue V);

  /// \brief Construct SSA for a value that is live at the *end* of a basic block.
  SILValue GetValueAtEndOfBlock(SILBasicBlock *BB);

  /// \brief Construct SSA for a value that is live in the middle of a block.
  /// This handles the case where the use is before a definition of the value.
  ///  BB1:
  ///    val_1 = def
  ///    br BB2:
  ///  BB2:
  ///         = use(val_?)
  ///    val_2 = def
  ///    cond_br ..., BB2, BB3
  ///
  /// In this case we need to insert a 'PHI' node at the beginning of BB2
  /// merging val_1 and val_2.
  SILValue GetValueInMiddleOfBlock(SILBasicBlock *BB);

  void RewriteUse(Operand &Op);

  void *allocate(unsigned Size, unsigned Align) const;
  static void deallocateSentinel(SILUndef *U);
private:

  SILValue GetValueAtEndOfBlockInternal(SILBasicBlock *BB);
};

/// \brief Utility to wrap 'Operand's to deal with invalidation of
/// ValueUseIterators during SSA construction.
///
/// Uses in branches change under us - we need to identify them by an
/// indirection. A ValueUseIterator is just an Operand pointer. As we update SSA
/// form we change branches and invalidate (by deleting the old branch and
/// creating a new one) the Operand pointed to by the ValueUseIterator.
///
/// This class wraps such uses (uses in branches) to provide a level of
/// indirection. We can restore the information - the use - by looking at the
/// new branch and the operand index.
///
/// Uses in branches are stored as an index and the parent block to
/// identify the use allowing us to reconstruct the use after the branch has
/// been changed.
class UseWrapper {
  Operand *U;
  SILBasicBlock *Parent;
  enum {
    kRegularUse,
    kBranchUse,
    kCondBranchUseTrue,
    kCondBranchUseFalse
  } Type;
  unsigned Idx;

public:

  /// \brief Construct a use wrapper. For branches we store information so that
  /// we can reconstruct the use after the branch has been modified.
  ///
  /// When a branch is modified existing pointers to the operand
  /// (ValueUseIterator) become invalid as they point to freed operands.
  /// Instead we store the branch's parent and the idx so that we can
  /// reconstruct the use.
  UseWrapper(Operand *Use);

  /// Return the operand we wrap. Reconstructing branch operands.
  operator Operand*();
};

} // end namespace swift
#endif
