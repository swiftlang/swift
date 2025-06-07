//===--- ScopedAddressUtils.h ---------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SCOPEDADDRESSUTILS_H
#define SWIFT_SIL_SCOPEDADDRESSUTILS_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/PrunedLiveness.h"
#include "swift/SIL/SILBasicBlock.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "swift/SILOptimizer/Utils/InstModCallbacks.h"

namespace swift {

class ScopedAddressValueKind {
public:
  enum Kind : uint8_t {
    Invalid = 0,
    StoreBorrow,
    BeginAccess,
  };

private:
  Kind value;

public:
  static ScopedAddressValueKind get(SILValue value) {
    switch (value->getKind()) {
    default:
      return Kind::Invalid;
    case ValueKind::StoreBorrowInst:
      return Kind::StoreBorrow;
    case ValueKind::BeginAccessInst:
      return Kind::BeginAccess;
    }
  }

  ScopedAddressValueKind(Kind newValue) : value(newValue) {}

  operator Kind() const { return value; }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              ScopedAddressValueKind kind);

struct ScopedAddressValue {
  SILValue value;
  ScopedAddressValueKind kind = ScopedAddressValueKind::Invalid;

  ScopedAddressValue() = default;

  explicit ScopedAddressValue(SILValue value) {
    kind = ScopedAddressValueKind::get(value);
    if (kind)
      this->value = value;
  }

  operator bool() const {
    return kind != ScopedAddressValueKind::Invalid && value;
  }

  // Both the store_borrow source and address operands are effectively used for
  // the duration of the address scope.
  static ScopedAddressValue forUse(Operand *use) {
    if (auto svi = dyn_cast<SingleValueInstruction>(use->getUser()))
      return ScopedAddressValue(svi);

    return ScopedAddressValue();
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  // Helpers to allow a ScopedAddressValue to easily be used as a SILValue
  // programatically.
  SILValue operator->() { return value; }
  SILValue operator->() const { return value; }
  SILValue operator*() { return value; }
  SILValue operator*() const { return value; }

  /// Returns true if \p op is a scope ending use of the scoped address value.
  bool isScopeEndingUse(Operand *op) const;
  /// Pass all scope ending instructions to the visitor.
  bool visitScopeEndingUses(function_ref<bool(Operand *)> visitor) const;

  /// Optimistically computes liveness for all transitive uses, and adds this
  /// scope's live blocks into the SSA PrunedLiveness result. Returns
  /// AddressUseKind indicated whether a PointerEscape or Unknown use was
  /// encountered.
  ///
  /// This transitively finds uses within nested borrow scopes to handle
  /// incomplete nested lifetimes. Here, liveness will consider the apply to be
  /// a live use of the store_borrow:
  ///   %a = store_borrow
  ///   %v = load_borrow
  ///   apply (%v)
  ///   unreachable
  ///
  /// FIXME: with complete OSSA lifetimes, store borrow liveness is simply
  /// computed by visiting the end_borrow users.
  AddressUseKind computeTransitiveLiveness(SSAPrunedLiveness &liveness) const;

  /// Update \p liveness for all the transitive address uses.
  ///
  /// Valid for any type of liveness, SSA or MultiDef, that may be used by a
  /// scoped address.
  AddressUseKind updateTransitiveLiveness(SSAPrunedLiveness &liveness) const;

  /// Create appropriate scope ending instruction at \p insertPt.
  SILInstruction *createScopeEnd(SILBasicBlock::iterator insertPt,
                                 SILLocation loc) const;

  /// Create scope ending instructions at \p liveness boundary.
  void endScopeAtLivenessBoundary(SSAPrunedLiveness *liveness) const;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const ScopedAddressValue &value);

/// Returns true if there are other store_borrows enclosed within a store_borrow
/// \p sbi's scope
bool hasOtherStoreBorrowsInLifetime(StoreBorrowInst *sbi,
                                    SSAPrunedLiveness *liveness,
                                    DeadEndBlocks *deadEndBlocks);

} // namespace swift

#endif
