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

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  // Helpers to allow a ScopedAddressValue to easily be used as a SILValue
  // programatically.
  SILValue operator->() { return value; }
  SILValue operator->() const { return value; }
  SILValue operator*() { return value; }
  SILValue operator*() const { return value; }

  /// Returns true if \p op is a scope edning use of the scoped address value.
  bool isScopeEndingUse(Operand *op) const;
  /// Pass all scope ending instructions to the visitor.
  bool visitScopeEndingUses(function_ref<bool(Operand *)> visitor) const;
  /// Returns false, if liveness cannot be computed due to pointer escape or
  /// unkown address use. Add this scope's live blocks into the PrunedLiveness
  /// result.
  bool computeLiveness(PrunedLiveness &liveness) const;

  /// Create appropriate scope ending instruction at \p insertPt.
  void createScopeEnd(SILBasicBlock::iterator insertPt, SILLocation loc) const;

  /// Create scope ending instructions at \p liveness boundary.
  void endScopeAtLivenessBoundary(PrunedLiveness *liveness) const;
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const ScopedAddressValue &value);

/// Returns true if there are other store_borrows enclosed within a store_borrow
/// \p sbi's scope
bool hasOtherStoreBorrowsInLifetime(StoreBorrowInst *sbi,
                                    PrunedLiveness *liveness,
                                    DeadEndBlocks *deadEndBlocks);

/// Extend the store_borrow \p sbi's scope such that it encloses \p newUsers.
bool extendStoreBorrow(StoreBorrowInst *sbi,
                       SmallVectorImpl<Operand *> &newUses,
                       DeadEndBlocks *deadEndBlocks,
                       InstModCallbacks callbacks = InstModCallbacks());
} // namespace swift

#endif
