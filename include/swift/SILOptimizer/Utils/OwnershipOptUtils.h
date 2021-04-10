//===--- OwnershipOptUtils.h ----------------------------------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
///
/// Ownership Utilities that rely on SILOptimizer functionality.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_OWNERSHIPOPTUTILS_H
#define SWIFT_SILOPTIMIZER_UTILS_OWNERSHIPOPTUTILS_H

#include "swift/Basic/Defer.h"
#include "swift/SIL/BasicBlockUtils.h"
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/Utils/InstOptUtils.h"

namespace swift {

/// Returns true if this value requires OSSA cleanups.
inline bool requiresOSSACleanup(SILValue v) {
  return v->getFunction()->hasOwnership()
    && v.getOwnershipKind() != OwnershipKind::None
    && v.getOwnershipKind() != OwnershipKind::Unowned;
}

// Defined in BasicBlockUtils.h
struct JointPostDominanceSetComputer;

/// Given a new phi that may use a guaranteed value, create nested borrow scopes
/// for its incoming operands and end_borrows that cover the phi's extended
/// borrow scope, which transitively includes any phis that use this phi.
///
/// Returns true if any changes were made.
///
/// Note: \p newPhi itself might not have Guaranteed ownership. A phi that
/// converts Guaranteed to None ownership still needs nested borrows.
///
/// Note: This may be called on partially invalid OSSA form, where multiple
/// newly created phis do not yet have a borrow scope.
bool createBorrowScopeForPhiOperands(SILPhiArgument *newPhi);

/// A struct that contains context shared in between different operation +
/// "ownership fixup" utilities. Please do not put actual methods on this, it is
/// meant to be composed with.
struct OwnershipFixupContext {
  Optional<InstModCallbacks> inlineCallbacks;
  InstModCallbacks &callbacks;
  DeadEndBlocks &deBlocks;

  SmallVector<Operand *, 8> transitiveBorrowedUses;
  SmallVector<PhiOperand, 8> recursiveReborrows;

  /// Extra state initialized by OwnershipRAUWFixupHelper::get() that we use
  /// when RAUWing addresses. This ensures we do not need to recompute this
  /// state when we perform the actual RAUW.
  struct AddressFixupContext {
    /// When determining if we need to perform an address pointer fixup, we
    /// compute all transitive address uses of oldValue. If we find that we do
    /// need this fixed up, then we will copy our interior pointer base value
    /// and use this to seed that new lifetime.
    SmallVector<Operand *, 8> allAddressUsesFromOldValue;

    /// This is the interior pointer operand that the new value we want to RAUW
    /// is transitively derived from and enables us to know the underlying
    /// borrowed base value that we need to lifetime extend.
    InteriorPointerOperand intPtrOp;

    void clear() {
      allAddressUsesFromOldValue.clear();
      intPtrOp = InteriorPointerOperand();
    }
  };
  AddressFixupContext extraAddressFixupInfo;

  OwnershipFixupContext(InstModCallbacks &callbacks, DeadEndBlocks &deBlocks)
      : callbacks(callbacks), deBlocks(deBlocks) {}

  void clear() {
    transitiveBorrowedUses.clear();
    recursiveReborrows.clear();
    extraAddressFixupInfo.allAddressUsesFromOldValue.clear();
    extraAddressFixupInfo.intPtrOp = InteriorPointerOperand();
  }

private:
  /// Helper method called to determine if we discovered we needed interior
  /// pointer fixups while simplifying.
  bool needsInteriorPointerFixups() const {
    return bool(extraAddressFixupInfo.intPtrOp);
  }
};

/// A utility composed ontop of OwnershipFixupContext that knows how to RAUW a
/// value or a single value instruction with a new value and then fixup
/// ownership invariants afterwards.
class OwnershipRAUWHelper {
  OwnershipFixupContext *ctx;
  SingleValueInstruction *oldValue;
  SILValue newValue;

public:
  OwnershipRAUWHelper() : ctx(nullptr), oldValue(nullptr), newValue(nullptr) {}

  /// Return an instance of this class if we can perform the specific RAUW
  /// operation ignoring if the types line up. Returns None otherwise.
  ///
  /// DISCUSSION: We do not check that the types line up here so that we can
  /// allow for our users to transform our new value in ways that preserve
  /// ownership at \p oldValue before we perform the actual RAUW. If \p newValue
  /// is an object, any instructions in the chain of transforming instructions
  /// from \p newValue at \p oldValue's must be forwarding. If \p newValue is an
  /// address, then these transforms can only transform the address into a
  /// derived address.
  OwnershipRAUWHelper(OwnershipFixupContext &ctx,
                      SingleValueInstruction *oldValue, SILValue newValue);

  /// Returns true if this helper was initialized into a valid state.
  operator bool() const { return isValid(); }
  bool isValid() const { return bool(ctx) && bool(oldValue) && bool(newValue); }

  /// Perform the actual RAUW. We require that \p newValue and \p oldValue have
  /// the same type at this point (in contrast to when calling
  /// OwnershipRAUWFixupHelper::get()).
  ///
  /// This is so that we can avoid creating "forwarding" transformation
  /// instructions before we know if we can perform the RAUW. Any such
  /// "forwarding" transformation must be performed upon \p newValue at \p
  /// oldValue's insertion point so that we can then here RAUW the transformed
  /// \p newValue.
  SILBasicBlock::iterator
  perform(SingleValueInstruction *maybeTransformedNewValue = nullptr);

private:
  SILBasicBlock::iterator replaceAddressUses(SingleValueInstruction *oldValue,
                                             SILValue newValue);
};

/// A utility composed ontop of OwnershipFixupContext that knows how to replace
/// a single use of a value with another value with a different ownership. We
/// allow for the values to have different types.
///
/// NOTE: When not in OSSA, this just performs a normal set use, so this code is
/// safe to use with all code.
class OwnershipReplaceSingleUseHelper {
  OwnershipFixupContext *ctx;
  Operand *use;
  SILValue newValue;

public:
  OwnershipReplaceSingleUseHelper()
      : ctx(nullptr), use(nullptr), newValue(nullptr) {}

  /// Return an instance of this class if we support replacing \p use->get()
  /// with \p newValue.
  ///
  /// NOTE: For now we only support objects, not addresses so addresses will
  /// always yield an invalid helper.
  OwnershipReplaceSingleUseHelper(OwnershipFixupContext &ctx, Operand *use,
                                  SILValue newValue);

  /// Returns true if this helper was initialized into a valid state.
  operator bool() const { return isValid(); }
  bool isValid() const { return bool(ctx) && bool(use) && bool(newValue); }

  /// Perform the actual RAUW.
  SILBasicBlock::iterator perform();
};

/// An abstraction over LoadInst/LoadBorrowInst so one can handle both types of
/// load using common code.
struct LoadOperation {
  llvm::PointerUnion<LoadInst *, LoadBorrowInst *> value;

  LoadOperation() : value() {}
  LoadOperation(SILInstruction *input) : value(nullptr) {
    if (auto *li = dyn_cast<LoadInst>(input)) {
      value = li;
      return;
    }

    if (auto *lbi = dyn_cast<LoadBorrowInst>(input)) {
      value = lbi;
      return;
    }
  }

  explicit operator bool() const { return !value.isNull(); }

  SingleValueInstruction *operator*() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>();
    return value.get<LoadBorrowInst *>();
  }

  const SingleValueInstruction *operator->() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>();
    return value.get<LoadBorrowInst *>();
  }

  SingleValueInstruction *operator->() {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>();
    return value.get<LoadBorrowInst *>();
  }

  SILValue getOperand() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>()->getOperand();
    return value.get<LoadBorrowInst *>()->getOperand();
  }

  /// Return the ownership qualifier of the underlying load if we have a load or
  /// None if we have a load_borrow.
  ///
  /// TODO: Rather than use an optional here, we should include an invalid
  /// representation in LoadOwnershipQualifier.
  Optional<LoadOwnershipQualifier> getOwnershipQualifier() const {
    if (auto *lbi = value.dyn_cast<LoadBorrowInst *>()) {
      return None;
    }

    return value.get<LoadInst *>()->getOwnershipQualifier();
  }
};

} // namespace swift

#endif
