//===--- OwnershipUtils.h ------------------------------------*- C++ -*----===//
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

#ifndef SWIFT_SIL_OWNERSHIPUTILS_H
#define SWIFT_SIL_OWNERSHIPUTILS_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {

class SILBasicBlock;
class SILInstruction;
class SILModule;
class SILValue;
class DeadEndBlocks;

/// Returns true if v is an address or trivial.
bool isValueAddressOrTrivial(SILValue v);

/// These operations forward both owned and guaranteed ownership.
bool isOwnershipForwardingValueKind(SILNodeKind kind);

/// Is this an instruction that can forward both owned and guaranteed ownership
/// kinds.
bool isOwnershipForwardingInst(SILInstruction *i);

/// Is this an instruction that can forward guaranteed ownership.
bool isGuaranteedForwardingInst(SILInstruction *i);

/// These operations forward guaranteed ownership, but don't necessarily forward
/// owned values.
bool isGuaranteedForwardingValueKind(SILNodeKind kind);

/// Is this a value that is the result of an operation that forwards owned
/// ownership.
bool isGuaranteedForwardingValue(SILValue value);

/// Is this a node kind that can forward owned ownership, but may not be able to
/// forward guaranteed ownership.
bool isOwnedForwardingValueKind(SILNodeKind kind);

/// Does this SILInstruction 'forward' owned ownership, but may not be able to
/// forward guaranteed ownership.
bool isOwnedForwardingInstruction(SILInstruction *inst);

/// Does this value 'forward' owned ownership, but may not be able to forward
/// guaranteed ownership.
///
/// This will be either a multiple value instruction resuilt, a single value
/// instruction that forwards or an argument that forwards the ownership from a
/// previous terminator.
bool isOwnedForwardingValue(SILValue value);

struct BorrowScopeOperandKind {
  enum Kind {
    BeginBorrow,
    BeginApply,
    Branch,
  };

  Kind value;

  BorrowScopeOperandKind(Kind newValue) : value(newValue) {}
  BorrowScopeOperandKind(const BorrowScopeOperandKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  static Optional<BorrowScopeOperandKind> get(SILInstructionKind kind) {
    switch (kind) {
    default:
      return None;
    case SILInstructionKind::BeginBorrowInst:
      return BorrowScopeOperandKind(BeginBorrow);
    case SILInstructionKind::BeginApplyInst:
      return BorrowScopeOperandKind(BeginApply);
    case SILInstructionKind::BranchInst:
      return BorrowScopeOperandKind(Branch);
    }
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              BorrowScopeOperandKind kind);

struct BorrowScopeIntroducingValue;

/// An operand whose user instruction introduces a new borrow scope for the
/// operand's value. The value of the operand must be considered as implicitly
/// borrowed until the user's corresponding end scope instruction.
///
/// NOTE: We do not require that the guaranteed scope be represented by a
/// guaranteed value in the same function: see begin_apply. In such cases, we
/// require instead an end_* instruction to mark the end of the scope's region.
struct BorrowScopeOperand {
  BorrowScopeOperandKind kind;
  Operand *op;

  BorrowScopeOperand(Operand *op)
      : kind(*BorrowScopeOperandKind::get(op->getUser()->getKind())), op(op) {}
  BorrowScopeOperand(const BorrowScopeOperand &other)
      : kind(other.kind), op(other.op) {}
  BorrowScopeOperand &operator=(const BorrowScopeOperand &other) {
    kind = other.kind;
    op = other.op;
    return *this;
  }

  /// If value is a borrow introducer return it after doing some checks.
  static Optional<BorrowScopeOperand> get(Operand *op) {
    auto *user = op->getUser();
    auto kind = BorrowScopeOperandKind::get(user->getKind());
    if (!kind)
      return None;
    return BorrowScopeOperand(*kind, op);
  }

  void visitEndScopeInstructions(function_ref<void(Operand *)> func) const;

  /// Returns true if this borrow scope operand consumes guaranteed
  /// values and produces a new scope afterwards.
  bool consumesGuaranteedValues() const {
    switch (kind) {
    case BorrowScopeOperandKind::BeginBorrow:
    case BorrowScopeOperandKind::BeginApply:
      return false;
    case BorrowScopeOperandKind::Branch:
      return true;
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  /// Is this a borrow scope operand that can open new borrow scopes
  /// for owned values.
  bool canAcceptOwnedValues() const {
    switch (kind) {
    case BorrowScopeOperandKind::BeginBorrow:
    case BorrowScopeOperandKind::BeginApply:
      return true;
    case BorrowScopeOperandKind::Branch:
      return false;
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  /// Is the result of this instruction also a borrow introducer?
  ///
  /// TODO: This needs a better name.
  bool areAnyUserResultsBorrowIntroducers() const {
    // TODO: Can we derive this by running a borrow introducer check ourselves?
    switch (kind) {
    case BorrowScopeOperandKind::BeginBorrow:
    case BorrowScopeOperandKind::Branch:
      return true;
    case BorrowScopeOperandKind::BeginApply:
      return false;
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

  /// Visit all of the results of the operand's user instruction that are
  /// consuming uses.
  void visitUserResultConsumingUses(function_ref<void(Operand *)> visitor);

  /// Visit all of the "results" of the user of this operand that are borrow
  /// scope introducers for the specific scope that this borrow scope operand
  /// summarizes.
  void visitBorrowIntroducingUserResults(
      function_ref<void(BorrowScopeIntroducingValue)> visitor);

  /// Passes to visitor all of the consuming uses of this use's using
  /// instruction.
  ///
  /// This enables one to walk the def-use chain of guaranteed phis for a single
  /// guaranteed scope by using a worklist and checking if any of the operands
  /// are BorrowScopeOperands.
  void visitConsumingUsesOfBorrowIntroducingUserResults(
      function_ref<void(Operand *)> visitor);

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

private:
  /// Internal constructor for failable static constructor. Please do not expand
  /// its usage since it assumes the code passed in is well formed.
  BorrowScopeOperand(BorrowScopeOperandKind kind, Operand *op)
      : kind(kind), op(op) {}
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const BorrowScopeOperand &operand);

struct BorrowScopeIntroducingValueKind {
  /// Enum we use for exhaustive pattern matching over borrow scope introducers.
  enum Kind {
    LoadBorrow,
    BeginBorrow,
    SILFunctionArgument,
    Phi,
  };

  static Optional<BorrowScopeIntroducingValueKind> get(SILValue value) {
    if (value.getOwnershipKind() != ValueOwnershipKind::Guaranteed)
      return None;
    switch (value->getKind()) {
    default:
      return None;
    case ValueKind::LoadBorrowInst:
      return BorrowScopeIntroducingValueKind(LoadBorrow);
    case ValueKind::BeginBorrowInst:
      return BorrowScopeIntroducingValueKind(BeginBorrow);
    case ValueKind::SILFunctionArgument:
      return BorrowScopeIntroducingValueKind(SILFunctionArgument);
    case ValueKind::SILPhiArgument: {
      if (llvm::any_of(value->getParentBlock()->getPredecessorBlocks(),
                       [](SILBasicBlock *block) {
                         return !isa<BranchInst>(block->getTerminator());
                       })) {
        return None;
      }
      return BorrowScopeIntroducingValueKind(Phi);
    }
    }
  }

  Kind value;

  BorrowScopeIntroducingValueKind(Kind newValue) : value(newValue) {}
  BorrowScopeIntroducingValueKind(const BorrowScopeIntroducingValueKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  /// Is this a borrow scope that begins and ends within the same function and
  /// thus is guaranteed to have an "end_scope" instruction.
  ///
  /// In contrast, borrow scopes that are non-local (e.x. from
  /// SILFunctionArguments) rely a construct like a SILFunction as the begin/end
  /// of the scope.
  bool isLocalScope() const {
    switch (value) {
    case BorrowScopeIntroducingValueKind::BeginBorrow:
    case BorrowScopeIntroducingValueKind::LoadBorrow:
    case BorrowScopeIntroducingValueKind::Phi:
      return true;
    case BorrowScopeIntroducingValueKind::SILFunctionArgument:
      return false;
    }
    llvm_unreachable("Covered switch isnt covered?!");
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              BorrowScopeIntroducingValueKind kind);

struct InteriorPointerOperand;

/// A higher level construct for working with values that represent the
/// introduction of a new borrow scope.
///
/// DISCUSSION: A "borrow introducer" is a SILValue that represents the
/// beginning of a borrow scope that the ownership verifier validates. The idea
/// is this API allows one to work in a generic way with all of the various
/// introducers.
///
/// Some examples of borrow introducers: guaranteed SILFunctionArgument,
/// LoadBorrow, BeginBorrow, guaranteed BeginApply results.
///
/// NOTE: It is assumed that if a borrow introducer is a value of a
/// SILInstruction with multiple results, that the all of the SILInstruction's
/// guaranteed results are borrow introducers. In practice this means that
/// borrow introducers can not have guaranteed results that are not creating a
/// new borrow scope. No such instructions exist today.
struct BorrowScopeIntroducingValue {
  BorrowScopeIntroducingValueKind kind;
  SILValue value;

  /// If value is a borrow introducer return it after doing some checks.
  ///
  /// This is the only way to construct a BorrowScopeIntroducingValue. We make
  /// the primary constructor private for this reason.
  static Optional<BorrowScopeIntroducingValue> get(SILValue value) {
    auto kind = BorrowScopeIntroducingValueKind::get(value);
    if (!kind)
      return None;
    return BorrowScopeIntroducingValue(*kind, value);
  }

  /// If this value is introducing a local scope, gather all local end scope
  /// instructions and append them to \p scopeEndingInsts. Asserts if this is
  /// called with a scope that is not local.
  ///
  /// NOTE: To determine if a scope is a local scope, call
  /// BorrowScopeIntoducingValue::isLocalScope().
  void getLocalScopeEndingInstructions(
      SmallVectorImpl<SILInstruction *> &scopeEndingInsts) const;

  /// If this value is introducing a local scope, gather all local end scope
  /// instructions and pass them individually to visitor. Asserts if this is
  /// called with a scope that is not local.
  ///
  /// The intention is that this method can be used instead of
  /// BorrowScopeIntroducingValue::getLocalScopeEndingUses() to avoid
  /// introducing an intermediate array when one needs to transform the
  /// instructions before storing them.
  ///
  /// NOTE: To determine if a scope is a local scope, call
  /// BorrowScopeIntoducingValue::isLocalScope().
  void visitLocalScopeEndingUses(function_ref<void(Operand *)> visitor) const;

  bool isLocalScope() const { return kind.isLocalScope(); }

  /// Returns true if the passed in set of uses is completely within
  /// the lifetime of this borrow introducer.
  ///
  /// NOTE: Scratch space is used internally to this method to store the end
  /// borrow scopes if needed.
  bool areUsesWithinScope(ArrayRef<Operand *> instructions,
                          SmallVectorImpl<Operand *> &scratchSpace,
                          SmallPtrSetImpl<SILBasicBlock *> &visitedBlocks,
                          DeadEndBlocks &deadEndBlocks) const;

  /// Given a local borrow scope introducer, visit all non-forwarding consuming
  /// users. This means that this looks through guaranteed block arguments.
  bool visitLocalScopeTransitiveEndingUses(
      function_ref<void(Operand *)> visitor) const;

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }

  /// Visit each of the interior pointer uses of this underlying borrow
  /// introduced value. These object -> address projections and any transitive
  /// address uses must be treated as liveness requiring uses of the guaranteed
  /// value and we can not shrink the scope beyond that point. Returns true if
  /// we were able to understand all uses and thus guarantee we found all
  /// interior pointer uses. Returns false otherwise.
  bool visitInteriorPointerOperands(
      function_ref<void(const InteriorPointerOperand &)> func) const;

private:
  /// Internal constructor for failable static constructor. Please do not expand
  /// its usage since it assumes the code passed in is well formed.
  BorrowScopeIntroducingValue(BorrowScopeIntroducingValueKind kind,
                              SILValue value)
      : kind(kind), value(value) {}
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              const BorrowScopeIntroducingValue &value);

/// Look up the def-use graph starting at use \p inputOperand, recording any
/// "borrow" introducing values that we find into \p out. If at any point, we
/// find a point in the chain we do not understand, we bail and return false. If
/// we are able to understand all of the def-use graph, we know that we have
/// found all of the borrow introducing values, we return true.
///
/// NOTE: This may return multiple borrow introducing values in cases where
/// there are phi-like nodes in the IR like any true phi block arguments or
/// aggregate literal instructions (struct, tuple, enum, etc.).
bool getAllBorrowIntroducingValues(
    SILValue value, SmallVectorImpl<BorrowScopeIntroducingValue> &out);

/// Look up through the def-use chain of \p inputValue, looking for an initial
/// "borrow" introducing value. If at any point, we find two introducers or we
/// find a point in the chain we do not understand, we bail and return false. If
/// we are able to understand all of the def-use graph and only find a single
/// introducer, then we return a .some(BorrowScopeIntroducingValue).
Optional<BorrowScopeIntroducingValue>
getSingleBorrowIntroducingValue(SILValue inputValue);

struct InteriorPointerOperandKind {
  using UnderlyingKindTy = std::underlying_type<SILInstructionKind>::type;

  enum Kind : UnderlyingKindTy {
    RefElementAddr = UnderlyingKindTy(SILInstructionKind::RefElementAddrInst),
    RefTailAddr = UnderlyingKindTy(SILInstructionKind::RefTailAddrInst),
  };

  Kind value;

  InteriorPointerOperandKind(Kind newValue) : value(newValue) {}
  InteriorPointerOperandKind(const InteriorPointerOperandKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  static Optional<InteriorPointerOperandKind> get(Operand *use) {
    switch (use->getUser()->getKind()) {
    default:
      return None;
    case SILInstructionKind::RefElementAddrInst:
      return InteriorPointerOperandKind(RefElementAddr);
    case SILInstructionKind::RefTailAddrInst:
      return InteriorPointerOperandKind(RefTailAddr);
    }
  }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};

/// A mixed object->address projection that projects a memory location out of an
/// object with guaranteed ownership. All transitive address uses of the
/// interior pointer must be within the lifetime of the guaranteed lifetime. As
/// such, these must be treated as implicit uses of the parent guaranteed value.
struct InteriorPointerOperand {
  Operand *operand;
  InteriorPointerOperandKind kind;

  InteriorPointerOperand(Operand *op)
      : operand(op), kind(*InteriorPointerOperandKind::get(op)) {}

  /// If value is a borrow introducer return it after doing some checks.
  static Optional<InteriorPointerOperand> get(Operand *op) {
    auto kind = InteriorPointerOperandKind::get(op);
    if (!kind)
      return None;
    return InteriorPointerOperand(op, *kind);
  }

  /// Return the end scope of all borrow introducers of the parent value of this
  /// projection. Returns true if we were able to find all borrow introducing
  /// values.
  bool visitBaseValueScopeEndingUses(function_ref<void(Operand *)> func) const {
    SmallVector<BorrowScopeIntroducingValue, 4> introducers;
    if (!getAllBorrowIntroducingValues(operand->get(), introducers))
      return false;
    for (const auto &introducer : introducers) {
      if (!introducer.isLocalScope())
        continue;
      introducer.visitLocalScopeEndingUses(func);
    }
    return true;
  }

  SILValue getProjectedAddress() const {
    switch (kind) {
    case InteriorPointerOperandKind::RefElementAddr:
      return cast<RefElementAddrInst>(operand->getUser());
    case InteriorPointerOperandKind::RefTailAddr:
      return cast<RefTailAddrInst>(operand->getUser());
    }
    llvm_unreachable("Covered switch isn't covered?!");
  }

private:
  /// Internal constructor for failable static constructor. Please do not expand
  /// its usage since it assumes the code passed in is well formed.
  InteriorPointerOperand(Operand *op, InteriorPointerOperandKind kind)
      : operand(op), kind(kind) {}
};

struct OwnedValueIntroducerKind {
  enum Kind {
    /// An owned value that is a result of an Apply.
    Apply,

    /// An owned value returned as a result of applying a begin_apply.
    BeginApply,

    /// An owned value that is an argument that is in one of the successor
    /// blocks of a try_apply. This represents in a sense the try applies
    /// result.
    TryApply,

    /// An owned value produced as a result of performing a copy_value on some
    /// other value.
    Copy,

    /// An owned value produced as a result of performing a load [copy] on a
    /// memory location.
    LoadCopy,

    /// An owned value produced as a result of performing a load [take] from a
    /// memory location.
    LoadTake,

    /// An owned value that is a result of a true phi argument.
    ///
    /// A true phi argument here is defined as an SIL phi argument that only has
    /// branch predecessors.
    Phi,

    /// An owned value that is a function argument.
    FunctionArgument,

    /// An owned value that is a new partial_apply that has been formed.
    PartialApplyInit,

    /// An owned value from the formation of a new alloc_box.
    AllocBoxInit,

    /// An owned value from the formataion of a new alloc_ref.
    AllocRefInit,
  };

  static Optional<OwnedValueIntroducerKind> get(SILValue value) {
    if (value.getOwnershipKind() != ValueOwnershipKind::Owned)
      return None;

    switch (value->getKind()) {
    default:
      return None;
    case ValueKind::ApplyInst:
      return OwnedValueIntroducerKind(Apply);
    case ValueKind::BeginApplyResult:
      return OwnedValueIntroducerKind(BeginApply);
    case ValueKind::SILPhiArgument: {
      auto *phiArg = cast<SILPhiArgument>(value);
      if (dyn_cast_or_null<TryApplyInst>(phiArg->getSingleTerminator())) {
        return OwnedValueIntroducerKind(TryApply);
      }
      if (llvm::all_of(phiArg->getParent()->getPredecessorBlocks(),
                       [](SILBasicBlock *block) {
                         return isa<BranchInst>(block->getTerminator());
                       })) {
        return OwnedValueIntroducerKind(Phi);
      }
      return None;
    }
    case ValueKind::SILFunctionArgument:
      return OwnedValueIntroducerKind(FunctionArgument);
    case ValueKind::CopyValueInst:
      return OwnedValueIntroducerKind(Copy);
    case ValueKind::LoadInst: {
      auto qual = cast<LoadInst>(value)->getOwnershipQualifier();
      if (qual == LoadOwnershipQualifier::Take)
        return OwnedValueIntroducerKind(LoadTake);
      if (qual == LoadOwnershipQualifier::Copy)
        return OwnedValueIntroducerKind(LoadCopy);
      return None;
    }
    case ValueKind::PartialApplyInst:
      return OwnedValueIntroducerKind(PartialApplyInit);
    case ValueKind::AllocBoxInst:
      return OwnedValueIntroducerKind(AllocBoxInit);
    case ValueKind::AllocRefInst:
      return OwnedValueIntroducerKind(AllocRefInit);
    }
    llvm_unreachable("Default should have caught this");
  }

  Kind value;

  OwnedValueIntroducerKind(Kind newValue) : value(newValue) {}
  OwnedValueIntroducerKind(const OwnedValueIntroducerKind &other)
      : value(other.value) {}
  operator Kind() const { return value; }

  void print(llvm::raw_ostream &os) const;
  SWIFT_DEBUG_DUMP { print(llvm::dbgs()); }
};

llvm::raw_ostream &operator<<(llvm::raw_ostream &os,
                              OwnedValueIntroducerKind kind);

/// A higher level construct for working with values that introduce a new
/// "owned" value.
///
/// An owned "introducer" is a value that signals in a SIL program the begin of
/// a new semantic @owned ownership construct that is live without respect to
/// any other values in the function. This introducer value is then either used
/// directly, forwarded then used, and then finally destroyed.
///
/// NOTE: Previous incarnations of this concept used terms like "RC-identity".
struct OwnedValueIntroducer {
  /// The actual underlying value that introduces the new owned value.
  SILValue value;

  /// The kind of "introducer" that we use to classify any of various possible
  /// underlying introducing values.
  OwnedValueIntroducerKind kind;

  /// If a value is an owned value introducer we can recognize, return
  /// .some(OwnedValueIntroducer). Otherwise, return None.
  static Optional<OwnedValueIntroducer> get(SILValue value) {
    auto kind = OwnedValueIntroducerKind::get(value);
    if (!kind)
      return None;
    return OwnedValueIntroducer(value, *kind);
  }

  bool operator==(const OwnedValueIntroducer &other) const {
    return value == other.value;
  }

  bool operator!=(const OwnedValueIntroducer &other) const {
    return !(*this == other);
  }

  bool operator<(const OwnedValueIntroducer &other) const {
    return value < other.value;
  }

private:
  OwnedValueIntroducer(SILValue value, OwnedValueIntroducerKind kind)
      : value(value), kind(kind) {}
};

/// Look up the def-use graph starting at use \p inputOperand, recording any
/// values that act as "owned" introducers.
///
/// NOTE: This may return multiple owned introducers in cases where there are
/// phi-like nodes in the IR like any true phi block arguments or aggregate
/// literal instructions (struct, tuple, enum, etc.).
bool getAllOwnedValueIntroducers(SILValue value,
                                 SmallVectorImpl<OwnedValueIntroducer> &out);

Optional<OwnedValueIntroducer> getSingleOwnedValueIntroducer(SILValue value);

} // namespace swift

#endif
