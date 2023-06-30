//===--- InstructionUtils.h - Utilities for SIL instructions ----*- C++ -*-===//
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

#ifndef SWIFT_SIL_WRAPPERTYPES_H
#define SWIFT_SIL_WRAPPERTYPES_H

#include "swift/SIL/SILInstruction.h"

namespace swift {
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

  SingleValueInstruction *getLoadInst() const {
    if (value.is<LoadInst *>())
      return value.get<LoadInst *>();
    return value.get<LoadBorrowInst *>();
  }

  SingleValueInstruction *operator*() const { return getLoadInst(); }

  const SingleValueInstruction *operator->() const { return getLoadInst(); }

  SingleValueInstruction *operator->() { return getLoadInst(); }

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
  llvm::Optional<LoadOwnershipQualifier> getOwnershipQualifier() const {
    if (auto *lbi = value.dyn_cast<LoadBorrowInst *>()) {
      return llvm::None;
    }

    return value.get<LoadInst *>()->getOwnershipQualifier();
  }
};

/// A wrapper type for writing generic code against conversion instructions.
struct ConversionOperation {
  SingleValueInstruction *inst = nullptr;

  ConversionOperation() = default;

  explicit ConversionOperation(SILInstruction *inst) {
    auto *svi = dyn_cast<SingleValueInstruction>(inst);
    if (!svi) {
      return;
    }
    if (!ConversionOperation::isa(svi)) {
      return;
    }
    this->inst = svi;
  }

  explicit ConversionOperation(SILValue value) {
    auto *inst = value->getDefiningInstruction();
    if (!inst) {
      return;
    }
    auto *svi = dyn_cast<SingleValueInstruction>(inst);
    if (!svi) {
      return;
    }
    if (!ConversionOperation::isa(svi)) {
      return;
    }
    this->inst = svi;
  }

  operator bool() const { return inst != nullptr; }

  SingleValueInstruction *operator->() { return inst; }
  SingleValueInstruction *operator->() const { return inst; }
  SingleValueInstruction *operator*() { return inst; }
  SingleValueInstruction *operator*() const { return inst; }

  static bool isa(SILInstruction *inst) {
    switch (inst->getKind()) {
    case SILInstructionKind::ConvertFunctionInst:
    case SILInstructionKind::UpcastInst:
    case SILInstructionKind::AddressToPointerInst:
    case SILInstructionKind::UncheckedTrivialBitCastInst:
    case SILInstructionKind::UncheckedAddrCastInst:
    case SILInstructionKind::UncheckedBitwiseCastInst:
    case SILInstructionKind::RefToRawPointerInst:
    case SILInstructionKind::RawPointerToRefInst:
    case SILInstructionKind::ConvertEscapeToNoEscapeInst:
    case SILInstructionKind::RefToBridgeObjectInst:
    case SILInstructionKind::BridgeObjectToRefInst:
    case SILInstructionKind::BridgeObjectToWordInst:
    case SILInstructionKind::ThinToThickFunctionInst:
    case SILInstructionKind::ThickToObjCMetatypeInst:
    case SILInstructionKind::ObjCToThickMetatypeInst:
    case SILInstructionKind::ObjCMetatypeToObjectInst:
    case SILInstructionKind::ObjCExistentialMetatypeToObjectInst:
    case SILInstructionKind::UnconditionalCheckedCastInst:
    case SILInstructionKind::UncheckedRefCastInst:
    case SILInstructionKind::UncheckedValueCastInst:
    case SILInstructionKind::RefToUnmanagedInst:
    case SILInstructionKind::RefToUnownedInst:
    case SILInstructionKind::UnmanagedToRefInst:
    case SILInstructionKind::UnownedToRefInst:
      return true;
    default:
      return false;
    }
  }

  SILValue getConverted() { return inst->getOperand(0); }
};

/// A wrapper type for writing generic code against SelectEnumAddrInst and
/// SelectEnumInst.
///
/// We use this instead of SelectEnumInstBase in order to avoid the need for
/// templating SelectEnumInstBase.
class SelectEnumOperation {
  PointerUnion<SelectEnumAddrInst *, SelectEnumInst *> value;

public:
  SelectEnumOperation(SelectEnumAddrInst *seai) : value(seai) {}
  SelectEnumOperation(SelectEnumInst *seai) : value(seai) {}
  SelectEnumOperation(SILInstruction *i) : value(nullptr) {
    if (auto *seai = dyn_cast<SelectEnumAddrInst>(i)) {
      value = seai;
      return;
    }

    if (auto *sei = dyn_cast<SelectEnumInst>(i)) {
      value = sei;
      return;
    }
  }

  SelectEnumOperation(const SILInstruction *i)
      : SelectEnumOperation(const_cast<SILInstruction *>(i)) {}

  operator SingleValueInstruction *() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  SingleValueInstruction *operator*() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  SingleValueInstruction *operator->() const {
    if (auto *seai = value.dyn_cast<SelectEnumAddrInst *>())
      return seai;
    return value.get<SelectEnumInst *>();
  }

  operator bool() const { return bool(value); }

  SILValue getOperand() {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getOperand();
    return value.get<SelectEnumAddrInst *>()->getOperand();
  }

  SILValue getEnumOperand() { return getOperand(); }

  const Operand &getEnumOperandRef() {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getEnumOperandRef();
    return value.get<SelectEnumAddrInst *>()->getEnumOperandRef();
  }

  unsigned getNumCases() const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getNumCases();
    return value.get<SelectEnumAddrInst *>()->getNumCases();
  }

  std::pair<EnumElementDecl *, SILValue> getCase(unsigned i) const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getCase(i);
    return value.get<SelectEnumAddrInst *>()->getCase(i);
  }
  /// Return the value that will be used as the result for the specified enum
  /// case.
  SILValue getCaseResult(EnumElementDecl *D) {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getCaseResult(D);
    return value.get<SelectEnumAddrInst *>()->getCaseResult(D);
  }

  /// If the default refers to exactly one case decl, return it.
  NullablePtr<EnumElementDecl> getUniqueCaseForDefault();

  bool hasDefault() const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->hasDefault();
    return value.get<SelectEnumAddrInst *>()->hasDefault();
  }

  SILValue getDefaultResult() const {
    if (auto *sei = value.dyn_cast<SelectEnumInst *>())
      return sei->getDefaultResult();
    return value.get<SelectEnumAddrInst *>()->getDefaultResult();
  }
};

} // end namespace swift

#endif
