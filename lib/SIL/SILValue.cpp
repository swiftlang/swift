//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
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

#include "swift/SIL/SILValue.h"
#include "ValueOwnershipKindClassifier.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       Check SILValue Type Properties
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.
static_assert(std::is_standard_layout<SILValue>::value,
              "Expected SILValue to be standard layout");
static_assert(sizeof(SILValue) == sizeof(uintptr_t),
              "SILValue should be pointer sized");

//===----------------------------------------------------------------------===//
//                              Utility Methods
//===----------------------------------------------------------------------===//

void ValueBase::replaceAllUsesWith(ValueBase *RHS) {
  assert(this != RHS && "Cannot RAUW a value with itself");
  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(RHS);
  }
}

SILBasicBlock *ValueBase::getParentBlock() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getParent();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getParent();
  return nullptr;
}

SILFunction *ValueBase::getFunction() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return Inst->getFunction();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return Arg->getFunction();
  return nullptr;
}

SILModule *ValueBase::getModule() const {
  auto *NonConstThis = const_cast<ValueBase *>(this);
  if (auto *Inst = dyn_cast<SILInstruction>(NonConstThis))
    return &Inst->getModule();
  if (auto *Arg = dyn_cast<SILArgument>(NonConstThis))
    return &Arg->getModule();
  return nullptr;
}

//===----------------------------------------------------------------------===//
//                             ValueOwnershipKind
//===----------------------------------------------------------------------===//

ValueOwnershipKind::ValueOwnershipKind(SILModule &M, SILType Type,
                                       SILArgumentConvention Convention)
    : Value() {
  switch (Convention) {
  case SILArgumentConvention::Indirect_In:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Trivial
      : ValueOwnershipKind::Owned;
    break;
  case SILArgumentConvention::Indirect_In_Guaranteed:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Trivial
      : ValueOwnershipKind::Guaranteed;
    break;
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    Value = ValueOwnershipKind::Trivial;
    return;
  case SILArgumentConvention::Direct_Owned:
    Value = ValueOwnershipKind::Owned;
    return;
  case SILArgumentConvention::Direct_Unowned:
    Value = Type.isTrivial(M) ? ValueOwnershipKind::Trivial
                              : ValueOwnershipKind::Unowned;
    return;
  case SILArgumentConvention::Direct_Guaranteed:
    Value = ValueOwnershipKind::Guaranteed;
    return;
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("Not handled");
  }
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind Kind) {
  switch (Kind) {
  case ValueOwnershipKind::Trivial:
    return os << "trivial";
  case ValueOwnershipKind::Unowned:
    return os << "unowned";
  case ValueOwnershipKind::Owned:
    return os << "owned";
  case ValueOwnershipKind::Guaranteed:
    return os << "guaranteed";
  case ValueOwnershipKind::Any:
    return os << "any";
  }

  llvm_unreachable("Unhandled ValueOwnershipKind in switch.");
}

Optional<ValueOwnershipKind>
ValueOwnershipKind::merge(ValueOwnershipKind RHS) const {
  auto LHSVal = Value;
  auto RHSVal = RHS.Value;

  // Any merges with anything.
  if (LHSVal == ValueOwnershipKind::Any) {
    return ValueOwnershipKind(RHSVal);
  }
  // Any merges with anything.
  if (RHSVal == ValueOwnershipKind::Any) {
    return ValueOwnershipKind(LHSVal);
  }

  return (LHSVal == RHSVal) ? Optional<ValueOwnershipKind>(*this) : None;
}

ValueOwnershipKind::ValueOwnershipKind(StringRef S) {
  auto Result = llvm::StringSwitch<Optional<ValueOwnershipKind::innerty>>(S)
                    .Case("trivial", ValueOwnershipKind::Trivial)
                    .Case("unowned", ValueOwnershipKind::Unowned)
                    .Case("owned", ValueOwnershipKind::Owned)
                    .Case("guaranteed", ValueOwnershipKind::Guaranteed)
                    .Case("any", ValueOwnershipKind::Any)
                    .Default(None);
  if (!Result.hasValue())
    llvm_unreachable("Invalid string representation of ValueOwnershipKind");
  Value = Result.getValue();
}

ValueOwnershipKind SILValue::getOwnershipKind() const {
  // Once we have multiple return values, this must be changed.
  sil::ValueOwnershipKindClassifier Classifier;
  return Classifier.visit(const_cast<ValueBase *>(Value));
}
