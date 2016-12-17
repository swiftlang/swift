//===--- SILValue.cpp - Implementation for SILValue -----------------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "swift/SIL/SILValue.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILVisitor.h"

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

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind Kind) {
  switch (Kind) {
  case ValueOwnershipKind::Trivial:
    return os << "Trivial";
  case ValueOwnershipKind::Unowned:
    return os << "Unowned";
  case ValueOwnershipKind::Owned:
    return os << "Owned";
  case ValueOwnershipKind::Guaranteed:
    return os << "Guaranteed";
  case ValueOwnershipKind::Undef:
    return os << "Undef";
  }
}

Optional<ValueOwnershipKind>
swift::ValueOwnershipKindMerge(Optional<ValueOwnershipKind> LHS,
                               Optional<ValueOwnershipKind> RHS) {
  if (!LHS.hasValue() || !RHS.hasValue())
    return NoneType::None;
  auto LHSVal = LHS.getValue();
  auto RHSVal = RHS.getValue();

  // Undef merges with anything.
  if (LHSVal == ValueOwnershipKind::Undef) {
    return RHSVal;
  }
  // Undef merges with anything.
  if (RHSVal == ValueOwnershipKind::Undef) {
    return LHSVal;
  }

  return (LHSVal == RHSVal) ? LHS : None;
}

//===----------------------------------------------------------------------===//
//                 Instruction ValueOwnershipKind Computation
//===----------------------------------------------------------------------===//

namespace {

class ValueOwnershipKindVisitor
    : public SILVisitor<ValueOwnershipKindVisitor,
                        Optional<ValueOwnershipKind>> {

public:
  ValueOwnershipKindVisitor() = default;
  ~ValueOwnershipKindVisitor() = default;
  ValueOwnershipKindVisitor(const ValueOwnershipKindVisitor &) = delete;
  ValueOwnershipKindVisitor(ValueOwnershipKindVisitor &&) = delete;

  Optional<ValueOwnershipKind> visitForwardingInst(SILInstruction *I);
  Optional<ValueOwnershipKind> visitPHISILArgument(SILArgument *Arg);

  Optional<ValueOwnershipKind> visitValueBase(ValueBase *V) {
    llvm_unreachable("unimplemented method on ValueBaseOwnershipVisitor");
  }
#define VALUE(Id, Parent)                                                      \
  Optional<ValueOwnershipKind> visit##Id(Id *ID) {                             \
    llvm_unreachable("unimplemented");                                         \
  }
#include "swift/SIL/SILNodes.def"
};

} // end anonymous namespace

Optional<ValueOwnershipKind> SILValue::getOwnershipKind() const {
  // Once we have multiple return values, this must be changed.
  return ValueOwnershipKindVisitor().visit(const_cast<ValueBase *>(Value));
}
