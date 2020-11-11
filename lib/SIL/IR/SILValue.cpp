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
#include "swift/SIL/OwnershipUtils.h"
#include "swift/SIL/SILArgument.h"
#include "swift/SIL/SILBuiltinVisitor.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILModule.h"
#include "swift/SIL/SILVisitor.h"
#include "llvm/ADT/StringSwitch.h"

using namespace swift;

//===----------------------------------------------------------------------===//
//                       Check SILNode Type Properties
//===----------------------------------------------------------------------===//

/// These are just for performance and verification. If one needs to make
/// changes that cause the asserts the fire, please update them. The purpose is
/// to prevent these predicates from changing values by mistake.

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

void ValueBase::replaceAllUsesWithUndef() {
  auto *F = getFunction();
  if (!F) {
    llvm_unreachable("replaceAllUsesWithUndef can only be used on ValueBase "
                     "that have access to the parent function.");
  }
  while (!use_empty()) {
    Operand *Op = *use_begin();
    Op->set(SILUndef::get(Op->get()->getType(), *F));
  }
}

SILInstruction *ValueBase::getDefiningInstruction() {
  if (auto *inst = dyn_cast<SingleValueInstruction>(this))
    return inst;
  if (auto *result = dyn_cast<MultipleValueInstructionResult>(this))
    return result->getParent();
  return nullptr;
}

SILInstruction *ValueBase::getDefiningInsertionPoint() {
  if (auto *inst = getDefiningInstruction())
    return inst;
  if (auto *arg = dyn_cast<SILArgument>(this))
    return &*arg->getParentBlock()->begin();
  return nullptr;
}

Optional<ValueBase::DefiningInstructionResult>
ValueBase::getDefiningInstructionResult() {
  if (auto *inst = dyn_cast<SingleValueInstruction>(this))
    return DefiningInstructionResult{inst, 0};
  if (auto *result = dyn_cast<MultipleValueInstructionResult>(this))
    return DefiningInstructionResult{result->getParent(), result->getIndex()};
  return None;
}

SILBasicBlock *SILNode::getParentBlock() const {
  auto *CanonicalNode =
      const_cast<SILNode *>(this)->getRepresentativeSILNodeInObject();
  if (auto *Inst = dyn_cast<SILInstruction>(CanonicalNode))
    return Inst->getParent();
  if (auto *Arg = dyn_cast<SILArgument>(CanonicalNode))
    return Arg->getParent();
  return nullptr;
}

SILFunction *SILNode::getFunction() const {
  auto *CanonicalNode =
      const_cast<SILNode *>(this)->getRepresentativeSILNodeInObject();
  if (auto *Inst = dyn_cast<SILInstruction>(CanonicalNode))
    return Inst->getFunction();
  if (auto *Arg = dyn_cast<SILArgument>(CanonicalNode))
    return Arg->getFunction();
  return nullptr;
}

SILModule *SILNode::getModule() const {
  auto *CanonicalNode =
      const_cast<SILNode *>(this)->getRepresentativeSILNodeInObject();
  if (auto *Inst = dyn_cast<SILInstruction>(CanonicalNode))
    return &Inst->getModule();
  if (auto *Arg = dyn_cast<SILArgument>(CanonicalNode))
    return &Arg->getModule();
  return nullptr;
}

const SILNode *SILNode::getRepresentativeSILNodeSlowPath() const {
  assert(getStorageLoc() != SILNodeStorageLocation::Instruction);

  if (isa<SingleValueInstruction>(this)) {
    assert(hasMultipleSILNodeBases(getKind()));
    return &static_cast<const SILInstruction &>(
        static_cast<const SingleValueInstruction &>(
            static_cast<const ValueBase &>(*this)));
  }

  if (auto *MVR = dyn_cast<MultipleValueInstructionResult>(this)) {
    return MVR->getParent();
  }

  llvm_unreachable("Invalid value for slow path");
}

/// Get a location for this value.
SILLocation SILValue::getLoc() const {
  if (auto *instr = Value->getDefiningInstruction())
    return instr->getLoc();

  if (auto *arg = dyn_cast<SILArgument>(*this)) {
    if (arg->getDecl())
      return RegularLocation(const_cast<ValueDecl *>(arg->getDecl()));
  }
  // TODO: bbargs should probably use one of their operand locations.
  return Value->getFunction()->getLocation();
}

//===----------------------------------------------------------------------===//
//                               OwnershipKind
//===----------------------------------------------------------------------===//

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     const OwnershipKind &kind) {
  return os << kind.asString();
}

StringRef OwnershipKind::asString() const {
  switch (value) {
  case OwnershipKind::Any:
    return "any";
  case OwnershipKind::Unowned:
    return "unowned";
  case OwnershipKind::Owned:
    return "owned";
  case OwnershipKind::Guaranteed:
    return "guaranteed";
  case OwnershipKind::None:
    return "none";
  }
}

//===----------------------------------------------------------------------===//
//                             ValueOwnershipKind
//===----------------------------------------------------------------------===//

ValueOwnershipKind::ValueOwnershipKind(const SILFunction &F, SILType Type,
                                       SILArgumentConvention Convention)
    : value(OwnershipKind::Any) {
  auto &M = F.getModule();

  // Trivial types can be passed using a variety of conventions. They always
  // have trivial ownership.
  if (Type.isTrivial(F)) {
    value = OwnershipKind::None;
    return;
  }

  switch (Convention) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_In_Constant:
    value = SILModuleConventions(M).useLoweredAddresses()
                ? OwnershipKind::None
                : OwnershipKind::Owned;
    break;
  case SILArgumentConvention::Indirect_In_Guaranteed:
    value = SILModuleConventions(M).useLoweredAddresses()
                ? OwnershipKind::None
                : OwnershipKind::Guaranteed;
    break;
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    value = OwnershipKind::None;
    return;
  case SILArgumentConvention::Direct_Owned:
    value = OwnershipKind::Owned;
    return;
  case SILArgumentConvention::Direct_Unowned:
    value = OwnershipKind::Unowned;
    return;
  case SILArgumentConvention::Direct_Guaranteed:
    value = OwnershipKind::Guaranteed;
    return;
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("Not handled");
  }
}

StringRef ValueOwnershipKind::asString() const {
  return value.asString();
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind kind) {
  return os << kind.asString();
}

ValueOwnershipKind::ValueOwnershipKind(StringRef S)
    : value(OwnershipKind::Any) {
  auto Result = llvm::StringSwitch<Optional<OwnershipKind::innerty>>(S)
                    .Case("unowned", OwnershipKind::Unowned)
                    .Case("owned", OwnershipKind::Owned)
                    .Case("guaranteed", OwnershipKind::Guaranteed)
                    .Case("any", OwnershipKind::None)
                    .Default(None);
  if (!Result.hasValue())
    llvm_unreachable("Invalid string representation of ValueOwnershipKind");
  value = Result.getValue();
}

ValueOwnershipKind
ValueOwnershipKind::getProjectedOwnershipKind(const SILFunction &F,
                                              SILType Proj) const {
  if (Proj.isTrivial(F))
    return OwnershipKind::None;
  return *this;
}

#if 0
/// Map a SILValue mnemonic name to its ValueKind.
ValueKind swift::getSILValueKind(StringRef Name) {
#define SINGLE_VALUE_INST(Id, TextualName, Parent, MemoryBehavior,             \
                          ReleasingBehavior)                                   \
  if (Name == #TextualName)                                                    \
    return ValueKind::Id;

#define VALUE(Id, Parent)                                                      \
  if (Name == #Id)                                                             \
    return ValueKind::Id;

#include "swift/SIL/SILNodes.def"

#ifdef NDEBUG
  llvm::errs()
    << "Unknown SILValue name\n";
  abort();
#endif
  llvm_unreachable("Unknown SILValue name");
}

/// Map ValueKind to a corresponding mnemonic name.
StringRef swift::getSILValueName(ValueKind Kind) {
  switch (Kind) {
#define SINGLE_VALUE_INST(Id, TextualName, Parent, MemoryBehavior,             \
                          ReleasingBehavior)                                   \
  case ValueKind::Id:                                                          \
    return #TextualName;

#define VALUE(Id, Parent)                                                      \
  case ValueKind::Id:                                                          \
    return #Id;

#include "swift/SIL/SILNodes.def"
  }
}
#endif

//===----------------------------------------------------------------------===//
//                           UseLifetimeConstraint
//===----------------------------------------------------------------------===//

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     UseLifetimeConstraint constraint) {
  switch (constraint) {
  case UseLifetimeConstraint::NonLifetimeEnding:
    os << "NonLifetimeEnding";
    break;
  case UseLifetimeConstraint::LifetimeEnding:
    os << "LifetimeEnding";
    break;
  }
  return os;
}

//===----------------------------------------------------------------------===//
//                                  Operand
//===----------------------------------------------------------------------===//

SILBasicBlock *Operand::getParentBlock() const {
  auto *self = const_cast<Operand *>(this);
  return self->getUser()->getParent();
}

SILFunction *Operand::getParentFunction() const {
  auto *self = const_cast<Operand *>(this);
  return self->getUser()->getFunction();
}

bool Operand::canAcceptKind(ValueOwnershipKind kind) const {
  auto constraint = getOwnershipConstraint();
  if (!constraint)
    return false;

  if (constraint->satisfiesConstraint(kind))
    return true;

  // Then see if our preferred ownership constraint was not guaranteed or our
  // use lifetime constraint was LifetimeEnding. If it wasn't, then we fail
  // since we do not allow for implicit borrows in such situations.
  if (kind == OwnershipKind::Owned && !isLifetimeEnding()) {
    // Otherwise, we now know that our constraint is non lifetime ending
    // and guaranteed and our value had owned ownership. If our user
    // allows for implicit borrows and thus can accept owned values,
    // return true.
    if (auto borrowingOperand = BorrowingOperand::get(this))
      if (borrowingOperand->canAcceptOwnedValues())
        return true;
  }

  return false;
}

bool Operand::satisfiesConstraints() const {
  return canAcceptKind(get().getOwnershipKind());
}

bool Operand::isLifetimeEnding() const {
  auto constraint = getOwnershipConstraint();

  // If we got back Optional::None, then our operand is for a type dependent
  // operand. So return false.
  if (!constraint)
    return false;

  // If our use lifetime constraint is NonLifetimeEnding, just return false.
  if (!constraint->isLifetimeEnding())
    return false;

  // Otherwise, we may have a lifetime ending use. We consider two cases here:
  // the case where our value has OwnershipKind::None and one where it has some
  // other OwnershipKind. Note that values with OwnershipKind::None ownership
  // can not have their lifetime ended since they are outside of the ownership
  // system. Given such a case, if we have such a value we return
  // isLifetimeEnding() as false even if the constraint itself has a constraint
  // that says a value is LifetimeEnding. If we have a value that has a
  // non-OwnershipKind::None ownership then we just return true as expected.
  return get().getOwnershipKind() != OwnershipKind::None;
}

//===----------------------------------------------------------------------===//
//                             OperandConstraint
//===----------------------------------------------------------------------===//

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     OwnershipConstraint constraint) {
  return os << "<Constraint "
               "Kind:" << constraint.getPreferredKind()
            << " LifetimeConstraint:" << constraint.getLifetimeConstraint()
            << ">";
}
