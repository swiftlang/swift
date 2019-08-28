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
//                             ValueOwnershipKind
//===----------------------------------------------------------------------===//

ValueOwnershipKind::ValueOwnershipKind(const SILFunction &F, SILType Type,
                                       SILArgumentConvention Convention)
    : Value() {
  auto &M = F.getModule();

  // Trivial types can be passed using a variety of conventions. They always
  // have trivial ownership.
  if (Type.isTrivial(F)) {
    Value = ValueOwnershipKind::Any;
    return;
  }

  switch (Convention) {
  case SILArgumentConvention::Indirect_In:
  case SILArgumentConvention::Indirect_In_Constant:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Any
      : ValueOwnershipKind::Owned;
    break;
  case SILArgumentConvention::Indirect_In_Guaranteed:
    Value = SILModuleConventions(M).useLoweredAddresses()
      ? ValueOwnershipKind::Any
      : ValueOwnershipKind::Guaranteed;
    break;
  case SILArgumentConvention::Indirect_Inout:
  case SILArgumentConvention::Indirect_InoutAliasable:
  case SILArgumentConvention::Indirect_Out:
    Value = ValueOwnershipKind::Any;
    return;
  case SILArgumentConvention::Direct_Owned:
    Value = ValueOwnershipKind::Owned;
    return;
  case SILArgumentConvention::Direct_Unowned:
    Value = ValueOwnershipKind::Unowned;
    return;
  case SILArgumentConvention::Direct_Guaranteed:
    Value = ValueOwnershipKind::Guaranteed;
    return;
  case SILArgumentConvention::Direct_Deallocating:
    llvm_unreachable("Not handled");
  }
}

StringRef ValueOwnershipKind::asString() const {
  switch (Value) {
  case ValueOwnershipKind::Unowned:
    return "unowned";
  case ValueOwnershipKind::Owned:
    return "owned";
  case ValueOwnershipKind::Guaranteed:
    return "guaranteed";
  case ValueOwnershipKind::Any:
    return "any";
  }
  llvm_unreachable("Unhandled ValueOwnershipKind in switch.");
}

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     ValueOwnershipKind kind) {
  return os << kind.asString();
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
                    .Case("unowned", ValueOwnershipKind::Unowned)
                    .Case("owned", ValueOwnershipKind::Owned)
                    .Case("guaranteed", ValueOwnershipKind::Guaranteed)
                    .Case("any", ValueOwnershipKind::Any)
                    .Default(None);
  if (!Result.hasValue())
    llvm_unreachable("Invalid string representation of ValueOwnershipKind");
  Value = Result.getValue();
}

ValueOwnershipKind
ValueOwnershipKind::getProjectedOwnershipKind(const SILFunction &F,
                                              SILType Proj) const {
  if (Proj.isTrivial(F))
    return ValueOwnershipKind::Any;
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
//                          OperandOwnershipKindMap
//===----------------------------------------------------------------------===//

void OperandOwnershipKindMap::print(llvm::raw_ostream &os) const {
  os << "-- OperandOwnershipKindMap --\n";

  unsigned index = 0;
  unsigned end = unsigned(ValueOwnershipKind::LastValueOwnershipKind) + 1;
  while (index != end) {
    auto kind = ValueOwnershipKind(index);
    if (canAcceptKind(kind)) {
      os << kind << ": Yes. Liveness: " << getLifetimeConstraint(kind) << "\n";
    } else {
      os << kind << ":  No."
         << "\n";
    }
    ++index;
  }
}

void OperandOwnershipKindMap::dump() const { print(llvm::dbgs()); }

//===----------------------------------------------------------------------===//
//                           UseLifetimeConstraint
//===----------------------------------------------------------------------===//

llvm::raw_ostream &swift::operator<<(llvm::raw_ostream &os,
                                     UseLifetimeConstraint constraint) {
  switch (constraint) {
  case UseLifetimeConstraint::MustBeLive:
    os << "MustBeLive";
    break;
  case UseLifetimeConstraint::MustBeInvalidated:
    os << "MustBeInvalidated";
    break;
  }
  return os;
}
