//===--- ApplySite.h -------------------------------------*- mode: c++ -*--===//
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
///
/// \file
///
/// This file defines utilities for working with "call-site like" SIL
/// instructions. We use the term "call-site" like since we handle partial
/// applications in our utilities.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_APPLYSITE_H
#define SWIFT_SIL_APPLYSITE_H

#include "swift/SIL/SILInstruction.h"

namespace swift {

//===----------------------------------------------------------------------===//
//                                 ApplySite
//===----------------------------------------------------------------------===//

struct ApplySiteKind {
  enum innerty : std::underlying_type<SILInstructionKind>::type {
#define APPLYSITE_INST(ID, PARENT) ID = unsigned(SILInstructionKind::ID),
#include "swift/SIL/SILNodes.def"
  } value;

  explicit ApplySiteKind(SILInstructionKind kind) {
    auto newValue = ApplySiteKind::fromNodeKindHelper(kind);
    assert(newValue && "Non apply site passed into ApplySiteKind");
    value = newValue.getValue();
  }

  ApplySiteKind(innerty value) : value(value) {}
  operator innerty() const { return value; }

  static Optional<ApplySiteKind> fromNodeKind(SILInstructionKind kind) {
    if (auto innerTyOpt = ApplySiteKind::fromNodeKindHelper(kind))
      return ApplySiteKind(*innerTyOpt);
    return None;
  }

private:
  static Optional<innerty> fromNodeKindHelper(SILInstructionKind kind) {
    switch (kind) {
#define APPLYSITE_INST(ID, PARENT)                                             \
  case SILInstructionKind::ID:                                                 \
    return ApplySiteKind::ID;
#include "swift/SIL/SILNodes.def"
    default:
      return None;
    }
  }
};

/// An apply instruction.
class ApplySite {
  SILInstruction *Inst;

protected:
  explicit ApplySite(void *p) : Inst(static_cast<SILInstruction *>(p)) {}

public:
  ApplySite() : Inst(nullptr) {}
  explicit ApplySite(SILInstruction *inst)
      : Inst(static_cast<SILInstruction *>(inst)) {
    assert(classof(inst) && "not an apply instruction?");
  }
  ApplySite(ApplyInst *inst) : Inst(inst) {}
  ApplySite(PartialApplyInst *inst) : Inst(inst) {}
  ApplySite(TryApplyInst *inst) : Inst(inst) {}
  ApplySite(BeginApplyInst *inst) : Inst(inst) {}

  SILModule &getModule() const { return Inst->getModule(); }

  static ApplySite isa(SILNode *node) {
    auto *i = dyn_cast<SILInstruction>(node);
    if (!i)
      return ApplySite();

    auto kind = ApplySiteKind::fromNodeKind(i->getKind());
    if (!kind)
      return ApplySite();

    switch (kind.getValue()) {
    case ApplySiteKind::ApplyInst:
      return ApplySite(cast<ApplyInst>(node));
    case ApplySiteKind::BeginApplyInst:
      return ApplySite(cast<BeginApplyInst>(node));
    case ApplySiteKind::TryApplyInst:
      return ApplySite(cast<TryApplyInst>(node));
    case ApplySiteKind::PartialApplyInst:
      return ApplySite(cast<PartialApplyInst>(node));
    }
  }

  ApplySiteKind getKind() const { return ApplySiteKind(Inst->getKind()); }

  explicit operator bool() const { return Inst != nullptr; }

  SILInstruction *getInstruction() const { return Inst; }
  SILLocation getLoc() const { return Inst->getLoc(); }
  const SILDebugScope *getDebugScope() const { return Inst->getDebugScope(); }
  SILFunction *getFunction() const { return Inst->getFunction(); }
  SILBasicBlock *getParent() const { return Inst->getParent(); }

#define FOREACH_IMPL_RETURN(OPERATION)                                         \
  do {                                                                         \
    switch (ApplySiteKind(Inst->getKind())) {                                  \
    case ApplySiteKind::ApplyInst:                                             \
      return cast<ApplyInst>(Inst)->OPERATION;                                 \
    case ApplySiteKind::BeginApplyInst:                                        \
      return cast<BeginApplyInst>(Inst)->OPERATION;                            \
    case ApplySiteKind::PartialApplyInst:                                      \
      return cast<PartialApplyInst>(Inst)->OPERATION;                          \
    case ApplySiteKind::TryApplyInst:                                          \
      return cast<TryApplyInst>(Inst)->OPERATION;                              \
    }                                                                          \
  } while (0)

  /// Return the callee operand.
  SILValue getCallee() const { FOREACH_IMPL_RETURN(getCallee()); }

  /// Return the callee value by looking through function conversions until we
  /// find a function_ref, partial_apply, or unrecognized callee value.
  SILValue getCalleeOrigin() const { FOREACH_IMPL_RETURN(getCalleeOrigin()); }

  /// Gets the referenced function by looking through partial apply,
  /// convert_function, and thin to thick function until we find a function_ref.
  SILFunction *getCalleeFunction() const {
    FOREACH_IMPL_RETURN(getCalleeFunction());
  }

  /// Return the referenced function if the callee is a function_ref
  /// instruction.
  SILFunction *getReferencedFunction() const {
    FOREACH_IMPL_RETURN(getReferencedFunction());
  }

  /// Return the type.
  SILType getType() const { return getSubstCalleeConv().getSILResultType(); }

  /// Get the type of the callee without the applied substitutions.
  CanSILFunctionType getOrigCalleeType() const {
    return getCallee()->getType().castTo<SILFunctionType>();
  }
  /// Get the conventions of the callee without the applied substitutions.
  SILFunctionConventions getOrigCalleeConv() const {
    return SILFunctionConventions(getOrigCalleeType(), getModule());
  }

  /// Get the type of the callee with the applied substitutions.
  CanSILFunctionType getSubstCalleeType() const {
    return getSubstCalleeSILType().castTo<SILFunctionType>();
  }
  SILType getSubstCalleeSILType() const {
    FOREACH_IMPL_RETURN(getSubstCalleeSILType());
  }
  /// Get the conventions of the callee with the applied substitutions.
  SILFunctionConventions getSubstCalleeConv() const {
    return SILFunctionConventions(getSubstCalleeType(), getModule());
  }

  /// Returns true if the callee function is annotated with
  /// @_semantics("programtermination_point")
  bool isCalleeKnownProgramTerminationPoint() const {
    FOREACH_IMPL_RETURN(isCalleeKnownProgramTerminationPoint());
  }

  /// Check if this is a call of a never-returning function.
  bool isCalleeNoReturn() const { FOREACH_IMPL_RETURN(isCalleeNoReturn()); }

  bool isCalleeThin() const {
    switch (getSubstCalleeType()->getRepresentation()) {
    case SILFunctionTypeRepresentation::CFunctionPointer:
    case SILFunctionTypeRepresentation::Thin:
    case SILFunctionTypeRepresentation::Method:
    case SILFunctionTypeRepresentation::ObjCMethod:
    case SILFunctionTypeRepresentation::WitnessMethod:
    case SILFunctionTypeRepresentation::Closure:
      return true;
    case SILFunctionTypeRepresentation::Block:
    case SILFunctionTypeRepresentation::Thick:
      return false;
    }
  }

  /// True if this application has generic substitutions.
  bool hasSubstitutions() const { FOREACH_IMPL_RETURN(hasSubstitutions()); }

  /// The substitutions used to bind the generic arguments of this function.
  SubstitutionMap getSubstitutionMap() const {
    FOREACH_IMPL_RETURN(getSubstitutionMap());
  }

  /// Return the associated specialization information.
  const GenericSpecializationInformation *getSpecializationInfo() const {
    FOREACH_IMPL_RETURN(getSpecializationInfo());
  }

  /// Return an operand list corresponding to the applied arguments.
  MutableArrayRef<Operand> getArgumentOperands() const {
    FOREACH_IMPL_RETURN(getArgumentOperands());
  }

  /// Return a list of applied argument values.
  OperandValueArrayRef getArguments() const {
    FOREACH_IMPL_RETURN(getArguments());
  }

  /// Return the number of applied arguments.
  unsigned getNumArguments() const { FOREACH_IMPL_RETURN(getNumArguments()); }

  /// Return the apply operand for the given applied argument index.
  Operand &getArgumentRef(unsigned i) const { return getArgumentOperands()[i]; }

  /// Return the ith applied argument.
  SILValue getArgument(unsigned i) const { return getArguments()[i]; }

  /// Set the ith applied argument.
  void setArgument(unsigned i, SILValue V) const {
    getArgumentOperands()[i].set(V);
  }

  /// Return the operand index of the first applied argument.
  unsigned getOperandIndexOfFirstArgument() const {
    FOREACH_IMPL_RETURN(getArgumentOperandNumber());
  }
#undef FOREACH_IMPL_RETURN

  /// Returns true if \p oper is an argument operand and not the callee
  /// operand.
  bool isArgumentOperand(const Operand &oper) const {
    return oper.getOperandNumber() >= getOperandIndexOfFirstArgument();
  }

  /// Return the applied argument index for the given operand.
  unsigned getAppliedArgIndex(const Operand &oper) const {
    assert(oper.getUser() == Inst);
    assert(isArgumentOperand(oper));

    return oper.getOperandNumber() - getOperandIndexOfFirstArgument();
  }

  /// Return the callee's function argument index corresponding to the first
  /// applied argument: 0 for full applies; >= 0 for partial applies.
  unsigned getCalleeArgIndexOfFirstAppliedArg() const {
    switch (ApplySiteKind(Inst->getKind())) {
    case ApplySiteKind::ApplyInst:
    case ApplySiteKind::BeginApplyInst:
    case ApplySiteKind::TryApplyInst:
      return 0;
    case ApplySiteKind::PartialApplyInst:
      // The arguments to partial_apply are a suffix of the partial_apply's
      // callee. Note that getSubstCalleeConv is function type of the callee
      // argument passed to this apply, not necessarilly the function type of
      // the underlying callee function (i.e. it is based on the `getCallee`
      // type, not the `getCalleeOrigin` type).
      //
      // pa1 = partial_apply f(c) : $(a, b, c)
      // pa2 = partial_apply pa1(b) : $(a, b)
      // apply pa2(a)
      return getSubstCalleeConv().getNumSILArguments() - getNumArguments();
    }
  }

  /// Return the callee's function argument index corresponding to the given
  /// apply operand. Each function argument index identifies a
  /// SILFunctionArgument in the callee and can be used as a
  /// SILFunctionConvention argument index.
  ///
  /// Note: Passing an applied argument index into SILFunctionConvention, as
  /// opposed to a function argument index, is incorrect.
  unsigned getCalleeArgIndex(const Operand &oper) const {
    return getCalleeArgIndexOfFirstAppliedArg() + getAppliedArgIndex(oper);
  }

  /// Return the SILArgumentConvention for the given applied argument operand.
  SILArgumentConvention getArgumentConvention(Operand &oper) const {
    unsigned calleeArgIdx =
        getCalleeArgIndexOfFirstAppliedArg() + getAppliedArgIndex(oper);
    return getSubstCalleeConv().getSILArgumentConvention(calleeArgIdx);
  }

  /// Return true if 'self' is an applied argument.
  bool hasSelfArgument() const {
    switch (ApplySiteKind(Inst->getKind())) {
    case ApplySiteKind::ApplyInst:
      return cast<ApplyInst>(Inst)->hasSelfArgument();
    case ApplySiteKind::BeginApplyInst:
      return cast<BeginApplyInst>(Inst)->hasSelfArgument();
    case ApplySiteKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->hasSelfArgument();
    case ApplySiteKind::PartialApplyInst:
      llvm_unreachable("unhandled case");
    }
  }

  /// Return the applied 'self' argument value.
  SILValue getSelfArgument() const {
    switch (ApplySiteKind(Inst->getKind())) {
    case ApplySiteKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getSelfArgument();
    case ApplySiteKind::BeginApplyInst:
      return cast<BeginApplyInst>(Inst)->getSelfArgument();
    case ApplySiteKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getSelfArgument();
    case ApplySiteKind::PartialApplyInst:
      llvm_unreachable("unhandled case");
    }
  }

  /// Return the 'self' apply operand.
  Operand &getSelfArgumentOperand() {
    switch (ApplySiteKind(Inst->getKind())) {
    case ApplySiteKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getSelfArgumentOperand();
    case ApplySiteKind::BeginApplyInst:
      return cast<BeginApplyInst>(Inst)->getSelfArgumentOperand();
    case ApplySiteKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getSelfArgumentOperand();
    case ApplySiteKind::PartialApplyInst:
      llvm_unreachable("Unhandled cast");
    }
  }

  /// Return a list of applied arguments without self.
  OperandValueArrayRef getArgumentsWithoutSelf() const {
    switch (ApplySiteKind(Inst->getKind())) {
    case ApplySiteKind::ApplyInst:
      return cast<ApplyInst>(Inst)->getArgumentsWithoutSelf();
    case ApplySiteKind::BeginApplyInst:
      return cast<BeginApplyInst>(Inst)->getArgumentsWithoutSelf();
    case ApplySiteKind::TryApplyInst:
      return cast<TryApplyInst>(Inst)->getArgumentsWithoutSelf();
    case ApplySiteKind::PartialApplyInst:
      llvm_unreachable("Unhandled case");
    }
  }

  /// Return whether the given apply is of a formally-throwing function
  /// which is statically known not to throw.
  bool isNonThrowing() const {
    switch (ApplySiteKind(getInstruction()->getKind())) {
    case ApplySiteKind::ApplyInst:
      return cast<ApplyInst>(Inst)->isNonThrowing();
    case ApplySiteKind::BeginApplyInst:
      return cast<BeginApplyInst>(Inst)->isNonThrowing();
    case ApplySiteKind::TryApplyInst:
      return false;
    case ApplySiteKind::PartialApplyInst:
      llvm_unreachable("Unhandled case");
    }
  }

  static ApplySite getFromOpaqueValue(void *p) { return ApplySite(p); }

  friend bool operator==(ApplySite lhs, ApplySite rhs) {
    return lhs.getInstruction() == rhs.getInstruction();
  }
  friend bool operator!=(ApplySite lhs, ApplySite rhs) {
    return lhs.getInstruction() != rhs.getInstruction();
  }

  static bool classof(const SILInstruction *inst) {
    return bool(ApplySiteKind::fromNodeKind(inst->getKind()));
  }
};

//===----------------------------------------------------------------------===//
//                               FullApplySite
//===----------------------------------------------------------------------===//

struct FullApplySiteKind {
  enum innerty : std::underlying_type<SILInstructionKind>::type {
#define FULLAPPLYSITE_INST(ID, PARENT) ID = unsigned(SILInstructionKind::ID),
#include "swift/SIL/SILNodes.def"
  } value;

  explicit FullApplySiteKind(SILInstructionKind kind) {
    auto fullApplySiteKind = FullApplySiteKind::fromNodeKindHelper(kind);
    assert(fullApplySiteKind && "SILNodeKind is not a FullApplySiteKind?!");
    value = fullApplySiteKind.getValue();
  }

  FullApplySiteKind(innerty value) : value(value) {}
  operator innerty() const { return value; }

  static Optional<FullApplySiteKind> fromNodeKind(SILInstructionKind kind) {
    if (auto innerOpt = FullApplySiteKind::fromNodeKindHelper(kind))
      return FullApplySiteKind(*innerOpt);
    return None;
  }

private:
  static Optional<innerty> fromNodeKindHelper(SILInstructionKind kind) {
    switch (kind) {
#define FULLAPPLYSITE_INST(ID, PARENT)                                         \
  case SILInstructionKind::ID:                                                 \
    return FullApplySiteKind::ID;
#include "swift/SIL/SILNodes.def"
    default:
      return None;
    }
  }
};

/// A full function application.
class FullApplySite : public ApplySite {
  explicit FullApplySite(void *p) : ApplySite(p) {}

public:
  FullApplySite() : ApplySite() {}
  explicit FullApplySite(SILInstruction *inst) : ApplySite(inst) {
    assert(classof(inst) && "not an apply instruction?");
  }
  FullApplySite(ApplyInst *inst) : ApplySite(inst) {}
  FullApplySite(BeginApplyInst *inst) : ApplySite(inst) {}
  FullApplySite(TryApplyInst *inst) : ApplySite(inst) {}

  static FullApplySite isa(SILNode *node) {
    auto *i = dyn_cast<SILInstruction>(node);
    if (!i)
      return FullApplySite();
    auto kind = FullApplySiteKind::fromNodeKind(i->getKind());
    if (!kind)
      return FullApplySite();
    switch (kind.getValue()) {
    case FullApplySiteKind::ApplyInst:
      return FullApplySite(cast<ApplyInst>(node));
    case FullApplySiteKind::BeginApplyInst:
      return FullApplySite(cast<BeginApplyInst>(node));
    case FullApplySiteKind::TryApplyInst:
      return FullApplySite(cast<TryApplyInst>(node));
    }
  }

  FullApplySiteKind getKind() const {
    return FullApplySiteKind(getInstruction()->getKind());
  }

  bool hasIndirectSILResults() const {
    return getSubstCalleeConv().hasIndirectSILResults();
  }

  unsigned getNumIndirectSILResults() const {
    return getSubstCalleeConv().getNumIndirectSILResults();
  }

  OperandValueArrayRef getIndirectSILResults() const {
    return getArguments().slice(0, getNumIndirectSILResults());
  }

  OperandValueArrayRef getArgumentsWithoutIndirectResults() const {
    return getArguments().slice(getNumIndirectSILResults());
  }

  /// Returns true if \p op is the callee operand of this apply site
  /// and not an argument operand.
  bool isCalleeOperand(const Operand &op) const {
    return op.getOperandNumber() < getOperandIndexOfFirstArgument();
  }

  /// Returns true if \p op is an operand that passes an indirect
  /// result argument to the apply site.
  bool isIndirectResultOperand(const Operand &op) const {
    return getCalleeArgIndex(op) < getNumIndirectSILResults();
  }

  static FullApplySite getFromOpaqueValue(void *p) { return FullApplySite(p); }

  static bool classof(const SILInstruction *inst) {
    return bool(FullApplySiteKind::fromNodeKind(inst->getKind()));
  }
};

} // namespace swift

namespace llvm {

// An ApplySite casts like a SILInstruction*.
template <> struct simplify_type<const ::swift::ApplySite> {
  using SimpleType = ::swift::SILInstruction *;
  static SimpleType getSimplifiedValue(const ::swift::ApplySite &Val) {
    return Val.getInstruction();
  }
};
template <>
struct simplify_type<::swift::ApplySite>
    : public simplify_type<const ::swift::ApplySite> {};
template <>
struct simplify_type<::swift::FullApplySite>
    : public simplify_type<const ::swift::ApplySite> {};
template <>
struct simplify_type<const ::swift::FullApplySite>
    : public simplify_type<const ::swift::ApplySite> {};

template <> struct DenseMapInfo<::swift::ApplySite> {
  static ::swift::ApplySite getEmptyKey() {
    return ::swift::ApplySite::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getEmptyKey());
  }
  static ::swift::ApplySite getTombstoneKey() {
    return ::swift::ApplySite::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getTombstoneKey());
  }
  static unsigned getHashValue(::swift::ApplySite AS) {
    auto *I = AS.getInstruction();
    return DenseMapInfo<::swift::SILInstruction *>::getHashValue(I);
  }
  static bool isEqual(::swift::ApplySite LHS, ::swift::ApplySite RHS) {
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<::swift::FullApplySite> {
  static ::swift::FullApplySite getEmptyKey() {
    return ::swift::FullApplySite::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getEmptyKey());
  }
  static ::swift::FullApplySite getTombstoneKey() {
    return ::swift::FullApplySite::getFromOpaqueValue(
        llvm::DenseMapInfo<void *>::getTombstoneKey());
  }
  static unsigned getHashValue(::swift::FullApplySite AS) {
    auto *I = AS.getInstruction();
    return DenseMapInfo<::swift::SILInstruction *>::getHashValue(I);
  }
  static bool isEqual(::swift::FullApplySite LHS, ::swift::FullApplySite RHS) {
    return LHS == RHS;
  }
};

} // namespace llvm

#endif
