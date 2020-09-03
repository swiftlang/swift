//===--- CSFix.cpp - Constraint Fixes -------------------------------------===//
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
//
// This file implements the \c ConstraintFix class and its related types,
// which is used by constraint solver to attempt to fix constraints to be
// able to produce a solution which is easily diagnosable.
//
//===----------------------------------------------------------------------===//

#include "CSFix.h"
#include "CSDiagnostics.h"
#include "ConstraintLocator.h"
#include "ConstraintSystem.h"
#include "OverloadChoice.h"
#include "swift/AST/Expr.h"
#include "swift/AST/ParameterList.h"
#include "swift/AST/Type.h"
#include "swift/AST/Types.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace swift;
using namespace constraints;

ConstraintFix::~ConstraintFix() {}

Expr *ConstraintFix::getAnchor() const { return getLocator()->getAnchor(); }

void ConstraintFix::print(llvm::raw_ostream &Out) const {
  Out << "[fix: ";
  Out << getName();
  Out << ']';
  Out << " @ ";
  getLocator()->dump(&CS.getASTContext().SourceMgr, Out);
}

void ConstraintFix::dump() const {print(llvm::errs()); }

std::string ForceDowncast::getName() const {
  llvm::SmallString<16> name;
  name += "force downcast (";
  name += getFromType()->getString();
  name += " as! ";
  name += getToType()->getString();
  name += ")";
  return name.c_str();
}

bool ForceDowncast::diagnose(const Solution &solution, bool asNote) const {
  MissingExplicitConversionFailure failure(solution, getFromType(), getToType(),
                                           getLocator());
  return failure.diagnose(asNote);
}

ForceDowncast *ForceDowncast::create(ConstraintSystem &cs, Type fromType,
                                     Type toType, ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceDowncast(cs, fromType, toType, locator);
}

bool ForceOptional::diagnose(const Solution &solution, bool asNote) const {
  MissingOptionalUnwrapFailure failure(solution, getFromType(), getToType(),
                                       getLocator());
  return failure.diagnose(asNote);
}

ForceOptional *ForceOptional::create(ConstraintSystem &cs, Type fromType,
                                     Type toType, ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceOptional(cs, fromType, toType, locator);
}

bool UnwrapOptionalBase::diagnose(const Solution &solution, bool asNote) const {
  bool resultIsOptional =
      getKind() == FixKind::UnwrapOptionalBaseWithOptionalResult;
  MemberAccessOnOptionalBaseFailure failure(solution, getLocator(), MemberName,
                                            resultIsOptional);
  return failure.diagnose(asNote);
}

UnwrapOptionalBase *UnwrapOptionalBase::create(ConstraintSystem &cs,
                                               DeclNameRef member,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      UnwrapOptionalBase(cs, FixKind::UnwrapOptionalBase, member, locator);
}

UnwrapOptionalBase *UnwrapOptionalBase::createWithOptionalResult(
    ConstraintSystem &cs, DeclNameRef member, ConstraintLocator *locator) {
  return new (cs.getAllocator()) UnwrapOptionalBase(
      cs, FixKind::UnwrapOptionalBaseWithOptionalResult, member, locator);
}

bool AddAddressOf::diagnose(const Solution &solution, bool asNote) const {
  MissingAddressOfFailure failure(solution, getFromType(), getToType(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AddAddressOf *AddAddressOf::create(ConstraintSystem &cs, Type argTy,
                                   Type paramTy, ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddAddressOf(cs, argTy, paramTy, locator);
}

bool TreatRValueAsLValue::diagnose(const Solution &solution,
                                   bool asNote) const {
  RValueTreatedAsLValueFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

TreatRValueAsLValue *TreatRValueAsLValue::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) TreatRValueAsLValue(cs, locator);
}

bool CoerceToCheckedCast::diagnose(const Solution &solution,
                                   bool asNote) const {
  MissingForcedDowncastFailure failure(solution, getFromType(), getToType(),
                                       getLocator());
  return failure.diagnose(asNote);
}

CoerceToCheckedCast *CoerceToCheckedCast::attempt(ConstraintSystem &cs,
                                                  Type fromType, Type toType,
                                                  ConstraintLocator *locator) {
  // If any of the types has a type variable, don't add the fix.
  if (fromType->hasTypeVariable() || toType->hasTypeVariable())
    return nullptr;

  auto *expr = locator->getAnchor();
  if (auto *assignExpr = dyn_cast<AssignExpr>(expr))
    expr = assignExpr->getSrc();
  auto *coerceExpr = dyn_cast<CoerceExpr>(expr);
  if (!coerceExpr)
    return nullptr;

  auto subExpr = coerceExpr->getSubExpr();
  auto castKind =
      TypeChecker::typeCheckCheckedCast(fromType, toType,
                                        CheckedCastContextKind::None, cs.DC,
                                        coerceExpr->getLoc(), subExpr,
                                        coerceExpr->getCastTypeLoc().getSourceRange());

  // Invalid cast.
  if (castKind == CheckedCastKind::Unresolved)
    return nullptr;

  return new (cs.getAllocator())
      CoerceToCheckedCast(cs, fromType, toType, locator);
}

bool MarkExplicitlyEscaping::diagnose(const Solution &solution,
                                      bool asNote) const {
  NoEscapeFuncToTypeConversionFailure failure(solution, getFromType(),
                                              getToType(), getLocator());
  return failure.diagnose(asNote);
}

MarkExplicitlyEscaping *
MarkExplicitlyEscaping::create(ConstraintSystem &cs, Type lhs, Type rhs,
                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) MarkExplicitlyEscaping(cs, lhs, rhs, locator);
}

bool RelabelArguments::diagnose(const Solution &solution, bool asNote) const {
  LabelingFailure failure(solution, getLocator(), getLabels());
  return failure.diagnose(asNote);
}

RelabelArguments *
RelabelArguments::create(ConstraintSystem &cs,
                         llvm::ArrayRef<Identifier> correctLabels,
                         ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<Identifier>(correctLabels.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(RelabelArguments));
  return new (mem) RelabelArguments(cs, correctLabels, locator);
}

bool MissingConformance::diagnose(const Solution &solution, bool asNote) const {
  auto *locator = getLocator();

  if (IsContextual) {
    auto &cs = solution.getConstraintSystem();
    auto context = cs.getContextualTypePurpose(locator->getAnchor());
    MissingContextualConformanceFailure failure(
        solution, context, NonConformingType, ProtocolType, locator);
    return failure.diagnose(asNote);
  }

  MissingConformanceFailure failure(
      solution, locator, std::make_pair(NonConformingType, ProtocolType));
  return failure.diagnose(asNote);
}

MissingConformance *
MissingConformance::forContextual(ConstraintSystem &cs, Type type,
                                  Type protocolType,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator()) MissingConformance(
      cs, /*isContextual=*/true, type, protocolType, locator);
}

MissingConformance *
MissingConformance::forRequirement(ConstraintSystem &cs, Type type,
                                   Type protocolType,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) MissingConformance(
      cs, /*isContextual=*/false, type, protocolType, locator);
}

bool SkipSameTypeRequirement::diagnose(const Solution &solution,
                                       bool asNote) const {
  SameTypeRequirementFailure failure(solution, LHS, RHS, getLocator());
  return failure.diagnose(asNote);
}

SkipSameTypeRequirement *
SkipSameTypeRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                ConstraintLocator *locator) {
  return new (cs.getAllocator()) SkipSameTypeRequirement(cs, lhs, rhs, locator);
}

bool SkipSuperclassRequirement::diagnose(const Solution &solution,
                                         bool asNote) const {
  SuperclassRequirementFailure failure(solution, LHS, RHS, getLocator());
  return failure.diagnose(asNote);
}

SkipSuperclassRequirement *
SkipSuperclassRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      SkipSuperclassRequirement(cs, lhs, rhs, locator);
}

bool ContextualMismatch::diagnose(const Solution &solution, bool asNote) const {
  ContextualFailure failure(solution, getFromType(), getToType(), getLocator());
  return failure.diagnose(asNote);
}

ContextualMismatch *ContextualMismatch::create(ConstraintSystem &cs, Type lhs,
                                               Type rhs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) ContextualMismatch(cs, lhs, rhs, locator);
}

/// Computes the contextual type information for a type mismatch of a
/// component in a structural type (tuple or function type).
///
/// \returns A tuple containing the contextual type purpose, the source type,
/// and the contextual type.
static Optional<std::tuple<ContextualTypePurpose, Type, Type>>
getStructuralTypeContext(const Solution &solution, ConstraintLocator *locator) {
  if (locator->findLast<LocatorPathElt::ContextualType>()) {
    assert(locator->isLastElement<LocatorPathElt::ContextualType>() ||
           locator->isLastElement<LocatorPathElt::FunctionArgument>());

    auto &cs = solution.getConstraintSystem();
    auto *anchor = locator->getAnchor();
    auto contextualType = cs.getContextualType(anchor);
    auto exprType = cs.getType(anchor);
    return std::make_tuple(cs.getContextualTypePurpose(anchor), exprType,
                           contextualType);
  } else if (auto argApplyInfo = solution.getFunctionArgApplyInfo(locator)) {
    return std::make_tuple(CTP_CallArgument,
                           argApplyInfo->getArgType(),
                           argApplyInfo->getParamType());
  } else if (auto *coerceExpr = dyn_cast<CoerceExpr>(locator->getAnchor())) {
    return std::make_tuple(CTP_CoerceOperand,
                           solution.getType(coerceExpr->getSubExpr()),
                           solution.getType(coerceExpr));
  } else if (auto *assignExpr = dyn_cast<AssignExpr>(locator->getAnchor())) {
    return std::make_tuple(CTP_AssignSource,
                           solution.getType(assignExpr->getSrc()),
                           solution.getType(assignExpr->getDest()));
  } else if (auto *call = dyn_cast<CallExpr>(locator->getAnchor())) {
    assert(isa<TypeExpr>(call->getFn()));
    return std::make_tuple(
        CTP_Initialization,
        solution.getType(call->getFn())->getMetatypeInstanceType(),
        solution.getType(call->getArg()));
  }

  return None;
}

bool AllowTupleTypeMismatch::coalesceAndDiagnose(
    const Solution &solution, ArrayRef<ConstraintFix *> fixes,
    bool asNote) const {
  llvm::SmallVector<unsigned, 4> indices;
  if (isElementMismatch())
    indices.push_back(*Index);

  for (auto fix : fixes) {
    auto *tupleFix = fix->getAs<AllowTupleTypeMismatch>();
    if (!tupleFix || !tupleFix->isElementMismatch())
      continue;
    indices.push_back(*tupleFix->Index);
  }

  auto &cs = getConstraintSystem();
  auto *locator = getLocator();
  ContextualTypePurpose purpose;
  Type fromType;
  Type toType;

  if (getFromType()->is<TupleType>() && getToType()->is<TupleType>()) {
    purpose = cs.getContextualTypePurpose(locator->getAnchor());
    fromType = getFromType();
    toType = getToType();
  } else if (auto contextualTypeInfo =
                 getStructuralTypeContext(solution, locator)) {
    std::tie(purpose, fromType, toType) = *contextualTypeInfo;
  } else {
    return false;
  }

  TupleContextualFailure failure(solution, purpose, fromType, toType, indices,
                                 locator);
  return failure.diagnose(asNote);
}

bool AllowTupleTypeMismatch::diagnose(const Solution &solution,
                                      bool asNote) const {
  return coalesceAndDiagnose(solution, {}, asNote);
}

AllowTupleTypeMismatch *
AllowTupleTypeMismatch::create(ConstraintSystem &cs, Type lhs, Type rhs,
                               ConstraintLocator *locator,
                               Optional<unsigned> index) {
  return new (cs.getAllocator())
      AllowTupleTypeMismatch(cs, lhs, rhs, locator, index);
}

bool AllowFunctionTypeMismatch::coalesceAndDiagnose(
    const Solution &solution, ArrayRef<ConstraintFix *> fixes,
    bool asNote) const {
  llvm::SmallVector<unsigned, 4> indices{ParamIndex};

  for (auto fix : fixes) {
    if (auto *fnFix = fix->getAs<AllowFunctionTypeMismatch>())
      indices.push_back(fnFix->ParamIndex);
  }

  auto *locator = getLocator();
  ContextualTypePurpose purpose;
  Type fromType;
  Type toType;

  auto contextualTypeInfo = getStructuralTypeContext(solution, locator);
  if (!contextualTypeInfo)
    return false;

  std::tie(purpose, fromType, toType) = *contextualTypeInfo;
  FunctionTypeMismatch failure(solution, purpose, fromType, toType, indices,
                               locator);
  return failure.diagnose(asNote);
}

bool AllowFunctionTypeMismatch::diagnose(const Solution &solution,
                                         bool asNote) const {
  return coalesceAndDiagnose(solution, {}, asNote);
}

AllowFunctionTypeMismatch *
AllowFunctionTypeMismatch::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                  ConstraintLocator *locator, unsigned index) {
  return new (cs.getAllocator())
      AllowFunctionTypeMismatch(cs, lhs, rhs, locator, index);
}

bool GenericArgumentsMismatch::diagnose(const Solution &solution,
                                        bool asNote) const {
  GenericArgumentsMismatchFailure failure(solution, getFromType(), getToType(),
                                          getMismatches(), getLocator());
  return failure.diagnose(asNote);
}

GenericArgumentsMismatch *GenericArgumentsMismatch::create(
    ConstraintSystem &cs, Type actual, Type required,
    llvm::ArrayRef<unsigned> mismatches, ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<unsigned>(mismatches.size());
  void *mem =
      cs.getAllocator().Allocate(size, alignof(GenericArgumentsMismatch));
  return new (mem)
      GenericArgumentsMismatch(cs, actual, required, mismatches, locator);
}

bool AutoClosureForwarding::diagnose(const Solution &solution,
                                     bool asNote) const {
  AutoClosureForwardingFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

AutoClosureForwarding *AutoClosureForwarding::create(ConstraintSystem &cs,
                                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) AutoClosureForwarding(cs, locator);
}

bool AllowAutoClosurePointerConversion::diagnose(const Solution &solution,
                                                 bool asNote) const {
  AutoClosurePointerConversionFailure failure(solution, getFromType(),
                                              getToType(), getLocator());
  return failure.diagnose(asNote);
}

AllowAutoClosurePointerConversion *
AllowAutoClosurePointerConversion::create(ConstraintSystem &cs, Type pointeeType,
                                          Type pointerType,
                                          ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowAutoClosurePointerConversion(cs, pointeeType, pointerType, locator);
}

bool RemoveUnwrap::diagnose(const Solution &solution, bool asNote) const {
  NonOptionalUnwrapFailure failure(solution, BaseType, getLocator());
  return failure.diagnose(asNote);
}

RemoveUnwrap *RemoveUnwrap::create(ConstraintSystem &cs, Type baseType,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveUnwrap(cs, baseType, locator);
}

bool InsertExplicitCall::diagnose(const Solution &solution, bool asNote) const {
  MissingCallFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

InsertExplicitCall *InsertExplicitCall::create(ConstraintSystem &cs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) InsertExplicitCall(cs, locator);
}

bool UsePropertyWrapper::diagnose(const Solution &solution, bool asNote) const {
  ExtraneousPropertyWrapperUnwrapFailure failure(
      solution, Wrapped, UsingStorageWrapper, Base, Wrapper, getLocator());
  return failure.diagnose(asNote);
}

UsePropertyWrapper *UsePropertyWrapper::create(ConstraintSystem &cs,
                                               VarDecl *wrapped,
                                               bool usingStorageWrapper,
                                               Type base, Type wrapper,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) UsePropertyWrapper(
      cs, wrapped, usingStorageWrapper, base, wrapper, locator);
}

bool UseWrappedValue::diagnose(const Solution &solution, bool asNote) const {
  MissingPropertyWrapperUnwrapFailure failure(solution, PropertyWrapper,
                                              usingStorageWrapper(), Base,
                                              Wrapper, getLocator());
  return failure.diagnose(asNote);
}

UseWrappedValue *UseWrappedValue::create(ConstraintSystem &cs,
                                         VarDecl *propertyWrapper, Type base,
                                         Type wrapper,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator())
      UseWrappedValue(cs, propertyWrapper, base, wrapper, locator);
}

bool UseSubscriptOperator::diagnose(const Solution &solution,
                                    bool asNote) const {
  SubscriptMisuseFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

UseSubscriptOperator *UseSubscriptOperator::create(ConstraintSystem &cs,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) UseSubscriptOperator(cs, locator);
}

bool DefineMemberBasedOnUse::diagnose(const Solution &solution,
                                      bool asNote) const {
  MissingMemberFailure failure(solution, BaseType, Name, getLocator());
  return AlreadyDiagnosed || failure.diagnose(asNote);
}

bool
DefineMemberBasedOnUse::diagnoseForAmbiguity(CommonFixesArray commonFixes) const {
  Type concreteBaseType;
  for (const auto &solutionAndFix : commonFixes) {
    const auto *solution = solutionAndFix.first;
    const auto *fix = solutionAndFix.second->getAs<DefineMemberBasedOnUse>();

    auto baseType = solution->simplifyType(fix->BaseType);
    if (!concreteBaseType)
      concreteBaseType = baseType;

    if (concreteBaseType->getCanonicalType() != baseType->getCanonicalType()) {
      getConstraintSystem().getASTContext().Diags.diagnose(getAnchor()->getLoc(),
          diag::unresolved_member_no_inference, Name);
      return true;
    }
  }

  return diagnose(*commonFixes.front().first);
}

DefineMemberBasedOnUse *
DefineMemberBasedOnUse::create(ConstraintSystem &cs, Type baseType,
                               DeclNameRef member, bool alreadyDiagnosed,
                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      DefineMemberBasedOnUse(cs, baseType, member, alreadyDiagnosed, locator);
}

AllowMemberRefOnExistential *
AllowMemberRefOnExistential::create(ConstraintSystem &cs, Type baseType,
                                    ValueDecl *member, DeclNameRef memberName,
                                    ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowMemberRefOnExistential(cs, baseType, memberName, member, locator);
}

bool AllowMemberRefOnExistential::diagnose(const Solution &solution,
                                           bool asNote) const {
  InvalidMemberRefOnExistential failure(solution, getBaseType(),
                                        getMemberName(), getLocator());
  return failure.diagnose(asNote);
}

bool AllowTypeOrInstanceMember::diagnose(const Solution &solution,
                                         bool asNote) const {
  AllowTypeOrInstanceMemberFailure failure(solution, getBaseType(), getMember(),
                                           getMemberName(), getLocator());
  return failure.diagnose(asNote);
}

AllowTypeOrInstanceMember *
AllowTypeOrInstanceMember::create(ConstraintSystem &cs, Type baseType,
                                  ValueDecl *member, DeclNameRef usedName,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowTypeOrInstanceMember(cs, baseType, member, usedName, locator);
}

bool AllowInvalidPartialApplication::diagnose(const Solution &solution,
                                              bool asNote) const {
  PartialApplicationFailure failure(isWarning(), solution, getLocator());
  return failure.diagnose(asNote);
}

AllowInvalidPartialApplication *
AllowInvalidPartialApplication::create(bool isWarning, ConstraintSystem &cs,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInvalidPartialApplication(isWarning, cs, locator);
}

bool AllowInvalidInitRef::diagnose(const Solution &solution,
                                   bool asNote) const {
  switch (Kind) {
  case RefKind::DynamicOnMetatype: {
    InvalidDynamicInitOnMetatypeFailure failure(solution, BaseType, Init,
                                                BaseRange, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::ProtocolMetatype: {
    InitOnProtocolMetatypeFailure failure(
        solution, BaseType, Init, IsStaticallyDerived, BaseRange, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::NonConstMetatype: {
    ImplicitInitOnNonConstMetatypeFailure failure(solution, BaseType, Init,
                                                  getLocator());
    return failure.diagnose(asNote);
  }
  }
  llvm_unreachable("covered switch");
}

AllowInvalidInitRef *AllowInvalidInitRef::dynamicOnMetatype(
    ConstraintSystem &cs, Type baseTy, ConstructorDecl *init,
    SourceRange baseRange, ConstraintLocator *locator) {
  return create(RefKind::DynamicOnMetatype, cs, baseTy, init,
                /*isStaticallyDerived=*/false, baseRange, locator);
}

AllowInvalidInitRef *AllowInvalidInitRef::onProtocolMetatype(
    ConstraintSystem &cs, Type baseTy, ConstructorDecl *init,
    bool isStaticallyDerived, SourceRange baseRange,
    ConstraintLocator *locator) {
  return create(RefKind::ProtocolMetatype, cs, baseTy, init,
                isStaticallyDerived, baseRange, locator);
}

AllowInvalidInitRef *
AllowInvalidInitRef::onNonConstMetatype(ConstraintSystem &cs, Type baseTy,
                                        ConstructorDecl *init,
                                        ConstraintLocator *locator) {
  return create(RefKind::NonConstMetatype, cs, baseTy, init,
                /*isStaticallyDerived=*/false, SourceRange(), locator);
}

AllowInvalidInitRef *
AllowInvalidInitRef::create(RefKind kind, ConstraintSystem &cs, Type baseTy,
                            ConstructorDecl *init, bool isStaticallyDerived,
                            SourceRange baseRange, ConstraintLocator *locator) {
  return new (cs.getAllocator()) AllowInvalidInitRef(
      cs, kind, baseTy, init, isStaticallyDerived, baseRange, locator);
}

bool AllowClosureParamDestructuring::diagnose(const Solution &solution,
                                              bool asNote) const {
  ClosureParamDestructuringFailure failure(solution, ContextualType,
                                           getLocator());
  return failure.diagnose(asNote);
}

AllowClosureParamDestructuring *
AllowClosureParamDestructuring::create(ConstraintSystem &cs,
                                       FunctionType *contextualType,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowClosureParamDestructuring(cs, contextualType, locator);
}

bool AddMissingArguments::diagnose(const Solution &solution,
                                   bool asNote) const {
  MissingArgumentsFailure failure(solution, getSynthesizedArguments(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AddMissingArguments *
AddMissingArguments::create(ConstraintSystem &cs,
                            ArrayRef<SynthesizedParam> synthesizedArgs,
                            ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<SynthesizedParam>(synthesizedArgs.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(AddMissingArguments));
  return new (mem) AddMissingArguments(cs, synthesizedArgs, locator);
}

bool RemoveExtraneousArguments::diagnose(const Solution &solution,
                                         bool asNote) const {
  ExtraneousArgumentsFailure failure(solution, ContextualType,
                                     getExtraArguments(), getLocator());
  return failure.diagnose(asNote);
}

bool RemoveExtraneousArguments::isMinMaxNameShadowing(
    ConstraintSystem &cs, ConstraintLocatorBuilder locator) {
  auto *anchor = dyn_cast_or_null<CallExpr>(locator.getAnchor());
  if (!anchor)
    return false;

  if (auto *UDE = dyn_cast<UnresolvedDotExpr>(anchor->getFn())) {
    if (auto *baseExpr = dyn_cast<DeclRefExpr>(UDE->getBase())) {
      auto *decl = baseExpr->getDecl();
      if (baseExpr->isImplicit() && decl &&
          decl->getName() == cs.getASTContext().Id_self) {
        auto memberName = UDE->getName();
        return memberName.isSimpleName("min") || memberName.isSimpleName("max");
      }
    }
  }

  return false;
}

RemoveExtraneousArguments *RemoveExtraneousArguments::create(
    ConstraintSystem &cs, FunctionType *contextualType,
    llvm::ArrayRef<IndexedParam> extraArgs, ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<IndexedParam>(extraArgs.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(RemoveExtraneousArguments));
  return new (mem)
      RemoveExtraneousArguments(cs, contextualType, extraArgs, locator);
}

bool MoveOutOfOrderArgument::diagnose(const Solution &solution,
                                      bool asNote) const {
  OutOfOrderArgumentFailure failure(solution, ArgIdx, PrevArgIdx, Bindings,
                                    getLocator());
  return failure.diagnose(asNote);
}

MoveOutOfOrderArgument *MoveOutOfOrderArgument::create(
    ConstraintSystem &cs, unsigned argIdx, unsigned prevArgIdx,
    ArrayRef<ParamBinding> bindings, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      MoveOutOfOrderArgument(cs, argIdx, prevArgIdx, bindings, locator);
}

bool AllowInaccessibleMember::diagnose(const Solution &solution,
                                       bool asNote) const {
  InaccessibleMemberFailure failure(solution, getMember(), getLocator());
  return failure.diagnose(asNote);
}

AllowInaccessibleMember *
AllowInaccessibleMember::create(ConstraintSystem &cs, Type baseType,
                                ValueDecl *member, DeclNameRef name,
                                ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInaccessibleMember(cs, baseType, member, name, locator);
}

bool AllowAnyObjectKeyPathRoot::diagnose(const Solution &solution,
                                         bool asNote) const {
  AnyObjectKeyPathRootFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

AllowAnyObjectKeyPathRoot *
AllowAnyObjectKeyPathRoot::create(ConstraintSystem &cs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator()) AllowAnyObjectKeyPathRoot(cs, locator);
}

bool AllowMultiArgFuncKeyPathMismatch::diagnose(const Solution &solution,
                                                bool asNote) const {
  MultiArgFuncKeyPathFailure failure(solution, functionType, getLocator());
  return failure.diagnose(asNote);
}

AllowMultiArgFuncKeyPathMismatch *
AllowMultiArgFuncKeyPathMismatch::create(ConstraintSystem &cs, Type fnType,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator())
  AllowMultiArgFuncKeyPathMismatch(cs, fnType, locator);
}

bool TreatKeyPathSubscriptIndexAsHashable::diagnose(const Solution &solution,
                                                    bool asNote) const {
  KeyPathSubscriptIndexHashableFailure failure(solution, NonConformingType,
                                               getLocator());
  return failure.diagnose(asNote);
}

TreatKeyPathSubscriptIndexAsHashable *
TreatKeyPathSubscriptIndexAsHashable::create(ConstraintSystem &cs, Type type,
                                             ConstraintLocator *locator) {
  return new (cs.getAllocator())
      TreatKeyPathSubscriptIndexAsHashable(cs, type, locator);
}

bool AllowInvalidRefInKeyPath::diagnose(const Solution &solution,
                                        bool asNote) const {
  switch (Kind) {
  case RefKind::StaticMember: {
    InvalidStaticMemberRefInKeyPath failure(solution, Member, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::EnumCase: {
    InvalidEnumCaseRefInKeyPath failure(solution, Member, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::MutatingGetter: {
    InvalidMemberWithMutatingGetterInKeyPath failure(solution, Member,
                                                     getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::Method:
  case RefKind::Initializer: {
    InvalidMethodRefInKeyPath failure(solution, Member, getLocator());
    return failure.diagnose(asNote);
  }
  }
  llvm_unreachable("covered switch");
}

AllowInvalidRefInKeyPath *
AllowInvalidRefInKeyPath::forRef(ConstraintSystem &cs, ValueDecl *member,
                                 ConstraintLocator *locator) {
  // Referencing (instance or static) methods in key path is
  // not currently allowed.
  if (isa<FuncDecl>(member))
    return AllowInvalidRefInKeyPath::create(cs, RefKind::Method, member,
                                            locator);

  // Referencing enum cases in key path is not currently allowed.
  if (isa<EnumElementDecl>(member)) {
    return AllowInvalidRefInKeyPath::create(cs, RefKind::EnumCase, member,
                                            locator);
  }

  // Referencing initializers in key path is not currently allowed.
  if (isa<ConstructorDecl>(member))
    return AllowInvalidRefInKeyPath::create(cs, RefKind::Initializer,
                                            member, locator);

  // Referencing static members in key path is not currently allowed.
  if (member->isStatic())
    return AllowInvalidRefInKeyPath::create(cs, RefKind::StaticMember, member,
                                            locator);

  if (auto *storage = dyn_cast<AbstractStorageDecl>(member)) {
    // Referencing members with mutating getters in key path is not
    // currently allowed.
    if (storage->isGetterMutating())
      return AllowInvalidRefInKeyPath::create(cs, RefKind::MutatingGetter,
                                              member, locator);
  }

  return nullptr;
}

AllowInvalidRefInKeyPath *
AllowInvalidRefInKeyPath::create(ConstraintSystem &cs, RefKind kind,
                                 ValueDecl *member,
                                 ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInvalidRefInKeyPath(cs, kind, member, locator);
}

KeyPathContextualMismatch *
KeyPathContextualMismatch::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      KeyPathContextualMismatch(cs, lhs, rhs, locator);
}

bool RemoveAddressOf::diagnose(const Solution &solution, bool asNote) const {
  InvalidUseOfAddressOf failure(solution, getFromType(), getToType(),
                                getLocator());
  return failure.diagnose(asNote);
}

RemoveAddressOf *RemoveAddressOf::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveAddressOf(cs, lhs, rhs, locator);
}

bool RemoveReturn::diagnose(const Solution &solution, bool asNote) const {
  ExtraneousReturnFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

RemoveReturn *RemoveReturn::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveReturn(cs, locator);
}

bool CollectionElementContextualMismatch::diagnose(const Solution &solution,
                                                   bool asNote) const {
  CollectionElementContextualFailure failure(solution, getFromType(),
                                             getToType(), getLocator());
  return failure.diagnose(asNote);
}

CollectionElementContextualMismatch *
CollectionElementContextualMismatch::create(ConstraintSystem &cs, Type srcType,
                                            Type dstType,
                                            ConstraintLocator *locator) {
  return new (cs.getAllocator())
      CollectionElementContextualMismatch(cs, srcType, dstType, locator);
}

bool DefaultGenericArgument::coalesceAndDiagnose(
    const Solution &solution, ArrayRef<ConstraintFix *> fixes,
    bool asNote) const {
  llvm::SmallVector<GenericTypeParamType *, 4> missingParams{Param};

  for (auto *otherFix : fixes) {
    if (auto *fix = otherFix->getAs<DefaultGenericArgument>())
      missingParams.push_back(fix->Param);
  }

  MissingGenericArgumentsFailure failure(solution, missingParams, getLocator());
  return failure.diagnose(asNote);
}

bool DefaultGenericArgument::diagnose(const Solution &solution,
                                      bool asNote) const {
  return coalesceAndDiagnose(solution, {}, asNote);
}

DefaultGenericArgument *
DefaultGenericArgument::create(ConstraintSystem &cs, GenericTypeParamType *param,
                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) DefaultGenericArgument(cs, param, locator);
}

SkipUnhandledConstructInFunctionBuilder *
SkipUnhandledConstructInFunctionBuilder::create(ConstraintSystem &cs,
                                                UnhandledNode unhandled,
                                                NominalTypeDecl *builder,
                                                ConstraintLocator *locator) {
  return new (cs.getAllocator())
    SkipUnhandledConstructInFunctionBuilder(cs, unhandled, builder, locator);
}

bool SkipUnhandledConstructInFunctionBuilder::diagnose(const Solution &solution,
                                                       bool asNote) const {
  SkipUnhandledConstructInFunctionBuilderFailure failure(solution, unhandled,
                                                         builder, getLocator());
  return failure.diagnose(asNote);
}

bool AllowMutatingMemberOnRValueBase::diagnose(const Solution &solution,
                                               bool asNote) const {
  MutatingMemberRefOnImmutableBase failure(solution, getMember(), getLocator());
  return failure.diagnose(asNote);
}

AllowMutatingMemberOnRValueBase *
AllowMutatingMemberOnRValueBase::create(ConstraintSystem &cs, Type baseType,
                                        ValueDecl *member, DeclNameRef name,
                                        ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowMutatingMemberOnRValueBase(cs, baseType, member, name, locator);
}

bool AllowTupleSplatForSingleParameter::diagnose(const Solution &solution,
                                                 bool asNote) const {
  InvalidTupleSplatWithSingleParameterFailure failure(solution, ParamType,
                                                      getLocator());
  return failure.diagnose(asNote);
}

bool AllowTupleSplatForSingleParameter::attempt(
    ConstraintSystem &cs, SmallVectorImpl<Param> &args, ArrayRef<Param> params,
    SmallVectorImpl<SmallVector<unsigned, 1>> &bindings,
    ConstraintLocatorBuilder locator) {
  if (params.size() != 1 || args.size() <= 1)
    return true;

  const auto &param = params.front();

  if (param.isInOut() || param.isVariadic() || param.isAutoClosure())
    return true;

  auto paramTy = param.getOldType();

  // Parameter type has to be either a tuple (with the same arity as
  // argument list), or a type variable.
  if (!(paramTy->is<TupleType>() &&
        paramTy->castTo<TupleType>()->getNumElements() == args.size()))
    return true;

  SmallVector<TupleTypeElt, 4> argElts;

  for (unsigned index : indices(args)) {
    const auto &arg = args[index];

    auto label = arg.getLabel();
    auto flags = arg.getParameterFlags();

    // In situations where there is a single labeled parameter
    // we need to form a tuple which omits the label e.g.
    //
    // func foo<T>(x: (T, T)) {}
    // foo(x: 0, 1)
    //
    // We'd want to suggest argument list to be `x: (0, 1)` instead
    // of `(x: 0, 1)` which would be incorrect.
    if (param.hasLabel() && label == param.getLabel()) {
      if (index == 0) {
        label = Identifier();
      } else {
        // If label match anything other than first argument,
        // this can't be a tuple splat.
        return true;
      }
    }

    // Tuple can't have `inout` elements.
    if (flags.isInOut())
      return true;

    argElts.push_back({arg.getPlainType(), label});
  }

  bindings[0].clear();
  bindings[0].push_back(0);

  auto newArgType = TupleType::get(argElts, cs.getASTContext());

  args.clear();
  args.push_back(AnyFunctionType::Param(newArgType, param.getLabel()));

  auto *fix = new (cs.getAllocator()) AllowTupleSplatForSingleParameter(
      cs, paramTy, cs.getConstraintLocator(locator));

  return cs.recordFix(fix);
}

bool DropThrowsAttribute::diagnose(const Solution &solution,
                                   bool asNote) const {
  ThrowingFunctionConversionFailure failure(solution, getFromType(),
                                            getToType(), getLocator());
  return failure.diagnose(asNote);
}

DropThrowsAttribute *DropThrowsAttribute::create(ConstraintSystem &cs,
                                                 FunctionType *fromType,
                                                 FunctionType *toType,
                                                 ConstraintLocator *locator) {
  return new (cs.getAllocator())
      DropThrowsAttribute(cs, fromType, toType, locator);
}

bool IgnoreContextualType::diagnose(const Solution &solution,
                                    bool asNote) const {
  ContextualFailure failure(solution, getFromType(), getToType(), getLocator());
  return failure.diagnose(asNote);
}

IgnoreContextualType *IgnoreContextualType::create(ConstraintSystem &cs,
                                                   Type resultTy,
                                                   Type specifiedTy,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator())
      IgnoreContextualType(cs, resultTy, specifiedTy, locator);
}

bool IgnoreAssignmentDestinationType::diagnose(const Solution &solution,
                                               bool asNote) const {
  auto &cs = getConstraintSystem();
  auto *AE = cast<AssignExpr>(getAnchor());

  // Let's check whether this is a situation of chained assignment where
  // one of the steps in the chain is an assignment to self e.g.
  // `let _ = { $0 = $0 = 42 }`. Assignment chaining results in
  // type mismatch between result of the previous assignment and the next.
  {
    llvm::SaveAndRestore<AssignExpr *> anchor(AE);

    do {
      if (TypeChecker::diagnoseSelfAssignment(AE))
        return true;
    } while ((AE = dyn_cast_or_null<AssignExpr>(cs.getParentExpr(AE))));
  }

  auto CTP = isa<SubscriptExpr>(AE->getDest()) ? CTP_SubscriptAssignSource
                                               : CTP_AssignSource;

  AssignmentTypeMismatchFailure failure(
      solution, CTP, getFromType(), getToType(),
      cs.getConstraintLocator(AE->getSrc(), LocatorPathElt::ContextualType()));
  return failure.diagnose(asNote);
}

IgnoreAssignmentDestinationType *
IgnoreAssignmentDestinationType::create(ConstraintSystem &cs, Type sourceTy,
                                        Type destTy,
                                        ConstraintLocator *locator) {
  return new (cs.getAllocator())
      IgnoreAssignmentDestinationType(cs, sourceTy, destTy, locator);
}

bool AllowInOutConversion::diagnose(const Solution &solution,
                                    bool asNote) const {
  InOutConversionFailure failure(solution, getFromType(), getToType(),
                                 getLocator());
  return failure.diagnose(asNote);
}

AllowInOutConversion *AllowInOutConversion::create(ConstraintSystem &cs,
                                                   Type argType, Type paramType,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInOutConversion(cs, argType, paramType, locator);
}

/// Check whether given `value` type is indeed a the same type as a `RawValue`
/// type of a given raw representable type.
static bool isValueOfRawRepresentable(ConstraintSystem &cs,
                                      Type rawRepresentableType,
                                      Type valueType) {
  auto rawType = isRawRepresentable(cs, rawRepresentableType);
  if (!rawType)
    return false;

  KnownProtocolKind protocols[] = {
      KnownProtocolKind::ExpressibleByStringLiteral,
      KnownProtocolKind::ExpressibleByIntegerLiteral};

  for (auto protocol : protocols) {
    if (conformsToKnownProtocol(cs, valueType, protocol) &&
        valueType->isEqual(rawType))
      return true;
  }

  return false;
}

ExpandArrayIntoVarargs *
ExpandArrayIntoVarargs::attempt(ConstraintSystem &cs, Type argType,
                                Type paramType,
                                ConstraintLocatorBuilder builder) {
  auto *locator = cs.getConstraintLocator(builder);

  auto argLoc = locator->getLastElementAs<LocatorPathElt::ApplyArgToParam>();
  if (!(argLoc && argLoc->getParameterFlags().isVariadic()))
    return nullptr;

  auto elementType = cs.isArrayType(argType);
  if (!elementType)
    return nullptr;

  ConstraintSystem::TypeMatchOptions options;
  options |= ConstraintSystem::TypeMatchFlags::TMF_ApplyingFix;
  options |= ConstraintSystem::TypeMatchFlags::TMF_GenerateConstraints;

  auto result = cs.matchTypes(*elementType, paramType, ConstraintKind::Subtype,
                              options, builder);

  if (result.isFailure())
    return nullptr;

  return new (cs.getAllocator())
      ExpandArrayIntoVarargs(cs, argType, paramType, locator);
}

bool ExpandArrayIntoVarargs::diagnose(const Solution &solution,
                                      bool asNote) const {
  ExpandArrayIntoVarargsFailure failure(solution, getFromType(), getToType(),
                                        getLocator());
  return failure.diagnose(asNote);
}

ExplicitlyConstructRawRepresentable *
ExplicitlyConstructRawRepresentable::attempt(ConstraintSystem &cs, Type argType,
                                             Type paramType,
                                             ConstraintLocatorBuilder locator) {
  auto rawRepresentableType = paramType->lookThroughAllOptionalTypes();
  auto valueType = argType->lookThroughAllOptionalTypes();

  if (isValueOfRawRepresentable(cs, rawRepresentableType, valueType))
    return new (cs.getAllocator()) ExplicitlyConstructRawRepresentable(
        cs, valueType, rawRepresentableType, cs.getConstraintLocator(locator));

  return nullptr;
}

UseValueTypeOfRawRepresentative *
UseValueTypeOfRawRepresentative::attempt(ConstraintSystem &cs, Type argType,
                                         Type paramType,
                                         ConstraintLocatorBuilder locator) {
  auto rawRepresentableType = argType->lookThroughAllOptionalTypes();
  auto valueType = paramType->lookThroughAllOptionalTypes();

  if (isValueOfRawRepresentable(cs, rawRepresentableType, valueType))
    return new (cs.getAllocator()) UseValueTypeOfRawRepresentative(
        cs, rawRepresentableType, valueType, cs.getConstraintLocator(locator));

  return nullptr;
}

unsigned AllowArgumentMismatch::getParamIdx() const {
  const auto *locator = getLocator();
  auto elt = locator->castLastElementTo<LocatorPathElt::ApplyArgToParam>();
  return elt.getParamIdx();
}

bool AllowArgumentMismatch::diagnose(const Solution &solution,
                                     bool asNote) const {
  ArgumentMismatchFailure failure(solution, getFromType(), getToType(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AllowArgumentMismatch *
AllowArgumentMismatch::create(ConstraintSystem &cs, Type argType,
                              Type paramType, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowArgumentMismatch(cs, argType, paramType, locator);
}

bool RemoveInvalidCall::diagnose(const Solution &solution, bool asNote) const {
  ExtraneousCallFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

RemoveInvalidCall *RemoveInvalidCall::create(ConstraintSystem &cs,
                                             ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveInvalidCall(cs, locator);
}

bool TreatEphemeralAsNonEphemeral::diagnose(const Solution &solution,
                                            bool asNote) const {
  NonEphemeralConversionFailure failure(solution, getLocator(), getFromType(),
                                        getToType(), ConversionKind,
                                        isWarning());
  return failure.diagnose(asNote);
}

TreatEphemeralAsNonEphemeral *TreatEphemeralAsNonEphemeral::create(
    ConstraintSystem &cs, ConstraintLocator *locator, Type srcType,
    Type dstType, ConversionRestrictionKind conversionKind,
    bool downgradeToWarning) {
  return new (cs.getAllocator()) TreatEphemeralAsNonEphemeral(
      cs, locator, srcType, dstType, conversionKind, downgradeToWarning);
}

std::string TreatEphemeralAsNonEphemeral::getName() const {
  std::string name;
  name += "treat ephemeral as non-ephemeral for ";
  name += ::getName(ConversionKind);
  return name;
}

bool SpecifyBaseTypeForContextualMember::diagnose(const Solution &solution,
                                                  bool asNote) const {
  MissingContextualBaseInMemberRefFailure failure(solution, MemberName,
                                                  getLocator());
  return failure.diagnose(asNote);
}

SpecifyBaseTypeForContextualMember *SpecifyBaseTypeForContextualMember::create(
    ConstraintSystem &cs, DeclNameRef member, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      SpecifyBaseTypeForContextualMember(cs, member, locator);
}

std::string SpecifyClosureParameterType::getName() const {
  std::string name;
  llvm::raw_string_ostream OS(name);

  auto *closure = cast<ClosureExpr>(getAnchor());
  auto paramLoc =
      getLocator()->castLastElementTo<LocatorPathElt::TupleElement>();

  auto *PD = closure->getParameters()->get(paramLoc.getIndex());

  OS << "specify type for parameter ";
  if (PD->isAnonClosureParam()) {
    OS << "$" << paramLoc.getIndex();
  } else {
    OS << "'" << PD->getParameterName() << "'";
  }

  return OS.str();
}

bool SpecifyClosureParameterType::diagnose(const Solution &solution,
                                           bool asNote) const {
  UnableToInferClosureParameterType failure(solution, getLocator());
  return failure.diagnose(asNote);
}

SpecifyClosureParameterType *
SpecifyClosureParameterType::create(ConstraintSystem &cs,
                                    ConstraintLocator *locator) {
  return new (cs.getAllocator()) SpecifyClosureParameterType(cs, locator);
}

bool SpecifyClosureReturnType::diagnose(const Solution &solution,
                                        bool asNote) const {
  UnableToInferClosureReturnType failure(solution, getLocator());
  return failure.diagnose(asNote);
}

SpecifyClosureReturnType *
SpecifyClosureReturnType::create(ConstraintSystem &cs,
                                 ConstraintLocator *locator) {
  return new (cs.getAllocator()) SpecifyClosureReturnType(cs, locator);
}

bool SpecifyObjectLiteralTypeImport::diagnose(const Solution &solution,
                                              bool asNote) const {
  UnableToInferProtocolLiteralType failure(solution, getLocator());
  return failure.diagnose(asNote);
}

SpecifyObjectLiteralTypeImport *
SpecifyObjectLiteralTypeImport::create(ConstraintSystem &cs,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator()) SpecifyObjectLiteralTypeImport(cs, locator);
}

AllowNonClassTypeToConvertToAnyObject::AllowNonClassTypeToConvertToAnyObject(
    ConstraintSystem &cs, Type type, ConstraintLocator *locator)
    : ContextualMismatch(cs, FixKind::AllowNonClassTypeToConvertToAnyObject,
                         type, cs.getASTContext().getAnyObjectType(), locator) {
}

bool AllowNonClassTypeToConvertToAnyObject::diagnose(const Solution &solution,
                                                     bool asNote) const {
  auto *locator = getLocator();

  if (locator->isForContextualType()) {
    ContextualFailure failure(solution, getFromType(), getToType(), locator);
    return failure.diagnose(asNote);
  }

  if (locator->isLastElement<LocatorPathElt::ApplyArgToParam>()) {
    ArgumentMismatchFailure failure(solution, getFromType(), getToType(),
                                    locator);
    return failure.diagnose(asNote);
  }

  return false;
}

AllowNonClassTypeToConvertToAnyObject *
AllowNonClassTypeToConvertToAnyObject::create(ConstraintSystem &cs, Type type,
                                              ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowNonClassTypeToConvertToAnyObject(cs, type, locator);
}

bool AddQualifierToAccessTopLevelName::diagnose(const Solution &solution,
                                                bool asNote) const {
  MissingQuialifierInMemberRefFailure failure(solution, getLocator());
  return failure.diagnose(asNote);
}

AddQualifierToAccessTopLevelName *
AddQualifierToAccessTopLevelName::create(ConstraintSystem &cs,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddQualifierToAccessTopLevelName(cs, locator);
}

bool AllowCoercionToForceCast::diagnose(const Solution &solution,
                                        bool asNote) const {
  CoercionAsForceCastFailure failure(solution, getFromType(), getToType(),
                                     getLocator());
  return failure.diagnose(asNote);
}

AllowCoercionToForceCast *
AllowCoercionToForceCast::create(ConstraintSystem &cs, Type fromType,
                                 Type toType, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowCoercionToForceCast(cs, fromType, toType, locator);
}

bool AllowKeyPathRootTypeMismatch::diagnose(const Solution &solution,
                                            bool asNote) const {
  KeyPathRootTypeMismatchFailure failure(solution, getFromType(), getToType(),
                                         getLocator());
  return failure.diagnose(asNote);
}

AllowKeyPathRootTypeMismatch *
AllowKeyPathRootTypeMismatch::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                     ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowKeyPathRootTypeMismatch(cs, lhs, rhs, locator);
}

SpecifyKeyPathRootType *
SpecifyKeyPathRootType::create(ConstraintSystem &cs,
                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      SpecifyKeyPathRootType(cs, locator);
}

bool SpecifyKeyPathRootType::diagnose(const Solution &solution,
                                      bool asNote) const {
  UnableToInferKeyPathRootFailure failure(solution, getLocator());
  
  return failure.diagnose(asNote);
}

bool IgnoreInvalidFunctionBuilderBody::diagnose(const Solution &solution,
                                                bool asNote) const {
  switch (Phase) {
  // Handled below
  case ErrorInPhase::PreCheck:
    break;
  case ErrorInPhase::ConstraintGeneration:
    return true; // Already diagnosed by `matchFunctionBuilder`.
  }

  auto *S = cast<ClosureExpr>(getAnchor())->getBody();

  class PreCheckWalker : public ASTWalker {
    DeclContext *DC;
    DiagnosticTransaction Transaction;

  public:
    PreCheckWalker(DeclContext *dc)
        : DC(dc), Transaction(dc->getASTContext().Diags) {}

    std::pair<bool, Expr *> walkToExprPre(Expr *E) override {
      auto hasError = ConstraintSystem::preCheckExpression(
          E, DC, /*replaceInvalidRefsWithErrors=*/true);
      return std::make_pair(false, hasError ? nullptr : E);
    }

    std::pair<bool, Stmt *> walkToStmtPre(Stmt *S) override {
      return std::make_pair(true, S);
    }

    // Ignore patterns because function builder pre-check does so as well.
    std::pair<bool, Pattern *> walkToPatternPre(Pattern *P) override {
      return std::make_pair(false, P);
    }

    bool diagnosed() const {
      return Transaction.hasErrors();
    }
  };

  auto &cs = getConstraintSystem();
  PreCheckWalker walker(cs.DC);
  S->walk(walker);

  return walker.diagnosed();
}

IgnoreInvalidFunctionBuilderBody *IgnoreInvalidFunctionBuilderBody::create(
    ConstraintSystem &cs, ErrorInPhase phase, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      IgnoreInvalidFunctionBuilderBody(cs, phase, locator);
}
