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

bool ForceDowncast::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  MissingExplicitConversionFailure failure(cs, getFromType(), getToType(),
                                           getLocator());
  return failure.diagnose(asNote);
}

ForceDowncast *ForceDowncast::create(ConstraintSystem &cs, Type fromType,
                                     Type toType, ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceDowncast(cs, fromType, toType, locator);
}

bool ForceOptional::diagnose(bool asNote) const {
  MissingOptionalUnwrapFailure failure(getConstraintSystem(), getFromType(),
                                       getToType(), getLocator());
  return failure.diagnose(asNote);
}

ForceOptional *ForceOptional::create(ConstraintSystem &cs, Type fromType,
                                     Type toType, ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceOptional(cs, fromType, toType, locator);
}

bool UnwrapOptionalBase::diagnose(bool asNote) const {
  bool resultIsOptional =
      getKind() == FixKind::UnwrapOptionalBaseWithOptionalResult;
  MemberAccessOnOptionalBaseFailure failure(
      getConstraintSystem(), getLocator(), MemberName, resultIsOptional);
  return failure.diagnose(asNote);
}

UnwrapOptionalBase *UnwrapOptionalBase::create(ConstraintSystem &cs,
                                               DeclName member,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      UnwrapOptionalBase(cs, FixKind::UnwrapOptionalBase, member, locator);
}

UnwrapOptionalBase *UnwrapOptionalBase::createWithOptionalResult(
    ConstraintSystem &cs, DeclName member, ConstraintLocator *locator) {
  return new (cs.getAllocator()) UnwrapOptionalBase(
      cs, FixKind::UnwrapOptionalBaseWithOptionalResult, member, locator);
}

bool AddAddressOf::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  MissingAddressOfFailure failure(cs, getFromType(), getToType(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AddAddressOf *AddAddressOf::create(ConstraintSystem &cs, Type argTy,
                                   Type paramTy, ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddAddressOf(cs, argTy, paramTy, locator);
}

bool TreatRValueAsLValue::diagnose(bool asNote) const {
  RValueTreatedAsLValueFailure failure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

TreatRValueAsLValue *TreatRValueAsLValue::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) TreatRValueAsLValue(cs, locator);
}

bool CoerceToCheckedCast::diagnose(bool asNote) const {
  MissingForcedDowncastFailure failure(getConstraintSystem(),
                                       getFromType(), getToType(),
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

bool MarkExplicitlyEscaping::diagnose(bool asNote) const {
  NoEscapeFuncToTypeConversionFailure failure(getConstraintSystem(),
                                              getLocator(), ConvertTo);
  return failure.diagnose(asNote);
}

MarkExplicitlyEscaping *
MarkExplicitlyEscaping::create(ConstraintSystem &cs, ConstraintLocator *locator,
                               Type convertingTo) {
  return new (cs.getAllocator())
      MarkExplicitlyEscaping(cs, locator, convertingTo);
}

bool RelabelArguments::diagnose(bool asNote) const {
  LabelingFailure failure(getConstraintSystem(), getLocator(),
                          getLabels());
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

bool MissingConformance::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  auto *locator = getLocator();

  if (IsContextual) {
    auto context = cs.getContextualTypePurpose();
    MissingContextualConformanceFailure failure(
        cs, context, NonConformingType, ProtocolType, locator);
    return failure.diagnose(asNote);
  }

  MissingConformanceFailure failure(
      cs, locator, std::make_pair(NonConformingType, ProtocolType));
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

bool SkipSameTypeRequirement::diagnose(bool asNote) const {
  SameTypeRequirementFailure failure(getConstraintSystem(), LHS, RHS,
                                     getLocator());
  return failure.diagnose(asNote);
}

SkipSameTypeRequirement *
SkipSameTypeRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                ConstraintLocator *locator) {
  return new (cs.getAllocator()) SkipSameTypeRequirement(cs, lhs, rhs, locator);
}

bool SkipSuperclassRequirement::diagnose(bool asNote) const {
  SuperclassRequirementFailure failure(getConstraintSystem(), LHS, RHS,
                                       getLocator());
  return failure.diagnose(asNote);
}

SkipSuperclassRequirement *
SkipSuperclassRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      SkipSuperclassRequirement(cs, lhs, rhs, locator);
}

bool ContextualMismatch::diagnose(bool asNote) const {
  auto failure = ContextualFailure(getConstraintSystem(), getFromType(),
                                   getToType(), getLocator());
  return failure.diagnose(asNote);
}

ContextualMismatch *ContextualMismatch::create(ConstraintSystem &cs, Type lhs,
                                               Type rhs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) ContextualMismatch(cs, lhs, rhs, locator);
}

bool AllowTupleTypeMismatch::diagnose(bool asNote) const {
  auto failure = TupleContextualFailure(
      getConstraintSystem(), getFromType(), getToType(), getLocator());
  return failure.diagnose(asNote);
}

AllowTupleTypeMismatch *
AllowTupleTypeMismatch::create(ConstraintSystem &cs, Type lhs, Type rhs,
                               ConstraintLocator *locator) {
  assert(lhs->is<TupleType>() && rhs->is<TupleType>() &&
         "lhs and rhs must be tuple types");
  return new (cs.getAllocator()) AllowTupleTypeMismatch(cs, lhs, rhs, locator);
}

bool GenericArgumentsMismatch::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  GenericArgumentsMismatchFailure failure(cs, getFromType(), getToType(),
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

bool AutoClosureForwarding::diagnose(bool asNote) const {
  auto failure =
      AutoClosureForwardingFailure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AutoClosureForwarding *AutoClosureForwarding::create(ConstraintSystem &cs,
                                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) AutoClosureForwarding(cs, locator);
}

bool AllowAutoClosurePointerConversion::diagnose(bool asNote) const {
  auto failure = AutoClosurePointerConversionFailure(getConstraintSystem(),
      getFromType(), getToType(), getLocator());
  return failure.diagnose(asNote);
}

AllowAutoClosurePointerConversion *
AllowAutoClosurePointerConversion::create(ConstraintSystem &cs, Type pointeeType,
                                          Type pointerType,
                                          ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowAutoClosurePointerConversion(cs, pointeeType, pointerType, locator);
}

bool RemoveUnwrap::diagnose(bool asNote) const {
  auto failure = NonOptionalUnwrapFailure(getConstraintSystem(), BaseType,
                                          getLocator());
  return failure.diagnose(asNote);
}

RemoveUnwrap *RemoveUnwrap::create(ConstraintSystem &cs, Type baseType,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveUnwrap(cs, baseType, locator);
}

bool InsertExplicitCall::diagnose(bool asNote) const {
  auto failure = MissingCallFailure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

InsertExplicitCall *InsertExplicitCall::create(ConstraintSystem &cs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) InsertExplicitCall(cs, locator);
}

bool UsePropertyWrapper::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  auto failure = ExtraneousPropertyWrapperUnwrapFailure(
      cs, Wrapped, UsingStorageWrapper, Base, Wrapper, getLocator());
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

bool UseWrappedValue::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  auto failure = MissingPropertyWrapperUnwrapFailure(
      cs, PropertyWrapper, usingStorageWrapper(), Base, Wrapper,
      getLocator());
  return failure.diagnose(asNote);
}

UseWrappedValue *UseWrappedValue::create(ConstraintSystem &cs,
                                         VarDecl *propertyWrapper, Type base,
                                         Type wrapper,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator())
      UseWrappedValue(cs, propertyWrapper, base, wrapper, locator);
}

bool UseSubscriptOperator::diagnose(bool asNote) const {
  auto failure = SubscriptMisuseFailure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

UseSubscriptOperator *UseSubscriptOperator::create(ConstraintSystem &cs,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) UseSubscriptOperator(cs, locator);
}

bool DefineMemberBasedOnUse::diagnose(bool asNote) const {
  auto failure = MissingMemberFailure(getConstraintSystem(), BaseType,
                                      Name, getLocator());
  return failure.diagnose(asNote);
}

DefineMemberBasedOnUse *
DefineMemberBasedOnUse::create(ConstraintSystem &cs, Type baseType,
                               DeclName member, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      DefineMemberBasedOnUse(cs, baseType, member, locator);
}

AllowMemberRefOnExistential *
AllowMemberRefOnExistential::create(ConstraintSystem &cs, Type baseType,
                                    ValueDecl *member, DeclName memberName,
                                    ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowMemberRefOnExistential(cs, baseType, memberName, member, locator);
}

bool AllowMemberRefOnExistential::diagnose(bool asNote) const {
  auto failure =
      InvalidMemberRefOnExistential(getConstraintSystem(), getBaseType(),
                                    getMemberName(), getLocator());
  return failure.diagnose(asNote);
}

bool AllowTypeOrInstanceMember::diagnose(bool asNote) const {
  auto failure = AllowTypeOrInstanceMemberFailure(
      getConstraintSystem(), getBaseType(), getMember(), getMemberName(),
      getLocator());
  return failure.diagnose(asNote);
}

AllowTypeOrInstanceMember *
AllowTypeOrInstanceMember::create(ConstraintSystem &cs, Type baseType,
                                  ValueDecl *member, DeclName usedName,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowTypeOrInstanceMember(cs, baseType, member, usedName, locator);
}

bool AllowInvalidPartialApplication::diagnose(bool asNote) const {
  auto failure = PartialApplicationFailure(isWarning(),
                                           getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AllowInvalidPartialApplication *
AllowInvalidPartialApplication::create(bool isWarning, ConstraintSystem &cs,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInvalidPartialApplication(isWarning, cs, locator);
}

bool AllowInvalidInitRef::diagnose(bool asNote) const {
  switch (Kind) {
  case RefKind::DynamicOnMetatype: {
    InvalidDynamicInitOnMetatypeFailure failure(
        getConstraintSystem(), BaseType, Init, BaseRange, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::ProtocolMetatype: {
    InitOnProtocolMetatypeFailure failure(getConstraintSystem(), BaseType,
                                          Init, IsStaticallyDerived, BaseRange,
                                          getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::NonConstMetatype: {
    ImplicitInitOnNonConstMetatypeFailure failure(getConstraintSystem(),
                                                  BaseType, Init, getLocator());
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

bool AllowClosureParamDestructuring::diagnose(bool asNote) const {
  ClosureParamDestructuringFailure failure(getConstraintSystem(),
                                           ContextualType, getLocator());
  return failure.diagnose(asNote);
}

AllowClosureParamDestructuring *
AllowClosureParamDestructuring::create(ConstraintSystem &cs,
                                       FunctionType *contextualType,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowClosureParamDestructuring(cs, contextualType, locator);
}

bool AddMissingArguments::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  MissingArgumentsFailure failure(cs, getSynthesizedArguments(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AddMissingArguments *
AddMissingArguments::create(ConstraintSystem &cs,
                            llvm::ArrayRef<Param> synthesizedArgs,
                            ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<Param>(synthesizedArgs.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(AddMissingArguments));
  return new (mem) AddMissingArguments(cs, synthesizedArgs, locator);
}

bool RemoveExtraneousArguments::diagnose(bool asNote) const {
  ExtraneousArgumentsFailure failure(getConstraintSystem(),
                                     ContextualType, getExtraArguments(),
                                     getLocator());
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
          decl->getFullName() == cs.getASTContext().Id_self) {
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

bool MoveOutOfOrderArgument::diagnose(bool asNote) const {
  OutOfOrderArgumentFailure failure(getConstraintSystem(), ArgIdx,
                                    PrevArgIdx, Bindings, getLocator());
  return failure.diagnose(asNote);
}

MoveOutOfOrderArgument *MoveOutOfOrderArgument::create(
    ConstraintSystem &cs, unsigned argIdx, unsigned prevArgIdx,
    ArrayRef<ParamBinding> bindings, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      MoveOutOfOrderArgument(cs, argIdx, prevArgIdx, bindings, locator);
}

bool AllowInaccessibleMember::diagnose(bool asNote) const {
  InaccessibleMemberFailure failure(getConstraintSystem(), getMember(),
                                    getLocator());
  return failure.diagnose(asNote);
}

AllowInaccessibleMember *
AllowInaccessibleMember::create(ConstraintSystem &cs, Type baseType,
                                ValueDecl *member, DeclName name,
                                ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInaccessibleMember(cs, baseType, member, name, locator);
}

bool AllowAnyObjectKeyPathRoot::diagnose(bool asNote) const {
  AnyObjectKeyPathRootFailure failure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AllowAnyObjectKeyPathRoot *
AllowAnyObjectKeyPathRoot::create(ConstraintSystem &cs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator()) AllowAnyObjectKeyPathRoot(cs, locator);
}

bool TreatKeyPathSubscriptIndexAsHashable::diagnose(bool asNote) const {
  KeyPathSubscriptIndexHashableFailure failure(getConstraintSystem(),
                                               NonConformingType, getLocator());
  return failure.diagnose(asNote);
}

TreatKeyPathSubscriptIndexAsHashable *
TreatKeyPathSubscriptIndexAsHashable::create(ConstraintSystem &cs, Type type,
                                             ConstraintLocator *locator) {
  return new (cs.getAllocator())
      TreatKeyPathSubscriptIndexAsHashable(cs, type, locator);
}

bool AllowInvalidRefInKeyPath::diagnose(bool asNote) const {
  switch (Kind) {
  case RefKind::StaticMember: {
    InvalidStaticMemberRefInKeyPath failure(getConstraintSystem(), Member,
                                            getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::MutatingGetter: {
    InvalidMemberWithMutatingGetterInKeyPath failure(
        getConstraintSystem(), Member, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::Method: {
    InvalidMethodRefInKeyPath failure(getConstraintSystem(), Member,
                                      getLocator());
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

bool RemoveAddressOf::diagnose(bool asNote) const {
  InvalidUseOfAddressOf failure(getConstraintSystem(), getFromType(),
                                getToType(), getLocator());
  return failure.diagnose(asNote);
}

RemoveAddressOf *RemoveAddressOf::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveAddressOf(cs, lhs, rhs, locator);
}

bool RemoveReturn::diagnose(bool asNote) const {
  ExtraneousReturnFailure failure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

RemoveReturn *RemoveReturn::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveReturn(cs, locator);
}

bool CollectionElementContextualMismatch::diagnose(bool asNote) const {
  CollectionElementContextualFailure failure(
      getConstraintSystem(), getFromType(), getToType(), getLocator());
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
    ArrayRef<ConstraintFix *> fixes, bool asNote) const {
  llvm::SmallVector<GenericTypeParamType *, 4> missingParams{Param};

  for (auto *otherFix : fixes) {
    if (auto *fix = otherFix->getAs<DefaultGenericArgument>())
      missingParams.push_back(fix->Param);
  }

  auto &cs = getConstraintSystem();
  MissingGenericArgumentsFailure failure(cs, missingParams, getLocator());
  return failure.diagnose(asNote);
}

bool DefaultGenericArgument::diagnose(bool asNote) const {
  return coalesceAndDiagnose({}, asNote);
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

bool SkipUnhandledConstructInFunctionBuilder::diagnose(bool asNote) const {
  SkipUnhandledConstructInFunctionBuilderFailure failure(
      getConstraintSystem(), unhandled, builder, getLocator());
  return failure.diagnose(asNote);
}

bool AllowMutatingMemberOnRValueBase::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  MutatingMemberRefOnImmutableBase failure(cs, getMember(), getLocator());
  return failure.diagnose(asNote);
}

AllowMutatingMemberOnRValueBase *
AllowMutatingMemberOnRValueBase::create(ConstraintSystem &cs, Type baseType,
                                        ValueDecl *member, DeclName name,
                                        ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowMutatingMemberOnRValueBase(cs, baseType, member, name, locator);
}

bool AllowTupleSplatForSingleParameter::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  InvalidTupleSplatWithSingleParameterFailure failure(cs, ParamType,
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

bool DropThrowsAttribute::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  ThrowingFunctionConversionFailure failure(cs, getFromType(),
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

bool IgnoreContextualType::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  ContextualFailure failure(cs, getFromType(), getToType(), getLocator());
  return failure.diagnose(asNote);
}

IgnoreContextualType *IgnoreContextualType::create(ConstraintSystem &cs,
                                                   Type resultTy,
                                                   Type specifiedTy,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator())
      IgnoreContextualType(cs, resultTy, specifiedTy, locator);
}

bool IgnoreAssignmentDestinationType::diagnose(bool asNote) const {
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
      cs, CTP, getFromType(), getToType(),
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

bool AllowInOutConversion::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  InOutConversionFailure failure(cs, getFromType(), getToType(),
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
                                ConstraintLocatorBuilder locator) {
  auto constraintLocator = cs.getConstraintLocator(locator);
  auto elementType = cs.isArrayType(argType);
  if (elementType &&
      constraintLocator->getLastElementAs<LocatorPathElt::ApplyArgToParam>()
          ->getParameterFlags()
          .isVariadic()) {
    auto options = ConstraintSystem::TypeMatchOptions(
        ConstraintSystem::TypeMatchFlags::TMF_ApplyingFix |
        ConstraintSystem::TypeMatchFlags::TMF_GenerateConstraints);
    auto result =
        cs.matchTypes(*elementType, paramType,
                      ConstraintKind::ArgumentConversion, options, locator);
    if (result.isSuccess())
      return new (cs.getAllocator())
          ExpandArrayIntoVarargs(cs, argType, paramType, constraintLocator);
  }

  return nullptr;
}

bool ExpandArrayIntoVarargs::diagnose(bool asNote) const {
  ExpandArrayIntoVarargsFailure failure(
      getConstraintSystem(), getFromType(), getToType(), getLocator());
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

bool AllowArgumentMismatch::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  ArgumentMismatchFailure failure(cs, getFromType(), getToType(),
                                  getLocator());
  return failure.diagnose(asNote);
}

AllowArgumentMismatch *
AllowArgumentMismatch::create(ConstraintSystem &cs, Type argType,
                              Type paramType, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowArgumentMismatch(cs, argType, paramType, locator);
}

bool RemoveInvalidCall::diagnose(bool asNote) const {
  ExtraneousCallFailure failure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

RemoveInvalidCall *RemoveInvalidCall::create(ConstraintSystem &cs,
                                             ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveInvalidCall(cs, locator);
}

bool AllowInvalidUseOfTrailingClosure::diagnose(bool asNote) const {
  auto &cs = getConstraintSystem();
  InvalidUseOfTrailingClosure failure(cs, getFromType(), getToType(),
                                      getLocator());
  return failure.diagnose(asNote);
}

AllowInvalidUseOfTrailingClosure *
AllowInvalidUseOfTrailingClosure::create(ConstraintSystem &cs, Type argType,
                                         Type paramType,
                                         ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInvalidUseOfTrailingClosure(cs, argType, paramType, locator);
}

bool TreatEphemeralAsNonEphemeral::diagnose(bool asNote) const {
  NonEphemeralConversionFailure failure(
      getConstraintSystem(), getLocator(), getFromType(), getToType(),
      ConversionKind, isWarning());
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
