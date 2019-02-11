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
  name += "force downcast (as! ";
  name += DowncastTo->getString();
  name += ")";
  return name.c_str();
}

bool ForceDowncast::diagnose(Expr *expr, bool asNote) const {
  MissingExplicitConversionFailure failure(expr, getConstraintSystem(),
                                           getLocator(), DowncastTo);
  return failure.diagnose(asNote);
}

ForceDowncast *ForceDowncast::create(ConstraintSystem &cs, Type toType,
                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceDowncast(cs, toType, locator);
}

bool ForceOptional::diagnose(Expr *root, bool asNote) const {
  MissingOptionalUnwrapFailure failure(root, getConstraintSystem(), BaseType,
                                       UnwrappedType, getLocator());
  return failure.diagnose(asNote);
}

ForceOptional *ForceOptional::create(ConstraintSystem &cs, Type baseType,
                                     Type unwrappedType,
                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceOptional(
      cs, baseType, unwrappedType,
      cs.getConstraintLocator(simplifyLocatorToAnchor(cs, locator)));
}

bool UnwrapOptionalBase::diagnose(Expr *root, bool asNote) const {
  bool resultIsOptional =
      getKind() == FixKind::UnwrapOptionalBaseWithOptionalResult;
  MemberAccessOnOptionalBaseFailure failure(
      root, getConstraintSystem(), getLocator(), MemberName, resultIsOptional);
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

bool AddAddressOf::diagnose(Expr *root, bool asNote) const {
  MissingAddressOfFailure failure(root, getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AddAddressOf *AddAddressOf::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddAddressOf(cs, locator);
}

bool TreatRValueAsLValue::diagnose(Expr *root, bool asNote) const {
  RValueTreatedAsLValueFailure failure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

TreatRValueAsLValue *TreatRValueAsLValue::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) TreatRValueAsLValue(cs, locator);
}

bool CoerceToCheckedCast::diagnose(Expr *root, bool asNote) const {
  MissingForcedDowncastFailure failure(root, getConstraintSystem(),
                                       getLocator());
  return failure.diagnose(asNote);
}

CoerceToCheckedCast *CoerceToCheckedCast::create(ConstraintSystem &cs,
                                                 ConstraintLocator *locator) {
  return new (cs.getAllocator()) CoerceToCheckedCast(cs, locator);
}

bool MarkExplicitlyEscaping::diagnose(Expr *root, bool asNote) const {
  NoEscapeFuncToTypeConversionFailure failure(root, getConstraintSystem(),
                                              getLocator(), ConvertTo);
  return failure.diagnose(asNote);
}

MarkExplicitlyEscaping *
MarkExplicitlyEscaping::create(ConstraintSystem &cs, ConstraintLocator *locator,
                               Type convertingTo) {
  return new (cs.getAllocator())
      MarkExplicitlyEscaping(cs, locator, convertingTo);
}

bool RelabelArguments::diagnose(Expr *root, bool asNote) const {
  LabelingFailure failure(getConstraintSystem(), getLocator(), getLabels());
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

bool MissingConformance::diagnose(Expr *root, bool asNote) const {
  MissingConformanceFailure failure(root, getConstraintSystem(), getLocator(),
                                    {NonConformingType, Protocol});
  return failure.diagnose(asNote);
}

MissingConformance *MissingConformance::create(ConstraintSystem &cs, Type type,
                                               ProtocolDecl *protocol,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      MissingConformance(cs, type, protocol, locator);
}

bool SkipSameTypeRequirement::diagnose(Expr *root, bool asNote) const {
  SameTypeRequirementFailure failure(root, getConstraintSystem(), LHS, RHS,
                                     getLocator());
  return failure.diagnose(asNote);
}

SkipSameTypeRequirement *
SkipSameTypeRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                ConstraintLocator *locator) {
  return new (cs.getAllocator()) SkipSameTypeRequirement(cs, lhs, rhs, locator);
}

bool SkipSuperclassRequirement::diagnose(Expr *root, bool asNote) const {
  SuperclassRequirementFailure failure(root, getConstraintSystem(), LHS, RHS,
                                       getLocator());
  return failure.diagnose(asNote);
}

SkipSuperclassRequirement *
SkipSuperclassRequirement::create(ConstraintSystem &cs, Type lhs, Type rhs,
                                  ConstraintLocator *locator) {
  return new (cs.getAllocator())
      SkipSuperclassRequirement(cs, lhs, rhs, locator);
}

bool ContextualMismatch::diagnose(Expr *root, bool asNote) const {
  auto failure = ContextualFailure(root, getConstraintSystem(), getFromType(),
                                   getToType(), getLocator());
  return failure.diagnose(asNote);
}

ContextualMismatch *ContextualMismatch::create(ConstraintSystem &cs, Type lhs,
                                               Type rhs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) ContextualMismatch(cs, lhs, rhs, locator);
}

bool AutoClosureForwarding::diagnose(Expr *root, bool asNote) const {
  auto failure =
      AutoClosureForwardingFailure(getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AutoClosureForwarding *AutoClosureForwarding::create(ConstraintSystem &cs,
                                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) AutoClosureForwarding(cs, locator);
}

bool RemoveUnwrap::diagnose(Expr *root, bool asNote) const {
  auto failure = NonOptionalUnwrapFailure(root, getConstraintSystem(), BaseType,
                                          getLocator());
  return failure.diagnose(asNote);
}

RemoveUnwrap *RemoveUnwrap::create(ConstraintSystem &cs, Type baseType,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) RemoveUnwrap(cs, baseType, locator);
}

bool InsertExplicitCall::diagnose(Expr *root, bool asNote) const {
  auto failure = MissingCallFailure(root, getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

InsertExplicitCall *InsertExplicitCall::create(ConstraintSystem &cs,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) InsertExplicitCall(cs, locator);
}

bool UseSubscriptOperator::diagnose(Expr *root, bool asNote) const {
  auto failure = SubscriptMisuseFailure(root, getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

UseSubscriptOperator *UseSubscriptOperator::create(ConstraintSystem &cs,
                                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) UseSubscriptOperator(cs, locator);
}

bool DefineMemberBasedOnUse::diagnose(Expr *root, bool asNote) const {
  auto failure = MissingMemberFailure(root, getConstraintSystem(), BaseType,
                                      Name, getLocator());
  return failure.diagnose(asNote);
}

DefineMemberBasedOnUse *
DefineMemberBasedOnUse::create(ConstraintSystem &cs, Type baseType,
                               DeclName member, ConstraintLocator *locator) {
  return new (cs.getAllocator())
      DefineMemberBasedOnUse(cs, baseType, member, locator);
}

bool AllowInvalidPartialApplication::diagnose(Expr *root, bool asNote) const {
  auto failure = PartialApplicationFailure(root, isWarning(),
                                           getConstraintSystem(), getLocator());
  return failure.diagnose(asNote);
}

AllowInvalidPartialApplication *
AllowInvalidPartialApplication::create(bool isWarning, ConstraintSystem &cs,
                                       ConstraintLocator *locator) {
  return new (cs.getAllocator())
      AllowInvalidPartialApplication(isWarning, cs, locator);
}

bool AllowInvalidInitRef::diagnose(Expr *root, bool asNote) const {
  switch (Kind) {
  case RefKind::DynamicOnMetatype: {
    InvalidDynamicInitOnMetatypeFailure failure(
        root, getConstraintSystem(), BaseType, Init, BaseRange, getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::ProtocolMetatype: {
    InitOnProtocolMetatypeFailure failure(root, getConstraintSystem(), BaseType,
                                          Init, IsStaticallyDerived, BaseRange,
                                          getLocator());
    return failure.diagnose(asNote);
  }

  case RefKind::NonConstMetatype: {
    ImplicitInitOnNonConstMetatypeFailure failure(root, getConstraintSystem(),
                                                  BaseType, Init, getLocator());
    return failure.diagnose(asNote);
  }
  }
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
