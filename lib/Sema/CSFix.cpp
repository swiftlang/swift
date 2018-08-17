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
#include "swift/AST/Expr.h"
#include "swift/AST/Type.h"
#include "swift/Basic/SourceManager.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/Support/raw_ostream.h"
#include <string>

using namespace swift;
using namespace constraints;

ConstraintFix::~ConstraintFix() {}

Expr *ConstraintFix::getAnchor() const { return getLocator()->getAnchor(); }

void ConstraintFix::print(llvm::raw_ostream &Out, SourceManager *sm) const {
  Out << "[fix: ";
  Out << getName();
  Out << ']';
  Out << " @ ";
  getLocator()->dump(sm, Out);
}

void ConstraintFix::dump(SourceManager *sm) const { print(llvm::errs(), sm); }

std::string ForceDowncast::getName() const {
  llvm::SmallString<16> name;
  name += "force downcast (as! ";
  name += DowncastTo->getString();
  name += ")";
  return name.c_str();
}

bool ForceDowncast::diagnose(Expr *expr, const Solution &solution) const {
  MissingExplicitConversionFailure failure(expr, solution, getLocator(),
                                           DowncastTo);
  return failure.diagnose();
}

ForceDowncast *ForceDowncast::create(ConstraintSystem &cs, Type toType,
                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceDowncast(toType, locator);
}

bool ForceOptional::diagnose(Expr *root, const Solution &solution) const {
  MissingOptionalUnwrapFailure failure(root, solution, getLocator());
  return failure.diagnose();
}

ForceOptional *ForceOptional::create(ConstraintSystem &cs,
                                     ConstraintLocator *locator) {
  return new (cs.getAllocator()) ForceOptional(locator);
}

bool UnwrapOptionalBase::diagnose(Expr *root, const Solution &solution) const {
  bool resultIsOptional =
      getKind() == FixKind::UnwrapOptionalBaseWithOptionalResult;
  MemberAccessOnOptionalBaseFailure failure(root, solution, getLocator(),
                                            MemberName, resultIsOptional);
  return failure.diagnose();
}

UnwrapOptionalBase *UnwrapOptionalBase::create(ConstraintSystem &cs,
                                               DeclName member,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator())
      UnwrapOptionalBase(FixKind::UnwrapOptionalBase, member, locator);
}

UnwrapOptionalBase *UnwrapOptionalBase::createWithOptionalResult(
    ConstraintSystem &cs, DeclName member, ConstraintLocator *locator) {
  return new (cs.getAllocator()) UnwrapOptionalBase(
      FixKind::UnwrapOptionalBaseWithOptionalResult, member, locator);
}

bool AddAddressOf::diagnose(Expr *root, const Solution &solution) const {
  MissingAddressOfFailure failure(root, solution, getLocator());
  return failure.diagnose();
}

AddAddressOf *AddAddressOf::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddAddressOf(locator);
}

bool TreatRValueAsLValue::diagnose(Expr *root, const Solution &solution) const {
  RValueTreatedAsLValueFailure failure(solution, getLocator());
  return failure.diagnose();
}

TreatRValueAsLValue *TreatRValueAsLValue::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) TreatRValueAsLValue(locator);
}


bool CoerceToCheckedCast::diagnose(Expr *root, const Solution &solution) const {
  MissingForcedDowncastFailure failure(root, solution, getLocator());
  return failure.diagnose();
}

CoerceToCheckedCast *CoerceToCheckedCast::create(ConstraintSystem &cs,
                                                 ConstraintLocator *locator) {
  return new (cs.getAllocator()) CoerceToCheckedCast(locator);
}

bool MarkExplicitlyEscaping::diagnose(Expr *root,
                                      const Solution &solution) const {
  NoEscapeFuncToTypeConversionFailure failure(root, solution, getLocator(),
                                              ConvertTo);
  return failure.diagnose();
}

MarkExplicitlyEscaping *
MarkExplicitlyEscaping::create(ConstraintSystem &cs, ConstraintLocator *locator,
                               Type convertingTo) {
  return new (cs.getAllocator()) MarkExplicitlyEscaping(locator, convertingTo);
}

bool RelabelArguments::diagnose(Expr *root, const Solution &solution) const {
  LabelingFailure failure(solution, getLocator(), getLabels());
  return failure.diagnose();
}

RelabelArguments *
RelabelArguments::create(ConstraintSystem &cs,
                         llvm::ArrayRef<Identifier> correctLabels,
                         ConstraintLocator *locator) {
  unsigned size = totalSizeToAlloc<Identifier>(correctLabels.size());
  void *mem = cs.getAllocator().Allocate(size, alignof(RelabelArguments));
  return new (mem) RelabelArguments(correctLabels, locator);
}

bool MissingConformance::diagnose(Expr *root, const Solution &solution) const {
  MissingConformanceFailure failure(root, solution, getLocator(),
                                    {NonConformingType, Protocol});
  return failure.diagnose();
}

MissingConformance *MissingConformance::create(ConstraintSystem &cs, Type type,
                                               ProtocolDecl *protocol,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) MissingConformance(type, protocol, locator);
}
