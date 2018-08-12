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
#include "llvm/Support/raw_ostream.h"

using namespace swift;
using namespace constraints;

ConstraintFix::~ConstraintFix() {}

Expr *ConstraintFix::getAnchor() const { return getLocator()->getAnchor(); }

void ConstraintFix::dump() const { print(llvm::errs()); }

bool ForceDowncast::diagnose(Expr *expr, const Solution &solution) const {
  MissingExplicitConversionFailure failure(expr, solution, getLocator(),
                                           DowncastTo);
  return failure.diagnose();
}

void ForceDowncast::print(llvm::raw_ostream &Out) const {
  Out << "[fix: force downcast (as! " << DowncastTo->getString() << ")]";
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
  bool resultIsOptional = false; // FIXME: figure out a way to implement this.
  MemberAccessOnOptionalBaseFailure failure(root, solution, getLocator(),
                                            MemberName, resultIsOptional);
  return failure.diagnose();
}

UnwrapOptionalBase *UnwrapOptionalBase::create(ConstraintSystem &cs,
                                               DeclName member,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) UnwrapOptionalBase(member, locator);
}

bool AddAddressOf::diagnose(Expr *root, const Solution &solution) const {
  MissingAddressOfFailure failure(root, solution, getLocator());
  return failure.diagnose();
}

AddAddressOf *AddAddressOf::create(ConstraintSystem &cs,
                                   ConstraintLocator *locator) {
  return new (cs.getAllocator()) AddAddressOf(locator);
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
  LabelingFailure failure(solution, getLocator(), CorrectLabels);
  return failure.diagnose();
}

RelabelArguments *
RelabelArguments::create(ConstraintSystem &cs,
                         llvm::ArrayRef<Identifier> correctLabels,
                         ConstraintLocator *locator) {
  return new (cs.getAllocator()) RelabelArguments(correctLabels, locator);
}

bool MissingConformance::diagnose(Expr *root, const Solution &solution) const {
  MissingConformanceFailure failure(root, solution, getLocator(),
                                    {NonConformingType.getPointer(), Protocol});
  return failure.diagnose();
}

MissingConformance *MissingConformance::create(ConstraintSystem &cs, Type type,
                                               ProtocolDecl *protocol,
                                               ConstraintLocator *locator) {
  return new (cs.getAllocator()) MissingConformance(type, protocol, locator);
}
