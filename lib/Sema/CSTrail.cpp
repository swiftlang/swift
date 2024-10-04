//===--- CSTrail.cpp -  Tracking changes that can be undone ---------------===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the \c SolverTrail class, which records the decisions taken
// while attempting to find a solution.
//
//===----------------------------------------------------------------------===//

#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintSystem.h"
#include "swift/Sema/CSTrail.h"
#include "swift/Basic/Assertions.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Debug.h"
#include "llvm/Support/SaveAndRestore.h"
#include <algorithm>
#include <memory>
#include <numeric>

using namespace swift;
using namespace constraints;

#define DEBUG_TYPE "SolverTrail"

SolverTrail::SolverTrail(ConstraintSystem &cs)
  : CS(cs) {
  for (unsigned i = 0; i <= unsigned(ChangeKind::Last); ++i) {
    Profile[i] = 0;
  }
}

SolverTrail::~SolverTrail() {
  // If constraint system is in an invalid state, it's
  // possible that constraint graph is corrupted as well
  // so let's not attempt to check change log.
  if (!CS.inInvalidState())
    ASSERT(Changes.empty() && "Trail corrupted");
}

#define LOCATOR_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(ConstraintLocator *locator) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.Locator = locator; \
    return result; \
  }
#include "swift/Sema/CSTrail.def"

SolverTrail::Change
SolverTrail::Change::AddedTypeVariable(TypeVariableType *typeVar) {
  Change result;
  result.Kind = ChangeKind::AddedTypeVariable;
  result.TypeVar = typeVar;
  return result;
}

SolverTrail::Change
SolverTrail::Change::AddedConstraint(TypeVariableType *typeVar,
                                     Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::AddedConstraint;
  result.TheConstraint.TypeVar = typeVar;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RemovedConstraint(TypeVariableType *typeVar,
                                       Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::RemovedConstraint;
  result.TheConstraint.TypeVar = typeVar;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::ExtendedEquivalenceClass(TypeVariableType *typeVar,
                                              unsigned prevSize) {
  Change result;
  result.Kind = ChangeKind::ExtendedEquivalenceClass;
  result.EquivClass.TypeVar = typeVar;
  result.EquivClass.PrevSize = prevSize;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RelatedTypeVariables(TypeVariableType *typeVar,
                                          TypeVariableType *otherTypeVar) {
  Change result;
  result.Kind = ChangeKind::RelatedTypeVariables;
  result.Relation.TypeVar = typeVar;
  result.Relation.OtherTypeVar = otherTypeVar;
  return result;
}

SolverTrail::Change
SolverTrail::Change::InferredBindings(TypeVariableType *typeVar,
                                     Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::InferredBindings;
  result.TheConstraint.TypeVar = typeVar;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RetractedBindings(TypeVariableType *typeVar,
                                       Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::RetractedBindings;
  result.TheConstraint.TypeVar = typeVar;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::UpdatedTypeVariable(
    TypeVariableType *typeVar,
    llvm::PointerUnion<TypeVariableType *, TypeBase *> parentOrFixed,
	  unsigned options) {
  Change result;
  result.Kind = ChangeKind::UpdatedTypeVariable;
  result.Update.TypeVar = typeVar;
  result.Update.ParentOrFixed = parentOrFixed;
  result.Options = options;
  return result;
}

SolverTrail::Change
SolverTrail::Change::AddedConversionRestriction(Type srcType, Type dstType) {
  Change result;
  result.Kind = ChangeKind::AddedConversionRestriction;
  result.Restriction.SrcType = srcType;
  result.Restriction.DstType = dstType;
  return result;
}

SolverTrail::Change
SolverTrail::Change::AddedFix(ConstraintFix *fix) {
  Change result;
  result.Kind = ChangeKind::AddedFix;
  result.Fix = fix;
  return result;
}

SolverTrail::Change
SolverTrail::Change::AddedFixedRequirement(GenericTypeParamType *GP,
                                           unsigned reqKind,
                                           Type reqTy) {
  Change result;
  result.Kind = ChangeKind::AddedFixedRequirement;
  result.FixedRequirement.GP = GP;
  result.FixedRequirement.ReqTy = reqTy;
  result.Options = reqKind;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedDisjunctionChoice(ConstraintLocator *locator,
                                               unsigned index) {
  Change result;
  result.Kind = ChangeKind::RecordedDisjunctionChoice;
  result.Locator = locator;
  result.Options = index;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedOpenedPackExpansionType(PackExpansionType *expansionTy) {
  Change result;
  result.Kind = ChangeKind::RecordedOpenedPackExpansionType;
  result.ExpansionTy = expansionTy;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedPackEnvironment(PackElementExpr *packElement) {
  Change result;
  result.Kind = ChangeKind::RecordedPackEnvironment;
  result.ElementExpr = packElement;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedNodeType(ASTNode node, Type oldType) {
  Change result;
  result.Kind = ChangeKind::RecordedNodeType;
  result.Node.Node = node;
  result.Node.OldType = oldType;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedKeyPathComponentType(const KeyPathExpr *expr,
                                                  unsigned component,
                                                  Type oldType) {
  Change result;
  result.Kind = ChangeKind::RecordedKeyPathComponentType;
  result.Options = component;
  result.KeyPath.Expr = expr;
  result.KeyPath.OldType = oldType;
  return result;
}

SolverTrail::Change
SolverTrail::Change::DisabledConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::DisabledConstraint;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::FavoredConstraint(Constraint *constraint) {
  Change result;
  result.Kind = ChangeKind::FavoredConstraint;
  result.TheConstraint.Constraint = constraint;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedResultBuilderTransform(AnyFunctionRef fn) {
  Change result;
  result.Kind = ChangeKind::RecordedResultBuilderTransform;
  result.AFR = fn;
  return result;
}

SolverTrail::Change
SolverTrail::Change::AppliedPropertyWrapper(Expr *expr) {
  Change result;
  result.Kind = ChangeKind::AppliedPropertyWrapper;
  result.TheExpr = expr;
  return result;
}

void SolverTrail::Change::undo(ConstraintSystem &cs) const {
  auto &cg = cs.getConstraintGraph();

  switch (Kind) {
  case ChangeKind::AddedTypeVariable:
    cg.removeNode(TypeVar);
    break;

  case ChangeKind::AddedConstraint:
    cg.removeConstraint(TheConstraint.TypeVar, TheConstraint.Constraint);
    break;

  case ChangeKind::RemovedConstraint:
    cg.addConstraint(TheConstraint.TypeVar, TheConstraint.Constraint);
    break;

  case ChangeKind::ExtendedEquivalenceClass: {
    auto &node = cg[EquivClass.TypeVar];
    node.truncateEquivalenceClass(EquivClass.PrevSize);
    break;
   }

  case ChangeKind::RelatedTypeVariables:
    cg.unrelateTypeVariables(Relation.TypeVar, Relation.OtherTypeVar);
    break;

  case ChangeKind::InferredBindings:
    cg.retractBindings(TheConstraint.TypeVar, TheConstraint.Constraint);
    break;

  case ChangeKind::RetractedBindings:
    cg.inferBindings(TheConstraint.TypeVar, TheConstraint.Constraint);
    break;

  case ChangeKind::UpdatedTypeVariable:
    Update.TypeVar->getImpl().setRawOptions(Options);
    Update.TypeVar->getImpl().ParentOrFixed = Update.ParentOrFixed;
    break;

  case ChangeKind::AddedConversionRestriction:
    cs.removeConversionRestriction(Restriction.SrcType,
                                   Restriction.DstType);
    break;

  case ChangeKind::AddedFix:
    cs.removeFix(Fix);
    break;

  case ChangeKind::AddedFixedRequirement:
    cs.removeFixedRequirement(FixedRequirement.GP, Options,
                              FixedRequirement.ReqTy);
    break;

  case ChangeKind::RecordedDisjunctionChoice:
    cs.removeDisjunctionChoice(Locator);
    break;

  case ChangeKind::RecordedAppliedDisjunction:
    cs.removeAppliedDisjunction(Locator);
    break;

  case ChangeKind::RecordedMatchCallArgumentResult:
    cs.removeMatchCallArgumentResult(Locator);
    break;

  case ChangeKind::RecordedOpenedTypes:
    cs.removeOpenedType(Locator);
    break;

  case ChangeKind::RecordedOpenedExistentialType:
    cs.removeOpenedExistentialType(Locator);
    break;

  case ChangeKind::RecordedOpenedPackExpansionType:
    cs.removeOpenedPackExpansionType(ExpansionTy);
    break;

  case ChangeKind::RecordedPackExpansionEnvironment:
    cs.removePackExpansionEnvironment(Locator);
    break;

  case ChangeKind::RecordedPackEnvironment:
    cs.removePackEnvironment(ElementExpr);
    break;

  case ChangeKind::RecordedDefaultedConstraint:
    cs.removeDefaultedConstraint(Locator);
    break;

  case ChangeKind::RecordedNodeType:
    cs.restoreType(Node.Node, Node.OldType);
    break;

  case ChangeKind::RecordedKeyPathComponentType:
    cs.restoreType(KeyPath.Expr, Options, KeyPath.OldType);
    break;

  case ChangeKind::DisabledConstraint:
    TheConstraint.Constraint->setEnabled();
    break;

  case ChangeKind::FavoredConstraint:
    ASSERT(TheConstraint.Constraint->isFavored());
    TheConstraint.Constraint->setFavored(false);
    break;

  case ChangeKind::RecordedResultBuilderTransform:
    cs.removeResultBuilderTransform(AFR);
    break;

  case ChangeKind::AppliedPropertyWrapper:
    cs.removePropertyWrapper(TheExpr);
    break;

  case ChangeKind::ResolvedOverload:
    cs.removeResolvedOverload(Locator);
    break;
  }
}

void SolverTrail::Change::dump(llvm::raw_ostream &out,
                               ConstraintSystem &cs,
                               unsigned indent) const {
  PrintOptions PO;
  PO.PrintTypesForDebugging = true;

  out.indent(indent);

  switch (Kind) {

#define LOCATOR_CHANGE(Name) \
  case ChangeKind::Name: \
    out << "(" << #Name << " at "; \
    Locator->dump(&cs.getASTContext().SourceMgr, out); \
    out << ")\n"; \
    break;
#include "swift/Sema/CSTrail.def"

  case ChangeKind::AddedTypeVariable:
    out << "(AddedTypeVariable ";
    TypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::AddedConstraint:
    out << "(AddedConstraint ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                         indent + 2);
    out << " to type variable ";
    TheConstraint.TypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::RemovedConstraint:
    out << "(RemovedConstraint ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                                    indent + 2);
    out << " from type variable ";
    TheConstraint.TypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::ExtendedEquivalenceClass: {
    out << "(ExtendedEquivalenceClass ";
    EquivClass.TypeVar->print(out, PO);
    out << " " << EquivClass.PrevSize << ")\n";
    break;
   }

  case ChangeKind::RelatedTypeVariables:
    out << "(RelatedTypeVariables ";
    Relation.TypeVar->print(out, PO);
    out << " with ";
    Relation.OtherTypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::InferredBindings:
    out << "(InferredBindings from ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                         indent + 2);
    out << " for type variable ";
    TheConstraint.TypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::RetractedBindings:
    out << "(RetractedBindings from ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                                    indent + 2);
    out << " for type variable ";
    TheConstraint.TypeVar->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::UpdatedTypeVariable: {
    out << "(UpdatedTypeVariable ";
    Update.TypeVar->print(out, PO);

    auto parentOrFixed = Update.TypeVar->getImpl().ParentOrFixed;
    if (auto *parent = parentOrFixed.dyn_cast<TypeVariableType *>()) {
      out << " to parent ";
      parent->print(out, PO);
    }
    else {
      out << " to fixed type ";
      parentOrFixed.get<TypeBase *>()->print(out, PO);
    }
    out << " with options 0x";
    out.write_hex(Options);
    out << ")\n";
    break;
  }

  case ChangeKind::AddedConversionRestriction:
    out << "(AddedConversionRestriction with source ";
    Restriction.SrcType->print(out, PO);
    out << " and destination ";
    Restriction.DstType->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::AddedFix:
    out << "(AddedFix ";
    Fix->print(out);
    out << ")\n";
    break;

  case ChangeKind::AddedFixedRequirement:
    out << "(AddedFixedRequirement ";
    FixedRequirement.GP->print(out, PO);
    out << " kind ";
    out << Options << " ";
    FixedRequirement.ReqTy->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::RecordedDisjunctionChoice:
    out << "(RecordedDisjunctionChoice at ";
    Locator->dump(&cs.getASTContext().SourceMgr, out);
    out << " index ";
    out << Options << ")\n";
    break;

  case ChangeKind::RecordedOpenedPackExpansionType:
    out << "(RecordedOpenedPackExpansionType for ";
    ExpansionTy->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::RecordedPackEnvironment:
    // FIXME: Print short form of PackExpansionExpr
    out << "(RecordedPackEnvironment ";
    simple_display(out, ElementExpr);
    out << "\n";
    break;

  case ChangeKind::RecordedNodeType:
    out << "(RecordedNodeType at ";
    Node.Node.getStartLoc().print(out, cs.getASTContext().SourceMgr);
    out << " previous ";
    if (Node.OldType)
      Node.OldType->print(out, PO);
    else
      out << "null";
    out << ")\n";
    break;

  case ChangeKind::RecordedKeyPathComponentType:
    out << "(RecordedKeyPathComponentType ";
    simple_display(out, KeyPath.Expr);
    out << " with component type ";
    if (Node.OldType)
      Node.OldType->print(out, PO);
    else
      out << "null";
    out << " for component " << Options << ")\n";
    break;

  case ChangeKind::DisabledConstraint:
    out << "(DisabledConstraint ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                                    indent + 2);
    out << ")\n";
    break;

  case ChangeKind::FavoredConstraint:
    out << "(FavoredConstraint ";
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr,
                                    indent + 2);
    out << ")\n";
    break;

  case ChangeKind::RecordedResultBuilderTransform:
    out << "(RecordedResultBuilderTransform ";
    simple_display(out, AFR);
    out << ")\n";
    break;

  case ChangeKind::AppliedPropertyWrapper:
    out << "(AppliedPropertyWrapper ";
    simple_display(out, TheExpr);
    out << ")\n";
    break;
  }
}

void SolverTrail::recordChange(Change change) {
  LLVM_DEBUG(llvm::dbgs() << "+ "; change.dump(llvm::dbgs(), CS, 0););
  ASSERT(!UndoActive);

  Changes.push_back(change);

  ++Profile[unsigned(change.Kind)];
  ++Total;
  if (Changes.size() > Max)
    Max = Changes.size();
}

void SolverTrail::undo(unsigned toIndex) {
  // Don't attempt to rollback if constraint system ended up
  // in an invalid state.
  if (CS.inInvalidState())
    return;

  auto dumpHistogram = [&]() {
#define CHANGE(Name) \
    if (auto count = Profile[unsigned(ChangeKind::Name)]) \
      llvm::dbgs() << "* " << #Name << ": " << count << "\n";
#include "swift/Sema/CSTrail.def"
  };

  LLVM_DEBUG(llvm::dbgs() << "decisions " << Changes.size()
                          << " max " << Max
                          << " total " << Total << "\n";
             dumpHistogram();
             llvm::dbgs() << "\n");

  ASSERT(Changes.size() >= toIndex && "Trail corrupted");
  ASSERT(!UndoActive);
  UndoActive = true;

  // FIXME: Undo all changes in the correct order!
  for (unsigned i = Changes.size(); i > toIndex; i--) {
    auto change = Changes[i - 1];
    if (change.Kind == ChangeKind::UpdatedTypeVariable) {
      LLVM_DEBUG(llvm::dbgs() << "- "; change.dump(llvm::dbgs(), CS, 0));
      change.undo(CS);
    }
  }

  for (unsigned i = Changes.size(); i > toIndex; i--) {
    auto change = Changes[i - 1];
    if (change.Kind != ChangeKind::UpdatedTypeVariable) {
      LLVM_DEBUG(llvm::dbgs() << "- "; change.dump(llvm::dbgs(), CS, 0));
      change.undo(CS);
    }
  }

  Changes.resize(toIndex);
  UndoActive = false;
}

void SolverTrail::dumpActiveScopeChanges(llvm::raw_ostream &out,
                                         unsigned fromIndex,
                                         unsigned indent) const {
  out.indent(indent);
  out << "(changes:\n";

  for (unsigned i = fromIndex; i < Changes.size(); ++i)
    Changes[i].dump(out, CS, indent + 2);

  out.indent(indent);
  out << ")\n";
}