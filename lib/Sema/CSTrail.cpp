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

#define LOCATOR_CHANGE(Name, _) \
  SolverTrail::Change \
  SolverTrail::Change::Name(ConstraintLocator *locator) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.TheLocator = locator; \
    return result; \
  }
#define EXPR_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(Expr *expr) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.TheExpr = expr; \
    return result; \
  }
#define CLOSURE_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(ClosureExpr *closure) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.TheClosure = closure; \
    return result; \
  }
#define CONSTRAINT_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(Constraint *constraint) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.TheConstraint.Constraint = constraint; \
    return result; \
  }
#define GRAPH_NODE_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(TypeVariableType *typeVar, \
                            Constraint *constraint) { \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.TheConstraint.TypeVar = typeVar; \
    result.TheConstraint.Constraint = constraint; \
    return result; \
  }
#define SCORE_CHANGE(Name) \
  SolverTrail::Change \
  SolverTrail::Change::Name(ScoreKind kind, unsigned value) { \
    ASSERT(value <= 0xffffff && "value must fit in 24 bits"); \
    Change result; \
    result.Kind = ChangeKind::Name; \
    result.Options = unsigned(kind) | (value << 8); \
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
  result.TheFix = fix;
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
SolverTrail::Change::RecordedOpenedPackExpansionType(PackExpansionType *expansionTy) {
  Change result;
  result.Kind = ChangeKind::RecordedOpenedPackExpansionType;
  result.TheExpansion = expansionTy;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedPackEnvironment(PackElementExpr *packElement) {
  Change result;
  result.Kind = ChangeKind::RecordedPackEnvironment;
  result.TheElement = packElement;
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
SolverTrail::Change::RecordedResultBuilderTransform(AnyFunctionRef fn) {
  Change result;
  result.Kind = ChangeKind::RecordedResultBuilderTransform;
  result.TheRef = fn;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedContextualInfo(ASTNode node) {
  Change result;
  result.Kind = ChangeKind::RecordedContextualInfo;
  result.Node.Node = node;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedTarget(SyntacticElementTargetKey key) {
  Change result;
  result.Kind = ChangeKind::RecordedTarget;
  result.Options = unsigned(key.kind);

  switch (key.kind) {
  case SyntacticElementTargetKey::Kind::empty:
  case SyntacticElementTargetKey::Kind::tombstone:
    llvm_unreachable("Invalid SyntacticElementTargetKey::Kind");
  case SyntacticElementTargetKey::Kind::stmtCondElement:
    result.TheCondElt = key.storage.stmtCondElement;
    break;
  case SyntacticElementTargetKey::Kind::expr:
    result.TheExpr = key.storage.expr;
    break;
  case SyntacticElementTargetKey::Kind::closure:
    result.TheClosure = cast<ClosureExpr>(key.storage.expr);
    break;
  case SyntacticElementTargetKey::Kind::stmt:
    result.TheStmt = key.storage.stmt;
    break;
  case SyntacticElementTargetKey::Kind::pattern:
    result.ThePattern = key.storage.pattern;
    break;
  case SyntacticElementTargetKey::Kind::patternBindingEntry:
    result.ThePatternBinding = key.storage.patternBindingEntry.patternBinding;
    result.Options |= key.storage.patternBindingEntry.index << 8;
    break;
  case SyntacticElementTargetKey::Kind::varDecl:
    result.TheVar = key.storage.varDecl;
    break;
  case SyntacticElementTargetKey::Kind::functionRef:
    result.TheDeclContext = key.storage.functionRef;
    break;
  }

  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedCaseLabelItemInfo(CaseLabelItem *item) {
  Change result;
  result.Kind = ChangeKind::RecordedCaseLabelItemInfo;
  result.TheItem = item;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedPotentialThrowSite(CatchNode catchNode) {
  Change result;
  result.Kind = ChangeKind::RecordedPotentialThrowSite;
  result.TheCatchNode = catchNode;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedIsolatedParam(ParamDecl *param) {
  Change result;
  result.Kind = ChangeKind::RecordedIsolatedParam;
  result.TheParam = param;
  return result;
}

SolverTrail::Change
SolverTrail::Change::RecordedKeyPath(KeyPathExpr *expr) {
  Change result;
  result.Kind = ChangeKind::RecordedKeyPath;
  result.KeyPath.Expr = expr;
  return result;
}

SyntacticElementTargetKey
SolverTrail::Change::getSyntacticElementTargetKey() const {
  ASSERT(Kind == ChangeKind::RecordedTarget);

  auto kind = SyntacticElementTargetKey::Kind(Options & 0xff);

  switch (kind) {
  case SyntacticElementTargetKey::Kind::empty:
  case SyntacticElementTargetKey::Kind::tombstone:
    llvm_unreachable("Invalid SyntacticElementTargetKey::Kind");
  case SyntacticElementTargetKey::Kind::stmtCondElement:
    return SyntacticElementTargetKey(TheCondElt);
  case SyntacticElementTargetKey::Kind::expr:
    return SyntacticElementTargetKey(TheExpr);
  case SyntacticElementTargetKey::Kind::closure:
    return SyntacticElementTargetKey(TheClosure);
  case SyntacticElementTargetKey::Kind::stmt:
    return SyntacticElementTargetKey(TheStmt);
  case SyntacticElementTargetKey::Kind::pattern:
    return SyntacticElementTargetKey(ThePattern);
  case SyntacticElementTargetKey::Kind::patternBindingEntry:
    return SyntacticElementTargetKey(ThePatternBinding, Options >> 8);
  case SyntacticElementTargetKey::Kind::varDecl:
    return SyntacticElementTargetKey(TheVar);
  case SyntacticElementTargetKey::Kind::functionRef:
    return SyntacticElementTargetKey(TheDeclContext);
  }
}

void SolverTrail::Change::undo(ConstraintSystem &cs) const {
  auto &cg = cs.getConstraintGraph();

  switch (Kind) {
#define LOCATOR_CHANGE(Name, Map) \
  case ChangeKind::Name: { \
    bool erased = cs.Map.erase(TheLocator); \
    ASSERT(erased); \
    break; \
  }
#include "swift/Sema/CSTrail.def"

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
    cs.removeFix(TheFix);
    break;

  case ChangeKind::AddedFixedRequirement:
    cs.removeFixedRequirement(FixedRequirement.GP, Options,
                              FixedRequirement.ReqTy);
    break;

  case ChangeKind::RecordedOpenedPackExpansionType:
    cs.removeOpenedPackExpansionType(TheExpansion);
    break;

  case ChangeKind::RecordedPackEnvironment:
    cs.removePackEnvironment(TheElement);
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
    cs.removeResultBuilderTransform(TheRef);
    break;

  case ChangeKind::AppliedPropertyWrapper:
    cs.removePropertyWrapper(TheExpr);
    break;

  case ChangeKind::RecordedClosureType:
    cs.removeClosureType(TheClosure);
    break;

  case ChangeKind::RecordedImpliedResult:
    cs.removeImpliedResult(TheExpr);
    break;

  case ChangeKind::RecordedContextualInfo:
    cs.removeContextualInfo(Node.Node);
    break;

  case ChangeKind::RecordedTarget:
    cs.removeTargetFor(getSyntacticElementTargetKey());
    break;

  case ChangeKind::RecordedCaseLabelItemInfo:
    cs.removeCaseLabelItemInfo(TheItem);
    break;

  case ChangeKind::RecordedPotentialThrowSite:
    cs.removePotentialThrowSite(TheCatchNode);
    break;

  case ChangeKind::RecordedExprPattern:
    cs.removeExprPatternFor(TheExpr);
    break;

  case ChangeKind::RecordedIsolatedParam:
    cs.removeIsolatedParam(TheParam);
    break;

  case ChangeKind::RecordedPreconcurrencyClosure:
    cs.removePreconcurrencyClosure(TheClosure);
    break;

  case ChangeKind::RecordedKeyPath:
    cs.removeKeyPath(KeyPath.Expr);
    break;

  case ChangeKind::IncreasedScore: {
    auto kind = Options & 0xff;
    unsigned value = Options >> 8;
    ASSERT(cs.CurrentScore.Data[kind] >= value);
    cs.CurrentScore.Data[kind] -= value;
    break;
  }

  case ChangeKind::DecreasedScore: {
    auto kind = Options & 0xff;
    unsigned value = Options >> 8;
    cs.CurrentScore.Data[kind] += value;
    break;
  }

  case ChangeKind::GeneratedConstraint: {
    auto iter = ConstraintList::iterator(TheConstraint.Constraint);
    cs.InactiveConstraints.erase(iter);
    break;
  }

  case ChangeKind::RetiredConstraint:
    cs.InactiveConstraints.push_back(TheConstraint.Constraint);
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

#define LOCATOR_CHANGE(Name, _) \
  case ChangeKind::Name: \
    out << "(" << #Name << " at "; \
    TheLocator->dump(&cs.getASTContext().SourceMgr, out); \
    out << ")\n"; \
    break;
#define EXPR_CHANGE(Name) \
  case ChangeKind::Name: \
    out << "(" << #Name << " "; \
    simple_display(out, TheExpr); \
    out << ")\n"; \
    break;
#define CLOSURE_CHANGE(Name) \
  case ChangeKind::Name: \
    out << "(" << #Name << " "; \
    simple_display(out, TheClosure); \
    out << ")\n"; \
    break;
#define CONSTRAINT_CHANGE(Name) \
  case ChangeKind::Name: \
    out << "(" << #Name << " "; \
    TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr, \
                                    indent + 2); \
    out << ")\n"; \
    break;
#define GRAPH_NODE_CHANGE(Name) \
    case ChangeKind::Name: \
      out << "(" << #Name << " "; \
      TheConstraint.Constraint->print(out, &cs.getASTContext().SourceMgr, \
                                      indent + 2); \
      out << " on type variable "; \
      TheConstraint.TypeVar->print(out, PO); \
      out << ")\n"; \
      break;
#define SCORE_CHANGE(Name) \
    case ChangeKind::Name: \
      out << "(" << #Name << " "; \
      out << Score::getNameFor(ScoreKind(Options & 0xff)); \
      out << " by " << (Options >> 8) << ")\n"; \
      break;
#include "swift/Sema/CSTrail.def"

  case ChangeKind::AddedTypeVariable:
    out << "(AddedTypeVariable ";
    TypeVar->print(out, PO);
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
    TheFix->print(out);
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

  case ChangeKind::RecordedOpenedPackExpansionType:
    out << "(RecordedOpenedPackExpansionType for ";
    TheExpansion->print(out, PO);
    out << ")\n";
    break;

  case ChangeKind::RecordedPackEnvironment:
    // FIXME: Print short form of PackExpansionExpr
    out << "(RecordedPackEnvironment ";
    simple_display(out, TheElement);
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

  case ChangeKind::RecordedResultBuilderTransform:
    out << "(RecordedResultBuilderTransform ";
    simple_display(out, TheRef);
    out << ")\n";
    break;

  case ChangeKind::RecordedContextualInfo:
    // FIXME: Print short form of ASTNode
    out << "(RecordedContextualInfo)\n";
    break;

  case ChangeKind::RecordedTarget:
    out << "(RecordedTarget ";
    getSyntacticElementTargetKey().dump(out);
    out << ")\n";
    break;

  case ChangeKind::RecordedCaseLabelItemInfo:
    // FIXME: Print something here
    out << "(RecordedCaseLabelItemInfo)\n";
    break;

  case ChangeKind::RecordedPotentialThrowSite:
    // FIXME: Print something here
    out << "(RecordedPotentialThrowSite)\n";
    break;

  case ChangeKind::RecordedIsolatedParam:
    out << "(RecordedIsolatedParam ";
    TheParam->dumpRef(out);
    out << ")\n";
    break;

  case ChangeKind::RecordedKeyPath:
    out << "(RecordedKeyPath ";
    simple_display(out, KeyPath.Expr);
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
