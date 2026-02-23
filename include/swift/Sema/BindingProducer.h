//===--- BindingProducer.h - Disjunction and binding choices ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SEMA_BINDING_PRODUCER_H
#define SWIFT_SEMA_BINDING_PRODUCER_H

#include "swift/AST/Attr.h"
#include "swift/Sema/Constraint.h"
#include "swift/Sema/ConstraintSystem.h"

namespace swift {

namespace constraints {

class DisjunctionChoice {
  ConstraintSystem &CS;
  unsigned Index;
  Constraint *Choice;
  bool ExplicitConversion;
  bool IsBeginningOfPartition;

public:
  DisjunctionChoice(ConstraintSystem &cs, unsigned index, Constraint *choice,
                    bool explicitConversion, bool isBeginningOfPartition)
      : CS(cs), Index(index), Choice(choice),
        ExplicitConversion(explicitConversion),
        IsBeginningOfPartition(isBeginningOfPartition) {}

  unsigned getIndex() const { return Index; }

  bool attempt(ConstraintSystem &cs) const;

  bool isDisabled() const {
    if (!Choice->isDisabled())
      return false;

    // If solver is in a diagnostic mode, let's allow
    // constraints that have fixes or have been disabled
    // in attempt to produce a solution faster for
    // well-formed expressions.
    if (CS.shouldAttemptFixes()) {
      return !(hasFix() || Choice->isDisabledInPerformanceMode());
    }

    return true;
  }

  bool hasFix() const {
    return bool(Choice->getFix());
  }

  bool isUnavailable() const {
    if (auto *decl = getOverloadChoiceDecl(Choice))
      return CS.isDeclUnavailable(decl, Choice->getLocator());
    return false;
  }

  bool isDisfavored() const {
    if (auto *decl = getOverloadChoiceDecl(Choice))
      return decl->getAttrs().hasAttribute<DisfavoredOverloadAttr>();
    return false;
  }

  bool isBeginningOfPartition() const { return IsBeginningOfPartition; }

  // FIXME: All three of the accessors below are required to support
  //        performance optimization hacks in constraint solver.

  bool isGenericOperator() const;
  bool isSymmetricOperator() const;
  bool isUnaryOperator() const;

  void print(llvm::raw_ostream &Out, SourceManager *SM,
             unsigned indent = 0) const {
    Out << "disjunction choice ";
    Choice->print(Out, SM, indent);
  }

  operator Constraint *() { return Choice; }
  operator Constraint *() const { return Choice; }

private:
  /// If associated disjunction is an explicit conversion,
  /// let's try to propagate its type early to prune search space.
  void propagateConversionInfo(ConstraintSystem &cs) const;

  static ValueDecl *getOperatorDecl(Constraint *choice) {
    auto *decl = getOverloadChoiceDecl(choice);
    if (!decl)
      return nullptr;

    return decl->isOperator() ? decl : nullptr;
  }
};

class ConjunctionElement {
  Constraint *Element;

public:
  ConjunctionElement(Constraint *element) : Element(element) {}

  bool attempt(ConstraintSystem &cs) const;

  ConstraintLocator *getLocator() const { return Element->getLocator(); }

  void print(llvm::raw_ostream &Out, SourceManager *SM, unsigned indent) const {
    Out << "conjunction element ";
    Element->print(Out, SM, indent);
  }

private:
  /// Find type variables referenced by this conjunction element.
  /// If this is a closure body element, it would look inside \c ASTNode.
  void
  findReferencedVariables(ConstraintSystem &cs,
                          SmallPtrSetImpl<TypeVariableType *> &typeVars) const;
};

class TypeVariableBinding {
  TypeVariableType *TypeVar;
  inference::PotentialBinding Binding;

public:
  TypeVariableBinding(TypeVariableType *typeVar,
                      inference::PotentialBinding &binding)
      : TypeVar(typeVar), Binding(binding) {}

  TypeVariableType *getTypeVariable() const { return TypeVar; }
  Type getType() const { return Binding.BindingType; }

  bool isDefaultable() const { return Binding.isDefaultableBinding(); }

  bool hasDefaultedProtocol() const {
    return Binding.hasDefaultedLiteralProtocol();
  }

  bool attempt(ConstraintSystem &cs) const;

  /// Determine what fix (if any) needs to be introduced into a
  /// constraint system as part of resolving type variable as a hole.
  std::optional<std::pair<ConstraintFix *, unsigned>>
  fixForHole(ConstraintSystem &cs) const;

  void print(llvm::raw_ostream &Out, SourceManager *, unsigned indent) const {
    PrintOptions PO = PrintOptions::forDebugging();
    Out << "type variable binding " << TypeVar->getString(PO)
        << " := " << Binding.BindingType->getString(PO);
  }
};

template<typename Choice>
class BindingProducer {
  ConstraintLocator *Locator;

protected:
  ConstraintSystem &CS;

public:
  BindingProducer(ConstraintSystem &cs, ConstraintLocator *locator)
      : Locator(locator), CS(cs) {}

  virtual ~BindingProducer() {}
  virtual std::optional<Choice> operator()() = 0;

  ConstraintLocator *getLocator() const { return Locator; }

  /// Check whether generator would have to compute next
  /// batch of bindings because it freshly ran out of current one.
  /// This is useful to be able to exhaustively attempt bindings
  /// for type variables found at one level, before proceeding to
  /// supertypes or literal defaults etc.
  virtual bool needsToComputeNext() const = 0;

  virtual bool isExhausted() const = 0;
};

class TypeVarBindingProducer : public BindingProducer<TypeVariableBinding> {
  using BindingKind = inference::AllowedBindingKind;
  using Binding = inference::PotentialBinding;

  TypeVariableType *TypeVar;
  llvm::SmallVector<Binding, 8> Bindings;
  /// The set of defaults to attempt once producer
  /// runs out of direct & transitive bindings.
  llvm::SmallVector<Constraint *, 4> DelayedDefaults;

  // The index pointing to the offset in the bindings
  // generator is currently at, `numTries` represents
  // the number of times bindings have been recomputed.
  unsigned Index = 0, NumTries = 0;

  llvm::SmallPtrSet<CanType, 4> ExploredTypes;
  llvm::SmallPtrSet<TypeBase *, 4> BoundTypes;

  /// Determines whether this type variable has a
  /// `ExpressibleByNilLiteral` requirement which
  /// means that bindings have to either conform
  /// to that protocol or be wrapped in an optional.
  bool CanBeNil;

  bool IsExhausted = false;

public:
  using Element = TypeVariableBinding;

  TypeVarBindingProducer(ConstraintSystem &cs,
                         TypeVariableType *typeVar,
                         const inference::BindingSet &bindings);

  /// Retrieve a set of bindings available in the current state.
  ArrayRef<Binding> getCurrentBindings() const { return Bindings; }

  std::optional<Element> operator()() override {
    if (isExhausted())
      return std::nullopt;

    // Once we reach the end of the current bindings
    // let's try to compute new ones, e.g. supertypes,
    // literal defaults, if that fails, we are done.
    if (needsToComputeNext() && !computeNext()) {
      IsExhausted = true;
      return std::nullopt;
    }

    auto &binding = Bindings[Index++];

    // Record produced type as bound/explored early, otherwise
    // it could be possible to re-discover it during `computeNext()`,
    // which leads to duplicate bindings e.g. inferring fallback
    // `Void` for a closure result type when `Void` was already
    // inferred as a direct/transitive binding.
    {
      auto type = binding.BindingType;

      BoundTypes.insert(type.getPointer());
      ExploredTypes.insert(type->getCanonicalType());
    }

    return TypeVariableBinding(TypeVar, binding);
  }

  bool needsToComputeNext() const override {
    return isExhausted() ? false : Index >= Bindings.size();
  }

  bool isExhausted() const override { return IsExhausted; }

private:
  /// Compute next batch of bindings if possible, this could
  /// be supertypes extracted from one of the current bindings
  /// or default literal types etc.
  ///
  /// \returns true if some new bindings were successfully computed,
  /// false otherwise.
  bool computeNext();

  /// Check whether binding type is required to either conform to
  /// `ExpressibleByNilLiteral` protocol or be wrapped into an optional type.
  bool requiresOptionalAdjustment(const Binding &binding) const;

  Binding getDefaultBinding(Constraint *constraint) const;
};

/// Iterator over disjunction choices, makes it
/// easy to work with disjunction and encapsulates
/// some other important information such as locator.
class DisjunctionChoiceProducer : public BindingProducer<DisjunctionChoice> {
  // The disjunction choices that this producer will iterate through.
  ArrayRef<Constraint *> Choices;

  // The ordering of disjunction choices. We index into Choices
  // through this vector in order to visit the disjunction choices in
  // the order we want to visit them.
  SmallVector<unsigned, 8> Ordering;

  // The index of the first element in a partition of the disjunction
  // choices. The choices are split into partitions where we will
  // visit all elements within a single partition before moving to the
  // elements of the next partition. If we visit all choices within a
  // single partition and have found a successful solution with one of
  // the choices in that partition, we stop looking for other
  // solutions.
  SmallVector<unsigned, 4> PartitionBeginning;

  // The index in the current partition of disjunction choices that we
  // are iterating over.
  unsigned PartitionIndex = 0;

  bool IsExplicitConversion;

  Constraint *Disjunction;

  unsigned Index = 0;

  bool needsGenericOperatorOrdering = true;

public:
  using Element = DisjunctionChoice;

  DisjunctionChoiceProducer(ConstraintSystem &cs, Constraint *disjunction,
                            llvm::TinyPtrVector<Constraint *> &favorites)
      : BindingProducer(cs, disjunction->shouldRememberChoice()
                                ? disjunction->getLocator()
                                : nullptr),
        Choices(disjunction->getNestedConstraints()),
        IsExplicitConversion(disjunction->isExplicitConversion()),
        Disjunction(disjunction) {
    assert(disjunction->getKind() == ConstraintKind::Disjunction);
    assert(!disjunction->shouldRememberChoice() || disjunction->getLocator());

    // Mark constraints as favored. This information
    // is going to be used by partitioner.
    for (auto *choice : favorites)
      cs.favorConstraint(choice);

    // Order and partition the disjunction choices.
    partitionDisjunction(Ordering, PartitionBeginning);
  }

  void setNeedsGenericOperatorOrdering(bool flag) {
    needsGenericOperatorOrdering = flag;
  }

  std::optional<Element> operator()() override {
    if (isExhausted())
      return std::nullopt;

    unsigned currIndex = Index;
    bool isBeginningOfPartition = PartitionIndex < PartitionBeginning.size() &&
                                  PartitionBeginning[PartitionIndex] == Index;
    if (isBeginningOfPartition)
      ++PartitionIndex;

    ++Index;

    auto choice = DisjunctionChoice(CS, currIndex, Choices[Ordering[currIndex]],
                                    IsExplicitConversion, isBeginningOfPartition);
    // Partition the generic operators before producing the first generic
    // operator disjunction choice.
    if (needsGenericOperatorOrdering && choice.isGenericOperator()) {
      unsigned nextPartitionIndex = (PartitionIndex < PartitionBeginning.size() ?
                                     PartitionBeginning[PartitionIndex] : Ordering.size());
      partitionGenericOperators(Ordering.begin() + currIndex,
                                Ordering.begin() + nextPartitionIndex);
      needsGenericOperatorOrdering = false;
    }

    return DisjunctionChoice(CS, currIndex, Choices[Ordering[currIndex]],
                             IsExplicitConversion, isBeginningOfPartition);
  }

  bool needsToComputeNext() const override { return false; }

  bool isExhausted() const override { return Index >= Choices.size(); }

private:
  // Partition the choices in the disjunction into groups that we will
  // iterate over in an order appropriate to attempt to stop before we
  // have to visit all of the options.
  void
  partitionDisjunction(SmallVectorImpl<unsigned> &Ordering,
                       SmallVectorImpl<unsigned> &PartitionBeginning);

  /// Partition the choices in the range \c first to \c last into groups and
  /// order the groups in the best order to attempt based on the argument
  /// function type that the operator is applied to.
  void partitionGenericOperators(SmallVectorImpl<unsigned>::iterator first,
                                 SmallVectorImpl<unsigned>::iterator last);
};

class ConjunctionElementProducer : public BindingProducer<ConjunctionElement> {
  ArrayRef<Constraint *> Elements;

  unsigned Index = 0;

public:
  using Element = ConjunctionElement;

  ConjunctionElementProducer(ConstraintSystem &cs, Constraint *conjunction)
      : BindingProducer(cs, conjunction->getLocator()),
        Elements(conjunction->getNestedConstraints()) {
    assert(conjunction->getKind() == ConstraintKind::Conjunction);
  }

  std::optional<Element> operator()() override {
    if (Index >= Elements.size())
      return std::nullopt;

    return ConjunctionElement(Elements[Index++]);
  }

  bool needsToComputeNext() const override { return false; }

  bool isExhausted() const override { return Index >= Elements.size(); }

  void markExhausted() {
    Index = Elements.size();
  }
};

}  // end namespace constraints

}  // end namespace swift
#endif  // SWIFT_SEMA_BINDING_PRODUCER_H