//===--- ConstraintSystem.h - Constraint-based Type Checking ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file provides the constraint-based type checker, anchored by the
// \c ConstraintSystem class, which provides type checking and type
// inference for expressions.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINT_SYSTEM_H
#define SWIFT_SEMA_CONSTRAINT_SYSTEM_H

#include "TypeChecker.h"
#include "Constraint.h"
#include "ConstraintGraphScope.h"
#include "ConstraintLocator.h"
#include "OverloadChoice.h"
#include "swift/Basic/Fixnum.h"
#include "swift/Basic/LLVM.h"
#include "swift/Basic/Optional.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "swift/AST/TypeCheckerDebugConsumer.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"
#include <cstddef>
#include <deque> // FIXME: Eliminate reliance on deque
#include <functional>

namespace swift {

class Expr;

namespace constraints {

class ConstraintGraph;
class ConstraintGraphNode;
class ConstraintSystem;

} // end namespace constraints

} // end namespace swift

/// \brief Allocate memory within the given constraint system.
void *operator new(size_t bytes, swift::constraints::ConstraintSystem& cs,
                   size_t alignment = 8);

namespace swift {

namespace constraints {

/// \brief A handle that holds the saved state of a type variable, which
/// can be restored.
class SavedTypeVariableBinding {
  /// \brief The type variable.
  TypeVariableType *TypeVar;

  /// \brief The parent or fixed type.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

public:
  explicit SavedTypeVariableBinding(TypeVariableType *typeVar);

  /// \brief Restore the state of the type variable to the saved state.
  void restore();
};

/// \brief A set of saved type variable bindings.
typedef SmallVector<SavedTypeVariableBinding, 16> SavedTypeVariableBindings;

class ConstraintLocator;

} // end namespace constraints

/// Options that describe how a type variable can be used.
enum TypeVariableOptions {
  /// Whether the type variable can be bound to an lvalue type or not.
  TVO_CanBindToLValue = 0x01,

  /// Whether a more specific deduction for this type variable implies a
  /// better solution to the constraint system.
  TVO_PrefersSubtypeBinding = 0x02
};

/// \brief The implementation object for a type variable used within the
/// constraint-solving type checker.
///
/// The implementation object for a type variable contains information about
/// the type variable, where it was generated, what protocols it must conform
/// to, what specific types it might be and, eventually, the fixed type to
/// which it is assigned.
class TypeVariableType::Implementation {
  /// Type variable options.
  unsigned Options : 2;

  /// \brief The locator that describes where this type variable was generated.
  constraints::ConstraintLocator *locator;

  /// \brief Either the parent of this type variable within an equivalence
  /// class of type variables, or the fixed type to which this type variable
  /// type is bound.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

  /// The corresponding node in the constraint graph.
  constraints::ConstraintGraphNode *GraphNode = nullptr;

  ///  Index into the list of type variables, as used by the
  ///  constraint graph.
  unsigned GraphIndex;

  friend class constraints::SavedTypeVariableBinding;

public:
  explicit Implementation(constraints::ConstraintLocator *locator,
                          unsigned options)
    : Options(options), locator(locator),
      ParentOrFixed(getTypeVariable()) { }

  /// \brief Retrieve the unique ID corresponding to this type variable.
  unsigned getID() const { return getTypeVariable()->getID(); }

  /// Whether this type variable can bind to an lvalue type.
  bool canBindToLValue() const { return Options & TVO_CanBindToLValue; }

  /// Whether this type variable prefers a subtype binding over a supertype
  /// binding.
  bool prefersSubtypeBinding() const {
    return Options & TVO_PrefersSubtypeBinding;
  }

  /// \brief Retrieve the type variable associated with this implementation.
  TypeVariableType *getTypeVariable() {
    return reinterpret_cast<TypeVariableType *>(this) - 1;
  }

  /// \brief Retrieve the type variable associated with this implementation.
  const TypeVariableType *getTypeVariable() const {
    return reinterpret_cast<const TypeVariableType *>(this) - 1;
  }

  /// Retrieve the corresponding node in the constraint graph.
  constraints::ConstraintGraphNode *getGraphNode() const { return GraphNode; }

  /// Set the corresponding node in the constraint graph.
  void setGraphNode(constraints::ConstraintGraphNode *newNode) { 
    GraphNode = newNode; 
  }

  /// Retrieve the index into the constraint graph's list of type variables.
  unsigned getGraphIndex() const { 
    assert(GraphNode && "Graph node isn't set");
    return GraphIndex; 
  }

  /// Set the index into the constraint graph's list of type variables.
  void setGraphIndex(unsigned newIndex) { GraphIndex = newIndex; }
  
  /// \brief Check whether this type variable either has a representative that
  /// is not itself or has a fixed type binding.
  bool hasRepresentativeOrFixed() const {
    // If we have a fixed type, we're done.
    if (!ParentOrFixed.is<TypeVariableType *>())
      return true;

    // Check whether the representatative is different from our own type
    // variable.
    return ParentOrFixed.get<TypeVariableType *>() != getTypeVariable();
  }

  /// \brief Record the current type-variable binding.
  void recordBinding(constraints::SavedTypeVariableBindings &record) {
    record.push_back(constraints::SavedTypeVariableBinding(getTypeVariable()));
  }

  /// \brief Retrieve the locator describing where this type variable was
  /// created.
  constraints::ConstraintLocator *getLocator() const {
    return locator;
  }

  /// \brief Retrieve the archetype opened by this type variable.
  ArchetypeType *getArchetype() const;

  /// \brief Retrieve the representative of the equivalence class to which this
  /// type variable belongs.
  ///
  /// \param record The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  TypeVariableType *
  getRepresentative(constraints::SavedTypeVariableBindings *record) {
    // Find the representative type variable.
    auto result = getTypeVariable();
    Implementation *impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == result)
        break;

      result = nextTV;
      impl = &nextTV->getImpl();
    }

    if (impl == this || !record)
      return result;

    // Perform path compression.
    impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == result)
        break;

      // Record the state change.
      impl->recordBinding(*record);

      impl->ParentOrFixed = result;
      impl = &nextTV->getImpl();
    }

    return result;
  }

  /// \brief Merge the equivalence class of this type variable with the
  /// equivalence class of another type variable.
  ///
  /// \param other The type variable to merge with.
  ///
  /// \param record The record of state changes.
  void mergeEquivalenceClasses(TypeVariableType *other,
                               constraints::SavedTypeVariableBindings *record) {
    // Merge the equivalence classes corresponding to these two type
    // variables. Always merge 'up' the constraint stack, because it is simpler.
    if (getID() < other->getImpl().getID()) {
      auto rep = other->getImpl().getRepresentative(record);
      if (record)
        rep->getImpl().recordBinding(*record);
      rep->getImpl().ParentOrFixed = getTypeVariable();
      assert(rep->getImpl().canBindToLValue() == canBindToLValue());
    } else {
      auto rep = getRepresentative(record);
      if (record)
        rep->getImpl().recordBinding(*record);
      rep->getImpl().ParentOrFixed = other;
      assert(rep->getImpl().canBindToLValue()
               == other->getImpl().canBindToLValue());
    }
  }

  /// \brief Retrieve the fixed type that corresponds to this type variable,
  /// if there is one.
  ///
  /// \returns the fixed type associated with this type variable, or a null
  /// type if there is no fixed type.
  ///
  /// \param record The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  Type getFixedType(constraints::SavedTypeVariableBindings *record) {
    // Find the representative type variable.
    auto rep = getRepresentative(record);
    Implementation &repImpl = rep->getImpl();

    // Check whether it has a fixed type.
    if (auto type = repImpl.ParentOrFixed.dyn_cast<TypeBase *>())
      return type;

    return Type();
  }

  /// \brief Assign a fixed type to this equivalence class.
  void assignFixedType(Type type,
                       constraints::SavedTypeVariableBindings *record) {
    assert((!getFixedType(0) || getFixedType(0)->isEqual(type)) &&
           "Already has a fixed type!");
    auto rep = getRepresentative(record);
    if (record)
      rep->getImpl().recordBinding(*record);
    rep->getImpl().ParentOrFixed = type.getPointer();
  }

  /// \brief Print the type variable to the given output stream.
  void print(llvm::raw_ostream &OS);
};

namespace constraints {

struct ResolvedOverloadSetListItem;

/// \brief Describes a failure.
class Failure : public llvm::FoldingSetNode {
public:
  /// \brief The various kinds of failures that can occur 
  enum FailureKind {
    /// \brief Tuple types with different numbers of elements.
    TupleSizeMismatch,
    /// \brief Tuple element names mismatch when they need to match.
    TupleNameMismatch,
    /// \brief Tuple element name matched, but at a different position.
    TupleNamePositionMismatch,
    /// \brief One tuple type is variadic, the other is not.
    TupleVariadicMismatch,
    /// \brief Unused element in tuple.
    TupleUnused,
    /// \brief Autoclosure function type mismatch.
    FunctionAutoclosureMismatch,
    /// \brief Noreturn attribute function type mismatch.
    FunctionNoReturnMismatch,
    /// \brief Types are not the same.
    TypesNotEqual,
    /// \brief Types are not trivial subtypes.
    TypesNotTrivialSubtypes,
    /// \brief Types are not subtypes.
    TypesNotSubtypes,
    /// \brief Types are not convertible.
    TypesNotConvertible,
    /// \brief Types are not constructible.
    TypesNotConstructible,
    /// \brief Function types mismatch.
    FunctionTypesMismatch,
    /// \brief Lvalue type qualifiers mismatch.
    LValueQualifiers,
    /// \brief The first type doesn't conform to a protocol in the second
    /// type.
    DoesNotConformToProtocol,
    /// \brief The first type does not have a member with the given name.
    DoesNotHaveMember,
    /// \brief The type is not an archetype.
    IsNotArchetype,
    /// \brief The type is not a class.
    IsNotClass,
    /// \brief The type is not a dynamic lookup value.
    IsNotDynamicLookup,
    /// \brief The type is not allowed to be an l-value.
    IsForbiddenLValue,
  };

private:
  /// \brief The kind of failure this describes.
  FailureKind kind : 8;

  /// \brief A value, if used.
  unsigned value : 32;

  /// Describes the location of this failure.
  ConstraintLocator *locator;

  /// The resolved overload sets that led to this failure.
  ResolvedOverloadSetListItem *resolvedOverloadSets;

  /// \brief The first type.
  Type first;

  /// \brief The second value, which may be one of several things (type,
  /// member name, etc.).
  union {
    TypeBase *type;
    void *name;
  } second;

public:
  /// \brief Retrieve the failure kind.
  FailureKind getKind() const { return kind; }

  /// \brief Retrieve the failure locator.
  ConstraintLocator *getLocator() const {
    return locator;
  }

  /// Retrieve the resolved overload sets active when this failure occurred.
  ResolvedOverloadSetListItem *getResolvedOverloadSets() const {
    return resolvedOverloadSets;
  }

  /// \brief Retrieve the first type.
  Type getFirstType() const { return first; }

  /// \brief Retrieve the second type.
  Type getSecondType() const {
    return second.type;
  }

  /// \brief Retrieve the name.
  Identifier getName() const {
    return Identifier::getFromOpaquePointer(second.name);
  }

  /// \brief Retrieve the value.
  unsigned getValue() const { return value; }

  /// \brief Profile the given failure.
  void Profile(llvm::FoldingSetNodeID &id) {
    switch (kind) {
    case FunctionTypesMismatch:
    case FunctionAutoclosureMismatch:
    case FunctionNoReturnMismatch:
    case LValueQualifiers:
    case TupleNameMismatch:
    case TupleNamePositionMismatch:
    case TupleSizeMismatch:
    case TupleUnused:
    case TupleVariadicMismatch:
    case TypesNotConstructible:
    case TypesNotConvertible:
    case TypesNotEqual:
    case TypesNotSubtypes:
    case TypesNotTrivialSubtypes:
    case DoesNotConformToProtocol:
    case IsForbiddenLValue:
      return Profile(id, locator, kind, resolvedOverloadSets, getFirstType(),
                     getSecondType());

    case DoesNotHaveMember:
      return Profile(id, locator, kind, resolvedOverloadSets, getFirstType(),
                     getName());

    case IsNotArchetype:
    case IsNotClass:
    case IsNotDynamicLookup:
      return Profile(id, locator, kind, resolvedOverloadSets, getFirstType());
    }
  }

  /// \brief Dump a debug representation of this failure.
  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *SM) const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");

  void dump(SourceManager *SM, raw_ostream &OS) const;

private:
  friend class ConstraintSystem;

  /// \brief Construct a failure involving one type.
  Failure(ConstraintLocator *locator, FailureKind kind,
          ResolvedOverloadSetListItem *resolvedOverloadSets,
          Type type)
    : kind(kind), value(0), locator(locator),
      resolvedOverloadSets(resolvedOverloadSets), first(type)
  {
    second.type = nullptr;
  }

  /// \brief Construct a failure involving two types and an optional value.
  Failure(ConstraintLocator *locator, FailureKind kind,
          ResolvedOverloadSetListItem *resolvedOverloadSets,
          Type type1, Type type2, unsigned value = 0)
    : kind(kind), value(value), locator(locator),
      resolvedOverloadSets(resolvedOverloadSets), first(type1)
  {
    second.type = type2.getPointer();
  }

  /// \brief Construct a failure involving a type and a name.
  Failure(ConstraintLocator *locator, FailureKind kind,
          ResolvedOverloadSetListItem *resolvedOverloadSets,
          Type type, Identifier name)
    : kind(kind), value(0), locator(locator),
      resolvedOverloadSets(resolvedOverloadSets), first(type)
  {
    second.name = name.getAsOpaquePointer();
  }

  /// \brief Profile a failure involving one type.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind,
                      ResolvedOverloadSetListItem *resolvedOverloadSets,
                      Type type) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(resolvedOverloadSets);
    id.AddPointer(type.getPointer());
  }

  /// \brief Profile a failure involving two types.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind,
                      ResolvedOverloadSetListItem *resolvedOverloadSets,
                      Type type1, Type type2) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(resolvedOverloadSets);
    id.AddPointer(type1.getPointer());
    id.AddPointer(type2.getPointer());
  }

  /// \brief Profile a failure involving two types and a value.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind,
                      ResolvedOverloadSetListItem *resolvedOverloadSets,
                      Type type1, Type type2, unsigned value) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(resolvedOverloadSets);
    id.AddPointer(type1.getPointer());
    id.AddPointer(type2.getPointer());
    id.AddInteger(value);
  }

  /// \brief Profile a failure involving a type and a name.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind,
                      ResolvedOverloadSetListItem *resolvedOverloadSets,
                      Type type, Identifier name) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(resolvedOverloadSets);
    id.AddPointer(type.getPointer());
    id.AddPointer(name.getAsOpaquePointer());
  }

  /// \brief Create a new Failure object with the given arguments, allocated
  /// from the given bump pointer allocator.
  template<typename ...Args>
  static Failure *create(llvm::BumpPtrAllocator &allocator,
                         ConstraintLocator *locator, FailureKind kind,
                         Args &&...args) {
    void *mem = allocator.Allocate(sizeof(Failure), alignof(Failure));
    return new (mem) Failure(locator, kind, args...);
  }
};

/// \brief A representative type variable with the list of constraints
/// that apply to it.
struct TypeVariableConstraints {
  TypeVariableConstraints(TypeVariableType *typeVar) : TypeVar(typeVar) {}

  /// \brief Whether there are any non-concrete constraints placed on this
  /// type variable that aren't represented by the stored constraints.
  bool HasNonConcreteConstraints = false;

  /// \brief Whether this type variable is either fully bound by either an
  /// overload set or a member constraint.
  bool FullyBound = false;

  /// \brief The representative type variable.
  TypeVariableType *TypeVar;

  /// \brief The set of constraints "above" the type variable.
  SmallVector<std::pair<Constraint *, Type>, 4> Above;

  /// \brief The set of constraints "below" the type variable.
  SmallVector<std::pair<Constraint *, Type>, 4> Below;

  /// \brief The set of protocol conformance constraints directly applicable
  /// to the type variable.
  SmallVector<Constraint *, 4> ConformsToConstraints;
};

/// \brief The kind of type matching to perform in matchTypes().
enum class TypeMatchKind : char {
  /// \brief Bind the types together directly.
  BindType,
  /// \brief Require the types to match exactly, but strips lvalueness from
  /// a type when binding to a type variable.
  SameType,
  /// \brief Require the first type to be a "trivial" subtype of the second
  /// type or be an exact match.
  TrivialSubtype,
  /// \brief Require the first type to be a subtype of the second type
  /// (or be an exact match or trivial subtype).
  Subtype,
  /// \brief Requires the first type to be convertible to the second type,
  /// which includes exact matches and both forms of subtyping.
  Conversion
};

/// \brief The result of comparing two constraint systems that are a solutions
/// to the given set of constraints.
enum class SolutionCompareResult {
  /// \brief The two solutions are incomparable, because, e.g., because one
  /// solution has some better decisions and some worse decisions than the
  /// other.
  Incomparable,
  /// \brief The two solutions are identical.
  Identical,
  /// \brief The first solution is better than the second.
  Better,
  /// \brief The second solution is better than the first.
  Worse
};

/// An overload that has been selected in a particular solution.
///
/// A selected overload captures the specific overload choice (e.g., a
/// particular declaration) as well as the type to which the reference to the
/// declaration was opened, which may involve type variables.
struct SelectedOverload {
  /// The overload choice.
  OverloadChoice choice;

  /// The opened type of the base of the reference to this overload, if
  /// we're referencing a member.
  Type openedFullType;

  /// The opened type produced by referring to this overload.
  Type openedType;
};

/// \brief A complete solution to a constraint system.
///
/// A solution to a constraint system consists of type variable bindings to
/// concrete types for every type variable that is used in the constraint
/// system along with a set of mappings from each constraint locator
/// involving an overload set to the selected overload.
class Solution {
  /// \brief The constraint system this solution solves.
  ConstraintSystem *constraintSystem;

  /// \brief The fixed score for this solution.
  mutable Optional<int> fixedScore;

public:
  /// \brief Create a solution for the given constraint system.
  Solution(ConstraintSystem &cs) : constraintSystem(&cs) {}

  // Solution is a non-copyable type for performance reasons.
  Solution(const Solution &other) = delete;
  Solution &operator=(const Solution &other) = delete;

  Solution(Solution &&other)
    : constraintSystem(other.constraintSystem),
      typeBindings(std::move(other.typeBindings)),
      overloadChoices(std::move(other.overloadChoices)),
      constraintRestrictions(std::move(other.constraintRestrictions))
  {
  }

  Solution &operator=(Solution &&other) {
    constraintSystem = other.constraintSystem;
    typeBindings = std::move(other.typeBindings);
    overloadChoices = std::move(other.overloadChoices);
    constraintRestrictions = std::move(other.constraintRestrictions);
    return *this;
  }

  /// \brief Retrieve the constraint system that this solution solves.
  ConstraintSystem &getConstraintSystem() const { return *constraintSystem; }

  /// \brief The set of type bindings.
  llvm::SmallDenseMap<TypeVariableType *, Type> typeBindings;

  /// \brief The set of overload choices along with their types.
  llvm::SmallDenseMap<ConstraintLocator *, SelectedOverload> overloadChoices;

  /// The set of constraint restrictions used to arrive at this restriction,
  /// which informs constraint application.
  llvm::SmallDenseMap<std::pair<CanType, CanType>, ConversionRestrictionKind>
    constraintRestrictions;

  /// \brief Simplify the given type by substituting all occurrences of
  /// type variables for their fixed types.
  Type simplifyType(TypeChecker &tc, Type type) const;

  /// \brief Coerce the given expression to the given type.
  ///
  /// This operation cannot fail.
  ///
  /// \param expr The expression to coerce.
  /// \param toType The type to coerce the expression to.
  /// \param locator Locator used to describe the location of this expression.
  ///
  /// \returns the coerced expression, which will have type \c ToType.
  Expr *coerceToType(Expr *expr, Type toType, ConstraintLocator *locator) const;

  /// \brief Convert the given expression to a logic value.
  ///
  /// This operation cannot fail.
  ///
  /// \param expr The expression to coerce. The type of this expression
  /// must conform to the LogicValue protocol.
  ///
  /// \param locator Locator used to describe the location of this expression.
  ///
  /// \returns the expression converted to a logic value (Builtin i1).
  Expr *convertToLogicValue(Expr *expr, ConstraintLocator *locator) const;

  /// \brief Convert the given expression to an array bound.
  ///
  /// This operation cannot fail.
  ///
  /// \param expr The expression to coerce. The type of this expression
  /// must conform to the ArrayBound protocol.
  ///
  /// \param locator Locator used to describe the location of this expression.
  ///
  /// \returns the expression converted to an array bound (Builtin integral
  /// type).
  Expr *convertToArrayBound(Expr *expr, ConstraintLocator *locator) const;

  /// Compute the set of substitutions required to map the given type
  /// to the provided "opened" type.
  ///
  /// Either the generic type (\c origType) must either be a
  /// \c GenericFunctionType, in which case it's generic requirements will be
  /// used to compute the required substitutions, or \c dc must be a generic
  /// context, in which case it's generic requirements will be used.
  ///
  /// \param origType The generic type.
  ///
  /// \param openedType The type to which this reference to the given
  /// generic function type was opened.
  ///
  /// \param dc          The declaration context that owns the generic type
  ///
  /// \param substitutions Will be populated with the set of substitutions
  /// to be applied to the generic function type.
  ///
  /// \returns The opened type after applying the computed substitutions.
  Type computeSubstitutions(Type origType,
                            DeclContext *dc,
                            Type openedType,
                            SmallVectorImpl<Substitution> &substitutions) const;

  /// \brief Retrieve the fixed score of this solution, which considers
  /// the number of user-defined conversions.
  int getFixedScore() const;
  
  /// \brief Retrieve the fixed type for the given type variable.
  Type getFixedType(TypeVariableType *typeVar) const;

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *SM) const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");

  /// \brief Dump this solution.
  void dump(SourceManager *SM, raw_ostream &OS) const LLVM_ATTRIBUTE_USED;
};

/// \brief Describes the differences between several solutions to the same
/// constraint system.
class SolutionDiff {
public:
  /// \brief A difference between two overloads.
  struct OverloadDiff {
    /// \brief The locator that describes where the overload comes from.
    ConstraintLocator *locator;

    /// \brief The choices that each solution made.
    SmallVector<OverloadChoice, 2> choices;
  };

  /// \brief A difference between two type variable bindings.
  struct TypeBindingDiff {
    /// \brief The type variable.
    TypeVariableType *typeVar;

    /// \brief The bindings that each solution made.
    SmallVector<Type, 2> bindings;
  };

  /// \brief The differences between the overload choices between the
  /// solutions.
  SmallVector<OverloadDiff, 4> overloads;

  /// \brief The differences between the type variable bindings of the
  /// solutions.
  SmallVector<TypeBindingDiff, 4> typeBindings;

  /// \brief Compute the differences between the given set of solutions.
  ///
  /// \param solutions The set of solutions.
  explicit SolutionDiff(ArrayRef<Solution> solutions);
};

/// Describes one resolved overload set within the list of overload sets
/// resolved by the solver.
struct ResolvedOverloadSetListItem {
  /// The previously resolved overload set in the list.
  ResolvedOverloadSetListItem *Previous;

  /// The type that this overload binds.
  Type BoundType;

  /// The overload choice.
  OverloadChoice Choice;

  /// The locator for this choice.
  ConstraintLocator *Locator;

  /// The type of the fully-opened base, if any.
  Type OpenedFullType;

  /// The type of the referenced choice.
  Type ImpliedType;

  // Make vanilla new/delete illegal for overload set items.
  void *operator new(size_t Bytes) = delete;
  void operator delete(void *Data) = delete;

  // Only allow allocation of list items using the allocator in the
  // constraint system.
  void *operator new(size_t bytes, ConstraintSystem &cs,
                     unsigned alignment
                       = alignof(ResolvedOverloadSetListItem));
};

/// Identifies a specific conversion from
struct SpecificConstraint {
  CanType First;
  CanType Second;
  ConstraintKind Kind;
};

/// Abstract class implemented by clients that want to be involved in
/// the process of opening dependent types to type variables.
class DependentTypeOpener {
public:
  virtual ~DependentTypeOpener() { }

  /// Invoked when a generic type parameter is opened to a type variable.
  ///
  /// \param param The generic type parameter.
  ///
  /// \param typeVar The type variable to which the generic parameter was
  /// opened.
  ///
  /// \param replacementType If the caller sets this to a non-null type, the
  /// type variable will be bound directly to this type.
  virtual void openedGenericParameter(GenericTypeParamType *param,
                                      TypeVariableType *typeVar,
                                      Type &replacementType) { }

  /// Invoked when an associated type reference is opened to a type
  /// variable to determine how the associated type should be resolved.
  ///
  /// \param baseType The type of the base of the reference.
  ///
  /// \param baseTypeVar The type variable to which the base type was
  /// opened.
  ///
  /// \param assocType The associated type being opened.
  ///
  /// \param memberTypeVar The type variable representing the
  /// dependent member type.
  ///
  /// \param replacementType If the caller sets this to a non-null type, the
  /// member type variable will be bound directly to this type.
  ///
  /// \returns true if the constraint system should introduce a
  /// constraint that specifies that the member type is in fact a the
  /// named member of the base's type variable.
  virtual bool shouldBindAssociatedType(Type baseType,
                                        TypeVariableType *baseTypeVar,
                                        AssociatedTypeDecl *assocType,
                                        TypeVariableType *memberTypeVar,
                                        Type &replacementType) { 
    return true;
  }
};

/// An intrusive, doubly-linked list of constraints.
typedef llvm::ilist<Constraint> ConstraintList;

/// \brief Describes a system of constraints on type variables, the
/// solution of which assigns concrete types to each of the type variables.
/// Constraint systems are typically generated given an (untyped) expression.
class ConstraintSystem {
public:
  TypeChecker &TC;
  DeclContext *DC;
private:
  Constraint *failedConstraint = nullptr;
  
  /// \brief Allocator used for all of the related constraint systems.
  llvm::BumpPtrAllocator Allocator;

  /// \brief Arena used for memory management of constraint-checker-related
  /// allocations.
  ConstraintCheckerArenaRAII Arena;

  /// \brief Counter for type variables introduced.
  unsigned TypeCounter = 0;

  /// \brief Cached member lookups.
  llvm::DenseMap<std::pair<Type, Identifier>, Optional<LookupResult>>
    MemberLookups;

  /// \brief Folding set containing all of the locators used in this
  /// constraint system.
  llvm::FoldingSet<ConstraintLocator> ConstraintLocators;

  /// \brief Folding set containing all of the failures that have occurred
  /// while building and initially simplifying this constraint system.
  ///
  /// These failures are unavoidable, in the sense that they occur before
  /// we have made any (potentially incorrect) assumptions at all.
  SmallVector<Failure *, 1> unavoidableFailures;

  /// \brief Failures that occured while solving.
  ///
  /// FIXME: We really need to track overload sets and type variable bindings
  /// to make any sense of this data. Also, it probably belongs within
  /// SolverState.
  llvm::FoldingSet<Failure> failures;

  /// \brief The overload sets that have been resolved along the current path.
  ResolvedOverloadSetListItem *resolvedOverloadSets = nullptr;

  SmallVector<TypeVariableType *, 16> TypeVariables;
  ConstraintList Constraints;

  /// The worklist of constraints that should be revisited due to a change.
  /// FIXME: This is a crappy data structure. We want to bounce between
  /// two ConstraintLists so there's no memory allocation needed.
  std::deque<Constraint *> Worklist;

  /// The constraint graph, if there is one.
  ConstraintGraph *CG = nullptr;

  typedef llvm::PointerUnion<TypeVariableType *, TypeBase *>
    RepresentativeOrFixed;

  /// \brief Describes the current solver state.
  struct SolverState {
    SolverState(ConstraintSystem &cs);
    ~SolverState();

    /// The constraint system.
    ConstraintSystem &CS;

    /// Old value of DebugConstraintSolver.
    /// FIXME: Move the "debug constraint solver" bit into the constraint 
    /// system itself.
    bool OldDebugConstraintSolver;

    /// \brief Depth of the solution stack.
    unsigned depth = 0;

    /// \brief Whether to record failures or not.
    bool recordFailures = false;

    /// The list of constraints that have been retired along the
    /// current path.
    ConstraintList retiredConstraints;

    /// The current set of generated constraints.
    llvm::SmallPtrSet<Constraint *, 4> *generatedConstraints = nullptr;

    /// \brief The set of constraint restrictions used to reach this state.
    ///
    /// Constraint restrictions help describe which path the solver took when
    /// there are multiple ways in which one type could convert to another, e.g.,
    /// given class types A and B, the solver might choose either a superclass
    /// conversion or a user-defined conversion.
    SmallVector<std::tuple<Type, Type, ConversionRestrictionKind>, 32>
      constraintRestrictions;

    /// \brief The set of type variable bindings that have changed while
    /// processing this constraint system.
    SavedTypeVariableBindings savedBindings;

    /// The number of the solution attempt we're looking at.
    unsigned SolutionAttempt;

    // Statistics
    #define CS_STATISTIC(Name, Description) unsigned Name = 0;
    #include "ConstraintSolverStats.def"
  };

public:
  /// \brief The current solver state.
  ///
  /// This will be non-null when we're actively solving the constraint
  /// system, and carries temporary state related to the current path
  /// we're exploring. 
  SolverState *solverState = nullptr;

private:
  unsigned assignTypeVariableID() {
    return TypeCounter++;
  }

public:
  /// \brief Introduces a new solver scope, which any changes to the
  /// solver state or constraint system are temporary and will be undone when
  /// this object is destroyed.
  ///
  ///
  class SolverScope {
    ConstraintSystem &cs;

    /// \brief The current resolved overload set list.
    ResolvedOverloadSetListItem *resolvedOverloadSets;

    /// \brief The length of \c TypeVariables.
    unsigned numTypeVariables;

    /// \brief The length of \c SavedBindings.
    unsigned numSavedBindings;

    /// The set of constraints generated within this scope.
    llvm::SmallPtrSet<Constraint *, 4> generatedConstraints;

    /// The outer generatedConstraints setting.
    llvm::SmallPtrSet<Constraint *, 4> *oldGeneratedConstraints;

    /// \brief The last retired constraint in the list.
    ConstraintList::iterator firstRetired;

    /// \brief The length of \c constraintRestrictions.
    unsigned numConstraintRestrictions;

    /// Constraint graph scope associated with this solver scope.
    ///
    /// FIXME: This is optional so we can easily enabled/disable the
    /// constraint graph globally.
    Optional<ConstraintGraphScope> CGScope;

    SolverScope(const SolverScope &) = delete;
    SolverScope &operator=(const SolverScope &) = delete;

  public:
    explicit SolverScope(ConstraintSystem &cs);
    ~SolverScope();
  };

  ConstraintSystem(TypeChecker &tc, DeclContext *dc);
  ~ConstraintSystem();

  /// \brief Retrieve the type checker associated with this constraint system.
  TypeChecker &getTypeChecker() const { return TC; }

  /// \brief Retrieve the AST context.
  ASTContext &getASTContext() const { return TC.Context; }

private:
  /// \brief Determine whether this constraint system has any free type
  /// variables.
  bool hasFreeTypeVariables();

  /// \brief Finalize this constraint system; we're done attempting to solve
  /// it.
  ///
  /// \returns the solution.
  Solution finalize(FreeTypeVariableBinding allowFreeTypeVariables);

  /// \brief Apply the given solution to the current constraint system.
  ///
  /// This operation is used to take a solution computed based on some
  /// subset of the constraints and then apply it back to the
  /// constraint system for further exploration.
  void applySolution(const Solution &solution);

  /// \brief Restore the type variable bindings to what they were before
  /// we attempted to solve this constraint system.
  ///
  /// \param numBindings The number of bindings to restore, from the end of
  /// the saved-binding stack.
  void restoreTypeVariableBindings(unsigned numBindings);

  /// \brief Retrieve the set of saved type variable bindings, if available.
  ///
  /// \returns null when we aren't currently solving the system.
  SavedTypeVariableBindings *getSavedBindings() const {
    return solverState? &solverState->savedBindings : nullptr;
  }

  /// Add a new type variable that was already created.
  void addTypeVariable(TypeVariableType *typeVar);

public:
  /// \brief Lookup for a member with the given name in the given base type.
  ///
  /// This routine caches the results of member lookups in the top constraint
  /// system, to avoid.
  ///
  /// FIXME: This caching should almost certainly be performed at the
  /// module level, since type checking occurs after name binding,
  /// and no new names are introduced after name binding.
  ///
  /// \returns A reference to the member-lookup result.
  LookupResult &lookupMember(Type base, Identifier name);

  /// \brief Create a new type variable.
  TypeVariableType *createTypeVariable(ConstraintLocator *locator,
                                       unsigned options) {
    auto tv = TypeVariableType::getNew(TC.Context, assignTypeVariableID(),
                                       locator, options);
    addTypeVariable(tv);
    return tv;
  }

  /// Retrieve the set of active type variables.
  ArrayRef<TypeVariableType *> getTypeVariables() const {
    return TypeVariables;
  }

  /// \brief Retrieve the constraint locator for the given anchor and
  /// path, uniqued.
  ConstraintLocator *
  getConstraintLocator(Expr *anchor,
                       ArrayRef<ConstraintLocator::PathElement> path);

  /// \brief Retrieve the constraint locator for the given anchor and
  /// path element.
  ConstraintLocator *
  getConstraintLocator(Expr *anchor, ConstraintLocator::PathElement pathElt) {
    return getConstraintLocator(anchor, llvm::makeArrayRef(pathElt));
  }

  /// \brief Extend the given constraint locator with a path element.
  ConstraintLocator *
  getConstraintLocator(ConstraintLocator *locator,
                       ConstraintLocator::PathElement pathElt) {
    return getConstraintLocator(ConstraintLocatorBuilder(locator)
                                  .withPathElement(pathElt));
  }

  /// \brief Retrieve the constraint locator described by the given
  /// builder.
  ConstraintLocator *
  getConstraintLocator(const ConstraintLocatorBuilder &builder);

private:
  /// \brief Record failure with already-simplified arguments.
  template<typename ...Args>
  void recordFailureSimplified(ConstraintLocator *locator,
                               Failure::FailureKind kind,
                               Args &&...args) {
    // If there is no solver state, this failure is unavoidable.
    if (!solverState) {
      auto failure = Failure::create(getAllocator(), locator, kind,
                                     resolvedOverloadSets,
                                     std::forward<Args>(args)...);

      // Debug output.
      if (getASTContext().LangOpts.DebugConstraintSolver) {
        auto &log = getASTContext().TypeCheckerDebug->getStream();
        log.indent(2);
        failure->dump(&TC.Context.SourceMgr, log);
      }

      unavoidableFailures.push_back(failure);
      return;
    }

    // Check whether we've recorded this failure already.
    llvm::FoldingSetNodeID id;
    Failure::Profile(id, locator, kind, resolvedOverloadSets, args...);
    void *insertPos = nullptr;
    auto failure = failures.FindNodeOrInsertPos(id, insertPos);
    if (!failure) {
      // Allocate a new failure and record it.
      failure = Failure::create(getAllocator(), locator, kind,
                                resolvedOverloadSets, args...);
      failures.InsertNode(failure, insertPos);
    }

    // Debug output.
    if (getASTContext().LangOpts.DebugConstraintSolver) {
      auto &log = getASTContext().TypeCheckerDebug->getStream();
      log.indent(solverState->depth * 2 + 2);
      failure->dump(&TC.Context.SourceMgr, log);
    }

    return;
  }

  /// \brief Simplifies an argument to the failure by simplifying the type.
  Type simplifyFailureArg(Type type) {
    // FIXME: Should also map type variables back to their corresponding
    // archetypes here.
    return simplifyType(type);
  }

  /// \brief Simplifies an argument to the failure by simplifying the type.
  Type simplifyFailureArg(TypeBase *type) {
    return simplifyType(type);
  }

  /// \brief Simplifies an argument to the failure (a no-op).
  unsigned simplifyFailureArg(unsigned arg) {
    return arg;
  }

  /// \brief Simplifies an argument to the failure (a no-op).
  Identifier simplifyFailureArg(Identifier arg) {
    return arg;
  }

public:
  /// \brief Whether we should be recording failures.
  bool shouldRecordFailures() {
    return !solverState || solverState->recordFailures ||
           TC.Context.LangOpts.DebugConstraintSolver;
  }

  /// \brief Record a failure at the given location with the given kind,
  /// along with any additional arguments to be passed to the failure
  /// constructor.
  template<typename ...Args>
  void recordFailure(ConstraintLocator *locator, Failure::FailureKind kind,
                     Args &&...args) {
    // If we don't want to record failures, don't.
    if (!shouldRecordFailures())
      return;

    recordFailureSimplified(locator, kind,
                            simplifyFailureArg(std::forward<Args>(args))...);
  }

  /// \brief Try to diagnose the problem that caused this constraint system
  /// to fail.
  ///
  /// \returns true if a diagnostic was produced, false otherwise.
  bool diagnose();

  /// \brief Add a newly-allocated constraint after attempting to simplify
  /// it.
  ///
  /// \param isExternallySolved Whether this constraint is being solved
  /// as an eager simplification, outside of the simplify() loop.
  ///
  /// \param simplifyExisting Whether we're simplifying an existing
  /// constraint rather than introducing a new constraint.
  ///
  /// \returns true if this constraint was solved.
  bool addConstraint(Constraint *constraint,
                     bool isExternallySolved = false,
                     bool simplifyExisting = false);

  /// \brief Add a constraint to the constraint system.
  void addConstraint(ConstraintKind kind, Type first, Type second,
                     ConstraintLocator *locator = nullptr) {
    assert(first && "Missing first type");
    assert(second && "Missing second type");
    addConstraint(Constraint::create(*this, kind, first, second, Identifier(),
                                     locator));
  }

  /// Add a constraint that binds an overload set to a specific choice.
  void addBindOverloadConstraint(Type boundTy, OverloadChoice choice,
                                 ConstraintLocator *locator) {
    addConstraint(Constraint::createBindOverload(*this, boundTy, choice, 
                                                 locator));
  }

  /// \brief Add a value member constraint to the constraint system.
  void addValueMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                                ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    assert(memberTy);
    assert(!name.empty());
    addConstraint(Constraint::create(*this, ConstraintKind::ValueMember,
                                     baseTy, memberTy, name, locator));
  }

  /// \brief Add a type member constraint to the constraint system.
  void addTypeMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                               ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    assert(memberTy);
    assert(!name.empty());
    
    addConstraint(Constraint::create(*this, ConstraintKind::TypeMember,
                                     baseTy, memberTy, name, locator));
  }

  /// \brief Add an archetype constraint.
  void addArchetypeConstraint(Type baseTy, ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    addConstraint(Constraint::create(*this, ConstraintKind::Archetype,
                                     baseTy, Type(), Identifier(),
                                         locator));
  }

  /// Retrieve the list of active constraints.
  ConstraintList &getConstraints() { return Constraints; }

  /// \brief Retrieve the representative of the equivalence class containing
  /// this type variable.
  TypeVariableType *getRepresentative(TypeVariableType *typeVar) {
    return typeVar->getImpl().getRepresentative(getSavedBindings());
  }

  /// \brief Merge the equivalence sets of the two type variables.
  ///
  /// Note that both \c typeVar1 and \c typeVar2 must be the
  /// representatives of their equivalence classes, and must be
  /// distinct.
  void mergeEquivalenceClasses(TypeVariableType *typeVar1,
                               TypeVariableType *typeVar2);

  /// \brief Retrieve the fixed type corresponding to the given type variable,
  /// or a null type if there is no fixed type.
  Type getFixedType(TypeVariableType *typeVar) {
    return typeVar->getImpl().getFixedType(getSavedBindings());
  }

  /// Retrieve the fixed type corresponding to a given type variable,
  /// recursively, until we hit something that isn't a type variable
  /// or a type variable that doesn't have a fixed type.
  ///
  /// \param type The type to simplify.
  ///
  /// \param typeVar Will receive the type variable at which simplification 
  /// stopped, which has no fixed type.
  ///
  /// \param wantRValue Whether this routine should look through
  /// lvalues at each step.
  Type getFixedTypeRecursive(Type type, TypeVariableType *&typeVar,
                             bool wantRValue);

  /// \brief Assign a fixed type to the given type variable.
  void assignFixedType(TypeVariableType *typeVar, Type type);

private:
  /// Introduce the constraints associated with the given type variable
  /// into the worklist.
  void addTypeVariableConstraintsToWorkList(TypeVariableType *typeVar);

public:

  /// \brief "Open" the given type by replacing any occurrences of generic
  /// parameter types and dependent member types with fresh type variables.
  ///
  /// \param type The type to open.
  ///
  /// \param dc The declaration context in which the type occurs.
  ///
  /// \param skipProtocolSelfConstraint Whether to skip the constraint on a
  /// protocol's 'Self' type.
  ///
  /// \returns The opened type.
  Type openType(Type type, DeclContext *dc = nullptr,
                bool skipProtocolSelfConstraint = false,
                DependentTypeOpener *opener = nullptr) {
    llvm::DenseMap<CanType, TypeVariableType *> replacements;
    return openType(type, replacements, dc, skipProtocolSelfConstraint, opener);
  }

  /// \brief "Open" the given type by replacing any occurrences of generic
  /// parameter types and dependent member types with fresh type variables.
  ///
  /// \param type The type to open.
  ///
  /// \param replacements The mapping from opened types to the type
  /// variables to which they were opened.
  ///
  /// \param dc The declaration context in which the type occurs.
  ///
  /// \param skipProtocolSelfConstraint Whether to skip the constraint on a
  /// protocol's 'Self' type.
  ///
  /// \param opener Abstract class that assists in opening dependent
  /// types.
  ///
  /// \returns The opened type, or \c type if there are no archetypes in it.
  Type openType(Type type,
                llvm::DenseMap<CanType, TypeVariableType *> &replacements,
                DeclContext *dc = nullptr,
                bool skipProtocolSelfConstraint = false,
                DependentTypeOpener *opener = nullptr);

  /// \brief "Open" the given binding type by replacing any occurrences of
  /// archetypes (including those implicit in unbound generic types) with
  /// fresh type variables.
  ///
  /// This variant of \c openType() tweaks the result from \c openType() to
  /// prefer arrays to slices.
  /// FIXME: This is a bit of a hack.
  ///
  /// \param type The type to open.
  /// \returns The opened type, or \c type if there are no archetypes in it.
  Type openBindingType(Type type, DeclContext *dc = nullptr);

  /// Open the generic parameter list and its requirements, creating
  /// type variables for each of the type parameters.
  void openGeneric(DeclContext *dc,
                   ArrayRef<GenericTypeParamType *> params,
                   ArrayRef<Requirement> requirements,
                   bool skipProtocolSelfConstraint,
                   DependentTypeOpener *opener,
                   llvm::DenseMap<CanType, TypeVariableType *> &replacements);

  /// \brief Retrieve the type of a reference to the given value declaration.
  ///
  /// For references to polymorphic function types, this routine "opens up"
  /// the type by replacing each instance of an archetype with a fresh type
  /// variable.
  ///
  /// \param decl The declarations whose type is being computed.
  ///
  /// \param isTypeReference Whether it's a reference to this declaration
  /// as a type.
  ///
  /// \param isSpecialized Whether this declaration is immediately specialized.
  ///
  /// \returns a pair containing the full opened type (if applicable) and
  /// opened type of a reference to declaration.
  std::pair<Type, Type> getTypeOfReference(
                          ValueDecl *decl,
                          bool isTypeReference,
                          bool isSpecialized,
                          DependentTypeOpener *opener = nullptr);

  /// \brief Retrieve the type of a reference to the given value declaration,
  /// as a member with a base of the given type.
  ///
  /// For references to generic function types or members of generic types,
  /// this routine "opens up" the type by replacing each instance of a generic
  /// parameter with a fresh type variable.
  ///
  /// \param isTypeReference Indicates that we want to refer to the declared
  /// type of the type declaration rather than referring to it as a value.
  ///
  /// \param isDynamicResult Indicates that this declaration was found via
  /// dynamic lookup.
  ///
  /// \returns a pair containing the full opened type (which includes the opened
  /// base) and opened type of a reference to this member.
  std::pair<Type, Type> getTypeOfMemberReference(
                          Type baseTy, ValueDecl *decl,
                          bool isTypeReference,
                          bool isDynamicResult,
                          DependentTypeOpener *opener = nullptr);

  /// \brief Add a new overload set to the list of unresolved overload
  /// sets.
  void addOverloadSet(Type boundType, ArrayRef<OverloadChoice> choices,
                      ConstraintLocator *locator);

  /// \brief Retrieve the allocator used by this constraint system.
  llvm::BumpPtrAllocator &getAllocator() { return Allocator; }

  template <typename It>
  ArrayRef<typename std::iterator_traits<It>::value_type>
  allocateCopy(It start, It end) {
    typedef typename std::iterator_traits<It>::value_type T;
    T *result = (T*)getAllocator().Allocate(sizeof(T)*(end-start),
                                            __alignof__(T));
    unsigned i;
    for (i = 0; start != end; ++start, ++i)
      new (result+i) T(*start);
    return ArrayRef<T>(result, i);
  }

  template<typename T>
  ArrayRef<T> allocateCopy(ArrayRef<T> array) {
    return allocateCopy(array.begin(), array.end());
  }

  /// \brief Generate constraints for the given (unchecked) expression.
  ///
  /// \returns a possibly-sanitized expression, or null if an error occurred.
  Expr *generateConstraints(Expr *E);

  /// \brief Generate constraints for the given top-level expression,
  /// assuming that its children are already type-checked.
  ///
  /// \returns a possibly-sanitized expression, or null if an error occurred.
  Expr *generateConstraintsShallow(Expr *E);

  /// \brief Generate constraints for binding the given pattern to the
  /// value of the given expression.
  ///
  /// \returns a possibly-sanitized initializer, or null if an error occurred.
  Type generateConstraints(Pattern *P, ConstraintLocatorBuilder locator);

  /// \brief The result of attempting to resolve a constraint or set of
  /// constraints.
  enum class SolutionKind : char {
    /// \brief The constraint has been solved completely, and provides no
    /// more information.
    Solved,
    /// \brief The constraint could not be solved at this point.
    Unsolved,
    /// \brief The constraint uncovers an inconsistency in the system.
    Error
  };

  /// \brief Enumerates all of the 'direct' supertypes of the given type.
  ///
  /// The direct supertype S of a type T is a supertype of T (e.g., T < S)
  /// such that there is no type U where T < U and U < S.
  SmallVector<Type, 4> enumerateDirectSupertypes(Type type);

  /// \brief Compute the rvalue type of the given expression, which is the
  /// destination of an assignment statement.
  Type computeAssignDestType(Expr *dest, SourceLoc equalLoc);

private:
  /// \brief Flags that direct type matching.
  enum TypeMatchFlags {
    TMF_None = 0,

    /// \brief Indicates that we are in a context where we should be
    /// generating constraints for any unsolvable problems.
    ///
    /// This flag is automatically introduced when type matching destructures
    /// a type constructor (tuple, function type, etc.), solving that
    /// constraint while potentially generating others.
    TMF_GenerateConstraints = 0x01
  };

  /// \brief Subroutine of \c matchTypes(), which matches up two tuple types.
  ///
  /// \returns the result of performing the tuple-to-tuple conversion.
  SolutionKind matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                         TypeMatchKind kind, unsigned flags,
                                         ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which matches a scalar type to
  /// a tuple type.
  ///
  /// \returns the result of performing the scalar-to-tuple conversion.
  SolutionKind matchScalarToTupleTypes(Type type1, TupleType *tuple2,
                                       TypeMatchKind kind, unsigned flags,
                                       ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which extracts a scalar value from
  /// a single-element tuple type.
  ///
  /// \returns the result of performing the tuple-to-scalar conversion.
  SolutionKind matchTupleToScalarTypes(TupleType *tuple1, Type type2,
                                       TypeMatchKind kind, unsigned flags,
                                       ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which matches up two function
  /// types.
  SolutionKind matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                  TypeMatchKind kind, unsigned flags,
                                  ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which matches up a value to a
  /// superclass.
  SolutionKind matchSuperclassTypes(Type type1, Type type2,
                                    TypeMatchKind kind, unsigned flags,
                                    ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which matches up two types that
  /// refer to the same declaration via their generic arguments.
  SolutionKind matchDeepEqualityTypes(Type type1, Type type2,
                                      ConstraintLocatorBuilder locator);

  /// \brief Subroutine of \c matchTypes(), which matches up a value to an
  /// existential type.
  SolutionKind matchExistentialTypes(Type type1, Type type2,
                                     TypeMatchKind kind, unsigned flags,
                                     ConstraintLocatorBuilder locator);

  /// \brief Attempt to match up types \c type1 and \c type2, which in effect
  /// is solving the given type constraint between these two types.
  ///
  /// \param type1 The first type, which is on the left of the type relation.
  ///
  /// \param type2 The second type, which is on the right of the type relation.
  ///
  /// \param kind The kind of type match being performed, e.g., exact match,
  /// trivial subtyping, subtyping, or conversion.
  ///
  /// \param flags A set of flags composed from the TMF_* constants, which
  /// indicates how
  ///
  /// \param locator The locator that will be used to track the location of
  /// the specific types being matched.
  ///
  /// \returns the result of attempting to solve this constraint.
  SolutionKind matchTypes(Type type1, Type type2, TypeMatchKind kind,
                          unsigned flags, ConstraintLocatorBuilder locator);

public:
  /// \brief Resolve the given overload set to the given choice.
  void resolveOverload(ConstraintLocator *locator, Type boundType,
                       OverloadChoice choice);

  /// \brief Simplify a type, by replacing type variables with either their
  /// fixed types (if available) or their representatives.
  ///
  /// The resulting types can be compared canonically, so long as additional
  /// type equivalence requirements aren't introduced between comparisons.
  Type simplifyType(Type type){
    llvm::SmallPtrSet<TypeVariableType *, 16> substituting;
    return simplifyType(type, substituting);
  }

private:
  /// \brief Simplify a type, by replacing type variables with either their
  /// fixed types (if available) or their representatives.
  ///
  /// \param type the type to be simplified.
  ///
  /// \param substituting the set of type variables that we're already
  /// substituting for. These type variables will not be substituted again,
  /// to avoid infinite recursion.
  ///
  /// The resulting types can be compared canonically, so long as additional
  /// type equivalence requirements aren't introduced between comparisons.
  Type simplifyType(Type type,
                    llvm::SmallPtrSet<TypeVariableType *, 16> &substituting);

  /// \brief Attempt to simplify the given construction constraint.
  ///
  /// \param valueType The type being constructed.
  ///
  /// \param argType The type of the argument, used to call \c
  /// valueType's constructor.
  ///
  /// \param flags Flags that indicate how the constraint should be
  /// simplified.
  /// 
  /// \param locator Locator describing where this construction
  /// occurred.
  SolutionKind simplifyConstructionConstraint(Type valueType, Type argType,
                                              unsigned flags,
                                              ConstraintLocator *locator);

  /// \brief Attempt to simplify the given conformance constraint.
  ///
  /// \param type The type being testing.
  /// \param protocol The protocol to which the type should conform.
  /// \param locator Locator describing where this constraint occurred.
  ///
  /// \param allowNonConformingExistential Allow an existential type that
  /// contains the protocol but does not conform to it (i.e., due to associated
  /// types).
  SolutionKind simplifyConformsToConstraint(Type type, ProtocolDecl *protocol,
                                            ConstraintLocatorBuilder locator,
                                            bool allowNonConformingExistential);

  /// Attempt to simplify a checked-cast constraint.
  SolutionKind simplifyCheckedCastConstraint(Type fromType, Type toType,
                                             ConstraintLocatorBuilder locator);

  /// \brief Attempt to simplify the given member constraint.
  SolutionKind simplifyMemberConstraint(const Constraint &constraint);

  /// \brief Attempt to simplify the ApplicableFunction constraint.
  SolutionKind simplifyApplicableFnConstraint(const Constraint &constraint);

  /// \brief Attempt to simplify the given archetype constraint.
  SolutionKind simplifyArchetypeConstraint(const Constraint &constraint);

  /// \brief Attempt to simplify the given class constraint.
  SolutionKind simplifyClassConstraint(const Constraint &constraint);

  /// \brief Attempt to simplify the given dynamic lookup constraint.
  SolutionKind simplifyDynamicLookupConstraint(const Constraint &constraint);

  /// \brief Simplify the given constaint.
  SolutionKind simplifyConstraint(const Constraint &constraint);

public:
  /// \brief Walks through the list of constraints, collecting the constraints
  /// that directly apply to each representative type variable.
  ///
  /// \param typeVarConstraints will be populated with a list of
  /// representative type variables and the constraints that apply directly
  /// to them.
  ///
  /// \param disjunctions will be populated with the list of disjunction
  /// constraints encountered.
  void collectConstraintsForTypeVariables(
         SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints,
         SmallVectorImpl<Constraint *> &disjunctions);

public:
  /// \brief Simplify the system of constraints, by breaking down complex
  /// constraints into simpler constraints.
  ///
  /// The result of simplification is a constraint system that consisting of
  /// only simple constraints relating type variables to each other or
  /// directly to fixed types. There are no constraints that involve
  /// type constructors on both sides. the simplified constraint system may,
  /// of course, include type variables for which we have constraints but
  /// no fixed type. Such type variables are left to the solver to bind.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool simplify();

private:
  /// \brief Solve the system of constraints after it has already been
  /// simplified.
  ///
  /// \param solutions The set of solutions to this system of constraints.
  ///
  /// \param allowFreeTypeVariables How to bind free type variables in
  /// the solution.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool solveSimplified(SmallVectorImpl<Solution> &solutions,
                       FreeTypeVariableBinding allowFreeTypeVariables);
 public:
  /// \brief Solve the system of constraints.
  ///
  /// \param solutions The set of solutions to this system of constraints.
  ///
  /// \param allowFreeTypeVariables How to bind free type variables in
  /// the solution.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool solve(SmallVectorImpl<Solution> &solutions,
             FreeTypeVariableBinding allowFreeTypeVariables
               = FreeTypeVariableBinding::Disallow);

private:
  // \brief Compare two solutions to the same set of constraints.
  ///
  /// \param cs The constraint system.
  /// \param solutions All of the solutions to the system.
  /// \param diff The differences among the solutions.
  /// \param idx1 The index of the first solution.
  /// \param idx2 The index of the second solution.
  static SolutionCompareResult compareSolutions(ConstraintSystem &cs,
                                                ArrayRef<Solution> solutions,
                                                const SolutionDiff &diff,
                                                unsigned idx1,
                                                unsigned idx2);

public:
  /// \brief Given a set of viable solutions, find the best
  /// solution.
  ///
  /// \param solutions The set of viable solutions to consider.
  ///
  /// \param minimize If true, then in the case where there is no single
  /// best solution, minimize the set of solutions by removing any solutions
  /// that are identical to or worse than some other solution. This operation
  /// is quadratic.
  ///
  /// \returns The index of the best solution, or nothing if there was no
  /// best solution.
  Optional<unsigned> findBestSolution(SmallVectorImpl<Solution> &solutions,
                                      bool minimize);

  /// \brief Apply a given solution to the expression, producing a fully
  /// type-checked expression.
  Expr *applySolution(const Solution &solution, Expr *expr);

  /// \brief Apply a given solution to the expression to the top-level
  /// expression, producing a fully type-checked expression.
  Expr *applySolutionShallow(const Solution &solution, Expr *expr);

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump() LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");
  void dump(raw_ostream &out);
};

/// \brief Adjust lvalue types within the type of a reference to a declaration.
///
/// For an lvalue type, this routine adds the 'implicit' and 'nonheap' bits to
/// the lvalue.
///
/// For the function type of an assignment operator, makes the first argument
/// an implicit inout(settable).
Type adjustLValueForReference(Type type, bool isAssignment,
                              ASTContext &context);

/// \brief Compute the shuffle required to map from a given tuple type to
/// another tuple type.
///
/// \param fromTuple The tuple type we're converting from.
///
/// \param toTuple The tuple type we're converting to.
///
/// \param sources Will be populated with information about the source of each
/// of the elements for the result tuple. The indices into this array are the
/// indices of the tuple type we're converting to, while the values are
/// either one of the \c TupleShuffleExpr constants or are an index into the
/// source tuple.
///
/// \param variadicArgs Will be populated with all of the variadic arguments
/// that will be placed into the variadic tuple element (i.e., at the index
/// \c where \c consumed[i] is \c TupleShuffleExpr::FirstVariadic). The values
/// are indices into the source tuple.
///
/// \param sourceLabelsAreMandatory True if labels in the source type are
/// mandatory to match; false means to make an effort to match them, but
/// they can also be dropped.
///
/// \returns true if no tuple conversion is possible, false otherwise.
bool computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                         SmallVectorImpl<int> &sources,
                         SmallVectorImpl<unsigned> &variadicArgs,
                         bool sourceLabelsAreMandatory);

/// Given that an expression has tuple type, are labels in that type
/// mandatory or advistory?  Mandatory labels must be matched in the
/// destination type; optional labels can be matched with unlabeled
/// elements.
bool hasMandatoryTupleLabels(Expr *expr);

/// Simplify the given locator by zeroing in on the most specific
/// subexpression described by the locator.
///
/// This routine can also find the corresponding "target" locator, which
/// typically provides the other end of a relational constraint. For example,
/// if the primary locator refers to a function argument, the target locator
/// will be set to refer to the corresponding function parameter.
///
/// \param cs The constraint system in which the locator will be simplified.
///
/// \param locator The locator to simplify.
///
/// \param range1 Will be populated with an "interesting" range.
///
/// \param range2 Will be populated with a second "interesting" range.
///
/// \param targetLocator If non-null, will be set to a locator that describes
/// the target of the input locator.
///
/// \return the simplified locator.
ConstraintLocator *simplifyLocator(ConstraintSystem &cs,
                                   ConstraintLocator *locator,
                                   SourceRange &range1,
                                   SourceRange &range2,
                                   ConstraintLocator **targetLocator = nullptr);

void simplifyLocator(Expr *&anchor,
                     ArrayRef<LocatorPathElt> &path,
                     Expr *&targetAnchor,
                     SmallVectorImpl<LocatorPathElt> &targetPath,
                     SourceRange &range1, SourceRange &range2);

/// Describes the kind of entity to which a locator was resolved.
enum class ResolvedLocatorKind : uint8_t {
  /// The locator could not be resolved.
  Unresolved,
  /// The locator refers to a function.
  Function,
  /// The locator refers to a constructor.
  Constructor,
  /// The locator refers to a parameter of a function.
  Parameter
};

/// The entity to which a locator resolved.
class ResolvedLocator {
  ResolvedLocatorKind kind;
  ValueDecl *decl;

public:
  ResolvedLocator() : kind(ResolvedLocatorKind::Unresolved) { }

  ResolvedLocator(FuncDecl *func)
    : kind(ResolvedLocatorKind::Function), decl(func)
  {
  }

  ResolvedLocator(ConstructorDecl *constructor)
    : kind(ResolvedLocatorKind::Constructor), decl(constructor)
  {
  }

  ResolvedLocator(VarDecl *param)
    : kind(ResolvedLocatorKind::Parameter), decl(param)
  {
  }
  

  /// Determine the kind of entity to which the locator resolved.
  ResolvedLocatorKind getKind() const { return kind; }

  /// Retrieve the declaration to which the locator resolved.
  ValueDecl *getDecl() const { return decl; }

  explicit operator bool() const {
    return getKind() != ResolvedLocatorKind::Unresolved;
  }
};

/// Resolve a locator to the specific declaration it references, if possible.
///
/// \param cs The constraint system in which the locator will be resolved.
///
/// \param locator The locator to resolve.
///
/// \param findOvlChoice A function that searches for the overload choice
/// associated with the given locator, or an empty optional if there is no such
/// overload.
///
/// \returns the entity to which the locator resolved.
///
/// FIXME: It would be more natural to express the result as a locator.
ResolvedLocator resolveLocatorToDecl(
                  ConstraintSystem &cs,
                  ConstraintLocator *locator,
                  std::function<Optional<OverloadChoice>(ConstraintLocator *)>
                    findOvlChoice);

} // end namespace constraints

template<typename ...Args>
TypeVariableType *TypeVariableType::getNew(const ASTContext &C, unsigned ID,
                                           Args &&...args) {
  // Allocate memory
  void *mem = C.Allocate(sizeof(TypeVariableType) + sizeof(Implementation),
                         alignof(TypeVariableType),
                         AllocationArena::ConstraintSolver);

  // Construct the type variable.
  auto *result = ::new (mem) TypeVariableType(C, ID);

  // Construct the implementation object.
  new (result+1) TypeVariableType::Implementation(std::forward<Args>(args)...);

  return result;
}

} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_SYSTEM_H
