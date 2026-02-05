//===--- TypeVariableType.h - Type Variable Implementation ------*- C++ -*-===//
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
//
// This file provides the constraint-based type checker, anchored by the
// \c ConstraintSystem class, which provides type checking and type
// inference for expressions.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_TYPE_VARIABLE_TYPE_H
#define SWIFT_SEMA_TYPE_VARIABLE_TYPE_H

#include "swift/AST/ASTContext.h"
#include "swift/AST/Expr.h"
#include "swift/AST/Types.h"
#include "swift/Sema/ConstraintGraph.h"
#include "swift/Sema/ConstraintLocator.h"

namespace swift {

/// Options that describe how a type variable can be used.
enum TypeVariableOptions {
  /// Whether the type variable can be bound to an lvalue type or not.
  TVO_CanBindToLValue = 0x01,

  /// Whether the type variable can be bound to an inout type or not.
  TVO_CanBindToInOut = 0x02,

  /// Whether the type variable can be bound to a non-escaping type or not.
  TVO_CanBindToNoEscape = 0x04,

  /// Whether the type variable can be bound to a hole or not.
  TVO_CanBindToHole = 0x08,

  /// Whether a more specific deduction for this type variable implies a
  /// better solution to the constraint system.
  TVO_PrefersSubtypeBinding = 0x10,

  /// Whether the type variable can be bound to a pack type or not.
  TVO_CanBindToPack = 0x20,

  /// Whether the type variable can be bound only to a pack expansion type.
  TVO_PackExpansion = 0x40,
};

/// The implementation object for a type variable used within the
/// constraint-solving type checker.
///
/// The implementation object for a type variable contains information about
/// the type variable, where it was generated, what protocols it must conform
/// to, what specific types it might be and, eventually, the fixed type to
/// which it is assigned.
class TypeVariableType::Implementation {
  /// The locator that describes where this type variable was generated.
  constraints::ConstraintLocator *locator;

  /// Either the parent of this type variable within an equivalence
  /// class of type variables, or the fixed type to which this type variable
  /// type is bound.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

  /// The corresponding node in the constraint graph.
  constraints::ConstraintGraphNode *GraphNode = nullptr;

  /// Temporary state for ConstraintGraph::computeConnectedComponents(),
  /// stored inline for performance.
  llvm::PointerIntPair<TypeVariableType *, 1, unsigned> Component;

  friend class constraints::SolverTrail;

public:
  /// Retrieve the type variable associated with this implementation.
  TypeVariableType *getTypeVariable() const {
    return reinterpret_cast<TypeVariableType *>(
        const_cast<Implementation *>(this)) - 1;
  }

  explicit Implementation(constraints::ConstraintLocator *locator,
                          unsigned options)
    : locator(locator), ParentOrFixed(getTypeVariable()) {
    getTypeVariable()->Bits.TypeVariableType.Options = options;
  }

  /// Retrieve the unique ID corresponding to this type variable.
  unsigned getID() const { return getTypeVariable()->getID(); }

  unsigned getRawOptions() const {
    return getTypeVariable()->Bits.TypeVariableType.Options;
  }

  void setRawOptions(unsigned bits) {
    getTypeVariable()->Bits.TypeVariableType.Options = bits;
    assert(getTypeVariable()->Bits.TypeVariableType.Options == bits
           && "Truncation");
  }

  /// Whether this type variable can bind to an LValueType.
  bool canBindToLValue() const { return getRawOptions() & TVO_CanBindToLValue; }

  /// Whether this type variable can bind to an InOutType.
  bool canBindToInOut() const { return getRawOptions() & TVO_CanBindToInOut; }

  /// Whether this type variable can bind to a noescape FunctionType.
  bool canBindToNoEscape() const { return getRawOptions() & TVO_CanBindToNoEscape; }

  /// Whether this type variable can bind to a PlaceholderType.
  bool canBindToHole() const { return getRawOptions() & TVO_CanBindToHole; }

  /// Whether this type variable can bind to a PackType.
  bool canBindToPack() const { return getRawOptions() & TVO_CanBindToPack; }

  /// Whether this type variable can bind only to PackExpansionType.
  bool isPackExpansion() const { return getRawOptions() & TVO_PackExpansion; }

  /// Whether this type variable prefers a subtype binding over a supertype
  /// binding.
  bool prefersSubtypeBinding() const {
    return getRawOptions() & TVO_PrefersSubtypeBinding;
  }

  /// Retrieve the corresponding node in the constraint graph.
  constraints::ConstraintGraphNode *getGraphNode() const { return GraphNode; }

  /// Set the corresponding node in the constraint graph.
  void setGraphNode(constraints::ConstraintGraphNode *newNode) { 
    GraphNode = newNode; 
  }
  
  /// Check whether this type variable either has a representative that
  /// is not itself or has a fixed type binding.
  bool hasRepresentativeOrFixed() const {
    // If we have a fixed type, we're done.
    if (!isa<TypeVariableType *>(ParentOrFixed))
      return true;

    // Check whether the representative is different from our own type
    // variable.
    return cast<TypeVariableType *>(ParentOrFixed) != getTypeVariable();
  }

  /// Low-level accessor; use getRepresentative() or getFixedType() instead.
  llvm::PointerUnion<TypeVariableType *, TypeBase *>
  getRepresentativeOrFixed() const {
    return ParentOrFixed;
  }

  /// Record the current type-variable binding.
  void recordBinding(constraints::SolverTrail &trail) {
    trail.recordChange(constraints::SolverTrail::Change::UpdatedTypeVariable(
        getTypeVariable(), ParentOrFixed, getRawOptions()));
  }

  /// Retrieve the locator describing where this type variable was
  /// created.
  constraints::ConstraintLocator *getLocator() const {
    return locator;
  }

  /// Retrieve the generic parameter opened by this type variable.
  GenericTypeParamType *getGenericParameter() const;

  /// Returns the \c ExprKind of this type variable if it's the type of an
  /// atomic literal expression, meaning the literal can't be composed of subexpressions.
  /// Otherwise, returns \c None.
  std::optional<ExprKind> getAtomicLiteralKind() const;

  /// Determine whether this type variable represents a closure type.
  bool isClosureType() const;

  /// Determine whether this type variable represents a type of tap expression.
  bool isTapType() const;

  /// Determine whether this type variable represents one of the
  /// parameter types associated with a closure.
  bool isClosureParameterType() const;

  /// Determine whether this type variable represents a closure result type.
  bool isClosureResultType() const;

  /// Determine whether this type variable represents
  /// a type of a key path expression.
  bool isKeyPathType() const;

  /// Determine whether this type variable represents a root type of a key path
  /// expression.
  bool isKeyPathRoot() const;

  /// Determine whether this type variable represents a value type of a key path
  /// expression.
  bool isKeyPathValue() const;

  /// Determine whether this type variable represents an index parameter of
  /// a special `subscript(keyPath:)` subscript.
  bool isKeyPathSubscriptIndex() const;

  /// Determine whether this type variable represents a subscript result type.
  bool isSubscriptResultType() const;

  /// Determine whether this type variable represents an opened
  /// type parameter pack.
  bool isParameterPack() const;

  /// Determine whether this type variable represents a code completion
  /// expression.
  bool isCodeCompletionToken() const;

  /// Determine whether this type variable represents an opened opaque type.
  bool isOpaqueType() const;

  /// Determine whether this type variable represents a type of a collection
  /// literal (represented by `ArrayExpr` and `DictionaryExpr` in AST).
  bool isCollectionLiteralType() const;

  /// Determine whether this type variable represents a literal such
  /// as an integer value, a floating-point value with and without a sign.
  bool isNumberLiteralType() const;

  /// Determine whether this type variable represents a result type of a
  /// function call.
  bool isFunctionResult() const;

  /// Determine whether this type variable represents a type of the ternary
  /// operator.
  bool isTernary() const;

  /// Retrieve the representative of the equivalence class to which this
  /// type variable belongs.
  ///
  /// \param trail The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  TypeVariableType *
  getRepresentative(constraints::SolverTrail *trail) {
    // Find the representative type variable.
    auto result = getTypeVariable();
    Implementation *impl = this;
    while (isa<TypeVariableType *>(impl->ParentOrFixed)) {
      // Extract the representative.
      auto nextTV = cast<TypeVariableType *>(impl->ParentOrFixed);
      if (nextTV == result)
        break;

      result = nextTV;
      impl = &nextTV->getImpl();
    }

    if (impl == this || !trail || trail->isUndoActive())
      return result;

    // Perform path compression.
    impl = this;
    while (isa<TypeVariableType *>(impl->ParentOrFixed)) {
      // Extract the representative.
      auto nextTV = cast<TypeVariableType *>(impl->ParentOrFixed);
      if (nextTV == result)
        break;

      // Record the state change.
      impl->recordBinding(*trail);

      impl->ParentOrFixed = result;
      impl = &nextTV->getImpl();
    }

    return result;
  }

  /// Merge the equivalence class of this type variable with the
  /// equivalence class of another type variable.
  ///
  /// \param other The type variable to merge with.
  ///
  /// \param trail The record of state changes.
  void mergeEquivalenceClasses(TypeVariableType *other,
                               constraints::SolverTrail *trail) {
    ASSERT(getID() < other->getImpl().getID());

    auto otherRep = other->getImpl().getRepresentative(trail);
    if (trail)
      otherRep->getImpl().recordBinding(*trail);
    otherRep->getImpl().ParentOrFixed = getTypeVariable();

    if (canBindToLValue() && !otherRep->getImpl().canBindToLValue()) {
      if (trail)
        recordBinding(*trail);
      getTypeVariable()->Bits.TypeVariableType.Options &= ~TVO_CanBindToLValue;
    }

    if (canBindToInOut() && !otherRep->getImpl().canBindToInOut()) {
      if (trail)
        recordBinding(*trail);
      getTypeVariable()->Bits.TypeVariableType.Options &= ~TVO_CanBindToInOut;
    }

    if (canBindToNoEscape() && !otherRep->getImpl().canBindToNoEscape()) {
      if (trail)
        recordBinding(*trail);
      getTypeVariable()->Bits.TypeVariableType.Options &= ~TVO_CanBindToNoEscape;
    }

    if (canBindToPack() && !otherRep->getImpl().canBindToPack()) {
      if (trail)
        recordBinding(*trail);
      getTypeVariable()->Bits.TypeVariableType.Options &= ~TVO_CanBindToPack;
    }
  }

  /// Retrieve the fixed type that corresponds to this type variable,
  /// if there is one.
  ///
  /// \returns the fixed type associated with this type variable, or a null
  /// type if there is no fixed type.
  ///
  /// \param trail The record of changes made by retrieving the representative,
  /// which can happen due to path compression. If null, path compression is
  /// not performed.
  Type getFixedType(constraints::SolverTrail *trail) {
    // Find the representative type variable.
    auto rep = getRepresentative(trail);
    Implementation &repImpl = rep->getImpl();

    // Return the bound type if there is one, otherwise, null.
    return repImpl.ParentOrFixed.dyn_cast<TypeBase *>();
  }

  /// Assign a fixed type to this equivalence class.
  void assignFixedType(Type type,
                       constraints::SolverTrail *trail) {
    assert((!getFixedType(nullptr) ||
            getFixedType(nullptr)->isEqual(type)) &&
           "Already has a fixed type!");
    auto rep = getRepresentative(trail);
    if (trail)
      rep->getImpl().recordBinding(*trail);
    rep->getImpl().ParentOrFixed = type.getPointer();
  }

  void setCanBindToLValue(constraints::SolverTrail *trail,
                          bool enabled) {
    auto &impl = getRepresentative(trail)->getImpl();
    if (trail)
      impl.recordBinding(*trail);

    if (enabled)
      impl.getTypeVariable()->Bits.TypeVariableType.Options |=
          TVO_CanBindToLValue;
    else
      impl.getTypeVariable()->Bits.TypeVariableType.Options &=
          ~TVO_CanBindToLValue;
  }

  void setCanBindToNoEscape(constraints::SolverTrail *trail,
                            bool enabled) {
    auto &impl = getRepresentative(trail)->getImpl();
    if (trail)
      impl.recordBinding(*trail);

    if (enabled)
      impl.getTypeVariable()->Bits.TypeVariableType.Options |=
          TVO_CanBindToNoEscape;
    else
      impl.getTypeVariable()->Bits.TypeVariableType.Options &=
          ~TVO_CanBindToNoEscape;
  }

  void enableCanBindToHole(constraints::SolverTrail *trail) {
    auto &impl = getRepresentative(trail)->getImpl();
    if (trail)
      impl.recordBinding(*trail);

    impl.getTypeVariable()->Bits.TypeVariableType.Options |= TVO_CanBindToHole;
  }

  void setComponent(TypeVariableType *parent) {
    Component.setPointerAndInt(parent, /*valid=*/false);
  }

  TypeVariableType *getComponent() const {
    auto *rep = getTypeVariable();
    while (rep != rep->getImpl().Component.getPointer())
      rep = rep->getImpl().Component.getPointer();

    // Path compression
    if (rep != getTypeVariable()) {
      const_cast<TypeVariableType::Implementation *>(this)
          ->Component.setPointer(rep);
    }

    return rep;
  }

  bool isValidComponent() const {
    ASSERT(Component.getPointer() == getTypeVariable());
    return Component.getInt();
  }

  bool markValidComponent() {
    if (Component.getInt())
      return false;
    ASSERT(Component.getPointer() == getTypeVariable());
    Component.setInt(1);
    return true;
  }

  /// Print the type variable to the given output stream.
  void print(llvm::raw_ostream &OS);

private:
  StringRef getTypeVariableOptions(TypeVariableOptions TVO) const {
  #define ENTRY(Kind, String) case TypeVariableOptions::Kind: return String
    switch (TVO) {
    ENTRY(TVO_CanBindToLValue, "lvalue");
    ENTRY(TVO_CanBindToInOut, "inout");
    ENTRY(TVO_CanBindToNoEscape, "noescape");
    ENTRY(TVO_CanBindToHole, "hole");
    ENTRY(TVO_PrefersSubtypeBinding, "");
    ENTRY(TVO_CanBindToPack, "pack");
    ENTRY(TVO_PackExpansion, "pack expansion");
    }
  #undef ENTRY
  }
};

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

}  // end namespace swift

#endif // SWIFT_SEMA_TYPE_VARIABLE_TYPE_H
