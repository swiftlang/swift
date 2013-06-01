//===--- ConstraintSystem.h - Constraint-based Type Checking --------------===//
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
#include "swift/Basic/LLVM.h"
#include "swift/AST/ASTContext.h"
#include "swift/AST/NameLookup.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/Support/ErrorHandling.h"
#include <cstddef>

namespace swift {

class Expr;
enum class LiteralKind : char;

namespace constraints {

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
  
} // end namespace constraints

/// \brief The implementation object for a type variable used within the
/// constraint-solving type checker.
///
/// The implementation object for a type variable contains information about
/// the type variable, where it was generated, what protocols it must conform
/// to, what specific types it might be and, eventually, the fixed type to
/// which it is assigned.
class TypeVariableType::Implementation {
  /// \brief The unique number assigned to this type variable.
  unsigned ID;

  /// \brief The archetype that this type variable describes.
  ArchetypeType *Archetype;

  /// \brief Either the parent of this type variable within an equivalence
  /// class of type variables, or the fixed type to which this type variable
  /// type is bound.
  llvm::PointerUnion<TypeVariableType *, TypeBase *> ParentOrFixed;

  friend class constraints::SavedTypeVariableBinding;

public:
  explicit Implementation(unsigned ID)
    : ID(ID), Archetype(nullptr),
      ParentOrFixed(getTypeVariable()) { }

  explicit Implementation(unsigned ID, Expr *TheExpr)
    : ID(ID), Archetype(nullptr),
      ParentOrFixed(getTypeVariable()) { }

  explicit Implementation(unsigned ID, ArchetypeType *Archetype)
    : ID(ID), Archetype(Archetype),
      ParentOrFixed(getTypeVariable()) { }

  /// \brief Retrieve the unique ID corresponding to this type variable.
  unsigned getID() const { return ID; }

  /// \brief Retrieve the archetype that this type variable replaced.
  ArchetypeType *getArchetype() const { return Archetype; }

  /// \brief Retrieve the type variable associated with this implementation.
  TypeVariableType *getTypeVariable() {
    return reinterpret_cast<TypeVariableType *>(this) - 1;
  }

  /// \brief Record the current type-variable binding.
  void recordBinding(constraints::SavedTypeVariableBindings &record) {
    record.push_back(constraints::SavedTypeVariableBinding(getTypeVariable()));
  }

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
    } else {
      auto rep = getRepresentative(record);
      if (record)
        rep->getImpl().recordBinding(*record);
      rep->getImpl().ParentOrFixed = other;
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
    Implementation *impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();

      // If we found the representative, there is no fixed type.
      if (nextTV == impl->getTypeVariable()) {
        return Type();
      }

      impl = &nextTV->getImpl();
    }

    Type result = impl->ParentOrFixed.get<TypeBase *>();
    if (impl == this || !record)
      return result;

    // Perform path compression.
    impl = this;
    while (impl->ParentOrFixed.is<TypeVariableType *>()) {
      // Extract the representative.
      auto nextTV = impl->ParentOrFixed.get<TypeVariableType *>();
      if (nextTV == impl->getTypeVariable())
        return result;

      impl->recordBinding(*record);
      impl->ParentOrFixed = result.getPointer();
      impl = &nextTV->getImpl();
    }

    return result;
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
  void print(llvm::raw_ostream &Out);
};

namespace constraints {

/// \brief Describes the kind of constraint placed on one or more types.
enum class ConstraintKind : char {
  /// \brief The two types must be bound to the same type. This is the only
  /// truly symmetric constraint.
  Bind,
  /// \brief The two types must be bound to the same type, dropping
  /// lvalueness when comparing a type variable to a type.
  Equal,
  /// \brief The first type is the rvalue of the second type.
  EqualRvalue,
  /// \brief The first type is a "trivial" subtype of the second type,
  /// meaning that it is a subtype that is also guaranteed to have the same
  // in-memory representation.
  TrivialSubtype,
  /// \brief The first type is a subtype of the second type, i.e., a value
  /// of the type of the first type can be used wherever a value of the
  /// second type is expected.
  Subtype,
  /// \brief The first type is convertible to the second type.
  Conversion,
  /// \brief The first type can be converted to the second type or can be
  /// used as an argument to a constructor for the second (non-reference)
  /// type.
  Construction,
  /// \brief The first type must conform to the second type (which is a
  /// protocol type).
  ConformsTo,
  /// \brief The first type is the type of a literal. There is no second
  /// type.
  Literal,
  /// \brief The first type has a member with the given name, and the
  /// type of that member, when referenced as a value, is the second type.
  ValueMember,
  /// \brief The first type has a type member with the given name, and the
  /// type of that member, when referenced as a type, is the second type.
  TypeMember,
  /// \brief The first type must be an archetype.
  Archetype
};

/// \brief Classification of the different kinds of constraints.
enum class ConstraintClassification : char {
  /// \brief A relational constraint, which relates two types.
  Relational,

  /// \brief A literal constraint, which specifies that a type must be
  /// able to be created from a particular kind of literal.
  Literal,

  /// \brief A member constraint, which names a member of a type and assigns
  /// it a reference type.
  Member,

  /// \brief An archetype constraint, which simply requires that the type
  /// variable be bound to an archetype.
  Archetype
};

/// \brief Locates a given constraint within the expression being
/// type-checked, which may refer down into subexpressions and parts of
/// the types of those subexpressions.
///
/// Each locator as anchored at some expression, e.g., (3, (x, 3.14)),
/// and contains a path that digs further into the type of that expression.
/// For example, the path "tuple element #1" -> "tuple element #0" with the
/// above expression would refer to 'x'. If 'x' had function type, the
/// path could be further extended with either "-> argument" or "-> result",
/// to indicate constraints on its argument or result type.
class ConstraintLocator : public llvm::FoldingSetNode {
public:
  /// \brief Describes the kind of a a particular path element, e.g.,
  /// "tuple element", "call result", "base of member lookup", etc.
  enum PathElementKind : unsigned char {
    /// \brief The argument of function application.
    ApplyArgument,
    /// \brief The function being applied.
    ApplyFunction,
    /// \brief The argument type of a function.
    FunctionArgument,
    /// \brief The result type of a function.
    FunctionResult,
    /// \brief A tuple element referenced by position.
    TupleElement,
    /// \brief A tuple element referenced by name.
    NamedTupleElement,
    /// \brief A generic argument.
    /// FIXME: Add support for named generic arguments?
    GenericArgument,
    /// \brief A member.
    /// FIXME: Do we need the actual member name here?
    Member,
    /// \brief The base of a member expression.
    MemberRefBase,
    /// \brief The lookup for a subscript member.
    SubscriptMember,
    /// \brief The index of a subscript expression.
    SubscriptIndex,
    /// \brief The result of a subscript expression.
    SubscriptResult,
    /// \brief An argument to string interpolation.
    InterpolationArgument,
    /// \brief The lookup for a constructor member.
    ConstructorMember,
    /// \brief Address of subexpression.
    AddressOf,
    /// \brief Rvalue adjustment.
    RvalueAdjustment,
    /// \brief The result of an explicit closure expression.
    ClosureResult,
    /// \brief The parent of a nested type.
    ParentType,
    /// \brief The instance of a metatype type.
    InstanceType,
    /// \brief The element of an array type.
    ArrayElementType,
    /// \brief The object type of an lvalue type.
    LvalueObjectType,
    /// \brief The scalar type of a tuple type.
    ScalarToTuple,
    /// \brief The load of an lvalue.
    Load,
    /// \brief The lookup for a conversion member.
    ConversionMember,
    /// \brief The conversion result.
    ConversionResult,
    /// \brief The 'then' branch of a ternary expression.
    IfThen,
    /// \brief The 'else' branch of a ternary expression.
    IfElse,
    /// \brief The source of an assignment.
    AssignSource
  };

  /// \brief Determine whether the given path element kind has an associated
  /// value.
  static bool pathElementHasValue(PathElementKind kind) {
    switch (kind) {
    case ApplyArgument:
    case ApplyFunction:
    case FunctionArgument:
    case FunctionResult:
    case Member:
    case MemberRefBase:
    case SubscriptIndex:
    case SubscriptMember:
    case SubscriptResult:
    case ConstructorMember:
    case AddressOf:
    case RvalueAdjustment:
    case ClosureResult:
    case ParentType:
    case InstanceType:
    case ArrayElementType:
    case LvalueObjectType:
    case ScalarToTuple:
    case Load:
    case ConversionMember:
    case ConversionResult:
    case IfThen:
    case IfElse:
    case AssignSource:
      return false;

    case GenericArgument:
    case InterpolationArgument:
    case NamedTupleElement:
    case TupleElement:
      return true;
    }
  }

  /// \brief One element in the path of a locator, which can include both
  /// a kind (PathElementKind) and a value used to describe specific
  /// kinds further (e.g., the position of a tuple element).
  class PathElement {
    /// \brief The kind of path element.
    PathElementKind kind : 8;

    ///\ brief The value of the path element, if applicable.
    unsigned value : 24;

    PathElement(PathElementKind kind, unsigned value)
      : kind(kind), value(value) { }

    friend class ConstraintLocator;
    
  public:
    PathElement(PathElementKind kind) : kind(kind), value(0) {
      assert(!pathElementHasValue(kind) && "Path element requires value");
    }

    /// \brief Retrieve a path element for a tuple element referred to by
    /// its position.
    static PathElement getTupleElement(unsigned position) {
      return PathElement(TupleElement, position);
    }

    /// \brief Retrieve a path element for a tuple element referred to by
    /// its name.
    static PathElement getNamedTupleElement(unsigned position) {
      return PathElement(NamedTupleElement, position);
    }

    /// \brief Retrieve a path element for a generic argument referred to by
    /// its position.
    static PathElement getGenericArgument(unsigned position) {
      return PathElement(GenericArgument, position);
    }

    /// \brief Retrieve a path element for an argument to string
    /// interpolation.
    static PathElement getInterpolationArgument(unsigned position) {
      return PathElement(InterpolationArgument, position);
    }

    /// \brief Retrieve the kind of path element.
    PathElementKind getKind() const { return kind; }

    /// \brief Retrieve the value associated with this path element,
    /// if it has one.
    unsigned getValue() const {
      assert(pathElementHasValue(kind) && "No value in path element!");
      return value;
    }
  };

  /// \brief Retrieve the expression that anchors this locator.
  Expr *getAnchor() const { return anchor; }
  
  /// \brief Retrieve the path that extends from the anchor to a specific
  /// subcomponent.
  ArrayRef<PathElement> getPath() const {
    // FIXME: Alignment.
    return llvm::makeArrayRef(reinterpret_cast<const PathElement *>(this + 1),
                              numPathElements);
  }

  /// \brief Determines whether this locator has a "simple" path, without
  /// any transformations that break apart types.
  bool hasSimplePath() const {
    for (auto elt : getPath()) {
      switch (elt.getKind()) {
      case AddressOf:
      case ApplyArgument:
      case ApplyFunction:
      case ArrayElementType:
      case ClosureResult:
      case ConstructorMember:
      case ConversionMember:
      case ConversionResult:
      case FunctionArgument:
      case FunctionResult:
      case InstanceType:
      case Load:
      case LvalueObjectType:
      case Member:
      case MemberRefBase:
      case ParentType:
      case RvalueAdjustment:
      case ScalarToTuple:
      case SubscriptIndex:
      case SubscriptMember:
      case SubscriptResult:
      case IfThen:
      case IfElse:
      case AssignSource:
        continue;

      case GenericArgument:
      case InterpolationArgument:
      case NamedTupleElement:
      case TupleElement:
        return false;
      }
    }

    return true;
  }

  /// \brief Produce a profile of this locator, for use in a folding set.
  static void Profile(llvm::FoldingSetNodeID &id, Expr *anchor,
                      ArrayRef<PathElement> path) {
    id.AddPointer(anchor);
    id.AddInteger(path.size());
    for (auto elt : path) {
      id.AddInteger(elt.getKind());
      if (pathElementHasValue(elt.getKind()))
        id.AddInteger(elt.getValue());
    }
  }
  
  /// \brief Produce a profile of this locator, for use in a folding set.
  void Profile(llvm::FoldingSetNodeID &id) {
    Profile(id, anchor, getPath());
  }

  /// \brief Produce a debugging dump of this locator.
  void dump(llvm::SourceMgr *sm) LLVM_ATTRIBUTE_USED;

private:
  /// \brief Initialize a constraint locator with an anchor and a path.
  ConstraintLocator(Expr *anchor, ArrayRef<PathElement> path)
    : anchor(anchor), numPathElements(path.size()) {
    // FIXME: Alignment.
    std::copy(path.begin(), path.end(),
              reinterpret_cast<PathElement *>(this + 1));
  }

  /// \brief Create a new locator from an anchor and an array of path
  /// elements.
  ///
  /// Note that this routine only handles the allocation and initialization
  /// of the locator. The ConstraintSystem object is responsible for
  /// uniquing via the FoldingSet.
  static ConstraintLocator *create(llvm::BumpPtrAllocator &allocator,
                                   Expr *anchor,
                                   ArrayRef<PathElement> path) {
    // FIXME: Alignment.
    unsigned size = sizeof(ConstraintLocator)
                  + path.size() * sizeof(PathElement);
    void *mem = allocator.Allocate(size, alignof(ConstraintLocator));
    return new (mem) ConstraintLocator(anchor, path);
  }

  /// \brief The expression at which this locator is anchored.
  Expr *anchor;

  /// \brief The number of path elements in this locator.
  ///
  /// The actual path elements are stored after the locator.
  unsigned numPathElements;

  friend class ConstraintSystem;
};

typedef ConstraintLocator::PathElement LocatorPathElt;

/// \brief A simple stack-only builder object that constructs a
/// constraint locator without allocating memory.
///
/// Use this object to build a path when passing components down the
/// stack, e.g., when recursively breaking apart types as in \c matchTypes().
class ConstraintLocatorBuilder {
  /// \brief The constraint locator that this builder extends or the
  /// previous builder in the chain.
  llvm::PointerUnion<ConstraintLocator *, ConstraintLocatorBuilder *>
    previous;

  /// \brief The current path element, if there is one.
  Optional<LocatorPathElt> element;

  ConstraintLocatorBuilder(llvm::PointerUnion<ConstraintLocator *,
                                              ConstraintLocatorBuilder *>
                             previous,
                           LocatorPathElt element)
    : previous(previous), element(element) { }

public:
  ConstraintLocatorBuilder(ConstraintLocator *locator)
    : previous(locator), element() { }

  /// \brief Retrieve a new path with the given path element added to it.
  ConstraintLocatorBuilder withPathElement(LocatorPathElt newElt) {
    if (!element)
      return ConstraintLocatorBuilder(previous, newElt);

    return ConstraintLocatorBuilder(this, newElt);
  }

  /// \brief Determine whether this builder has an empty path.
  bool hasEmptyPath() const {
    return !element;
  }

  /// \brief Retrieve the base constraint locator, on which this builder's
  /// path is based.
  ConstraintLocator *getBaseLocator() const {
    for (auto prev = this;
         prev;
         prev = prev->previous.dyn_cast<ConstraintLocatorBuilder *>()) {
      if (auto locator = prev->previous.dyn_cast<ConstraintLocator *>())
        return locator;
    }

    return nullptr;
  }

  /// \brief Retrieve the components of the complete locator, which includes
  /// the anchor expression and the path.
  Expr *getLocatorParts(llvm::SmallVectorImpl<LocatorPathElt> &path) const {
    for (auto prev = this;
         prev;
         prev = prev->previous.dyn_cast<ConstraintLocatorBuilder *>()) {
      // If there is an element at this level, add it.
      if (prev->element)
        path.push_back(*prev->element);

      if (auto locator = prev->previous.dyn_cast<ConstraintLocator *>()) {
        // We found the end of the chain. Reverse the path we've built up,
        // then prepend the locator's path.
        std::reverse(path.begin(), path.end());
        path.insert(path.begin(),
                    locator->getPath().begin(),
                    locator->getPath().end());
        return locator->getAnchor();
      }
    }

    // We only get here if there was no locator; fail gracefully.
    path.clear();
    return nullptr;
  }
};

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
    /// \brief Lvalue type qualifiers mismatch.
    LValueQualifiers,
    /// \brief The first type doesn't conform to a protocol in the second
    /// type.
    DoesNotConformToProtocol,
    /// \brief The first type does not have a member with the given name.
    DoesNotHaveMember,
    /// \brief The type is not an archetype.
    IsNotArchetype,
    /// \brief The type is not a literal type.
    IsNotLiteralType,
  };

private:
  /// \brief The kind of failure this describes.
  FailureKind kind : 8;

  /// \brief A value, if used.
  unsigned value : 32;

  /// \brief Describes the location of this failure.
  ConstraintLocator *locator;

  /// \brief The first type.
  Type first;

  /// \brief The second value, which may be one of several things (type,
  /// member name, literal kind, etc.).
  union {
    TypeBase *type;
    void *name;
    LiteralKind literal;
  } second;

public:
  /// \brief Retrieve the failure kind.
  FailureKind getKind() const { return kind; }

  /// \brief Retrieve the failure locator.
  ConstraintLocator *getLocator() const {
    return locator;
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

  /// \brief Retrieve the literal kind.
  LiteralKind getLiteralKind() const {
    return second.literal;
  }

  /// \brief Retrieve the value.
  unsigned getValue() const { return value; }

  /// \brief Profile the given failure.
  void Profile(llvm::FoldingSetNodeID &id) {
    switch (kind) {
    case FunctionAutoclosureMismatch:
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
      return Profile(id, locator, kind, getFirstType(), getSecondType());

    case DoesNotHaveMember:
      return Profile(id, locator, kind, getFirstType(), getName());

    case IsNotArchetype:
      return Profile(id, locator, kind, getFirstType());

    case IsNotLiteralType:
      return Profile(id, locator, kind, getFirstType(), getLiteralKind());
    }
  }

private:
  friend class ConstraintSystem;

  /// \brief Construct a failure involving one type.
  Failure(ConstraintLocator *locator, FailureKind kind, Type type)
    : kind(kind), value(0), locator(locator), first(type)
  {
    second.type = nullptr;
  }

  /// \brief Construct a failure involving two types and an optional value.
  Failure(ConstraintLocator *locator, FailureKind kind,
          Type type1, Type type2, unsigned value = 0)
    : kind(kind), value(value), locator(locator), first(type1)
  {
    second.type = type2.getPointer();
  }

  /// \brief Construct a failure involving a type and a name.
  Failure(ConstraintLocator *locator, FailureKind kind,
          Type type, Identifier name)
  : kind(kind), value(0), locator(locator), first(type)
  {
    second.name = name.getAsOpaquePointer();
  }

  /// \brief Construct a failure involving a type and a literal kind.
  Failure(ConstraintLocator *locator, FailureKind kind, Type type,
          LiteralKind literal)
  : kind(kind), value(0), locator(locator), first(type)
  {
    second.literal = literal;
  }

  /// \brief Profile a failure involving one type.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind, Type type) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(type.getPointer());
  }

  /// \brief Profile a failure involving two types.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind, Type type1, Type type2) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(type1.getPointer());
    id.AddPointer(type2.getPointer());
  }

  /// \brief Profile a failure involving two types and a value.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind, Type type1, Type type2,
                      unsigned value) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(type1.getPointer());
    id.AddPointer(type2.getPointer());
    id.AddInteger(value);
  }

  /// \brief Profile a failure involving a type and a name.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind, Type type, Identifier name) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(type.getPointer());
    id.AddPointer(name.getAsOpaquePointer());
  }

  /// \brief Profile a failure involving a type and a literal kind.
  static void Profile(llvm::FoldingSetNodeID &id, ConstraintLocator *locator,
                      FailureKind kind, Type type, LiteralKind literal) {
    id.AddPointer(locator);
    id.AddInteger(kind);
    id.AddPointer(type.getPointer());
    id.AddInteger(static_cast<unsigned>(literal));
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


/// \brief A constraint between two type variables.
class Constraint {
  /// \brief The kind of constraint.
  ConstraintKind Kind : 8;

  /// \brief For a literal-type constraint, the kind of literal we're
  /// expecting.
  LiteralKind Literal : 8;

  /// \brief The first type.
  Type First;

  /// \brief The second type.
  Type Second;

  /// \brief If non-null, the name of a member of the first type is that
  /// being related to the second type.
  Identifier Member;

  /// \brief The locator that describes where in the expression this
  /// constraint applies.
  ConstraintLocator *Locator;

  /// \brief Constraints are always allocated within a given constraint
  /// system.
  void *operator new(size_t) = delete;

public:
  Constraint(ConstraintKind Kind, Type First, Type Second, Identifier Member,
             ConstraintLocator *locator)
    : Kind(Kind), First(First), Second(Second), Member(Member),
      Locator(locator)
  {
    switch (Kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::EqualRvalue:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::Construction:
    case ConstraintKind::ConformsTo:
      assert(Member.empty() && "Relational constraint cannot have a member");
      break;

    case ConstraintKind::Literal:
      llvm_unreachable("Wrong constructor for literal constraint");
      break;

    case ConstraintKind::TypeMember:
    case ConstraintKind::ValueMember:
      assert(!Member.empty() && "Member constraint has no member");
      break;

    case ConstraintKind::Archetype:
      assert(Member.empty() && "Archetype constraint cannot have a member");
      assert(Second.isNull() && "Archetype constraint with second type");
      break;
    }
  }

  Constraint(Type type, LiteralKind literal, ConstraintLocator *locator)
    : Kind(ConstraintKind::Literal), Literal(literal), First(type),
      Locator(locator) { }

  /// \brief Determine the kind of constraint.
  ConstraintKind getKind() const { return Kind; }

  /// \brief Determine the classification of this constraint, providing
  /// a broader categorization than \c getKind().
  ConstraintClassification getClassification() const {
    switch (Kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::EqualRvalue:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::Construction:
    case ConstraintKind::ConformsTo:
      return ConstraintClassification::Relational;

    case ConstraintKind::Literal:
      return ConstraintClassification::Literal;

    case ConstraintKind::ValueMember:
    case ConstraintKind::TypeMember:
      return ConstraintClassification::Member;

    case ConstraintKind::Archetype:
      return ConstraintClassification::Archetype;
    }
  }

  /// \brief Retrieve the first type in the constraint.
  Type getFirstType() const { return First; }

  /// \brief Retrieve the second type in the constraint.
  Type getSecondType() const {
    assert(Kind != ConstraintKind::Literal &&
           "No second type for literal constraints");
    return Second;
  }

  /// \brief Determine whether this constraint kind has a second type.
  static bool hasSecondType(ConstraintKind kind) {
    return kind != ConstraintKind::Literal;
  }

  /// \brief Retrieve the protocol in a conformance constraint.
  ProtocolDecl *getProtocol() const {
    assert(Kind==ConstraintKind::ConformsTo && "Not a conformance constraint");
    return Second->castTo<ProtocolType>()->getDecl();
  }

  /// \brief Retrieve the name of the member for a member constraint.
  Identifier getMember() const {
    assert(Kind == ConstraintKind::ValueMember ||
           Kind == ConstraintKind::TypeMember);
    return Member;
  }

  /// \brief Determine whether this constraint kind has a second type.
  static bool hasMember(ConstraintKind kind) {
    return kind == ConstraintKind::ValueMember
        || kind == ConstraintKind::TypeMember;
  }

  /// \brief Determine the kind of literal for a literal constraint.
  LiteralKind getLiteralKind() const {
    assert(Kind == ConstraintKind::Literal && "Not a literal constraint!");
    return Literal;
  }

  /// \brief Retrieve the locator for this constraint.
  ConstraintLocator *getLocator() const { return Locator; }

  void print(llvm::raw_ostream &Out, llvm::SourceMgr *sm);

  void dump(llvm::SourceMgr *sm) LLVM_ATTRIBUTE_USED;

  void *operator new(size_t bytes, ConstraintSystem& cs,
                     size_t alignment = alignof(Constraint)) {
    return ::operator new (bytes, cs, alignment);
  }

  inline void operator delete(void *, const ConstraintSystem &cs, size_t) {}
};

/// \brief The kind of overload choice.
enum class OverloadChoiceKind : int {
  /// \brief The overload choice selects a particular declaration from a
  /// set of declarations.
  Decl,
  /// \brief The overload choice equates the member type with the
  /// base type. Used for unresolved member expressions like ".none" that
  /// refer to oneof members with unit type.
  BaseType,
  /// \brief The overload choice equates the member type with a function
  /// of arbitrary input type whose result type is the base type. Used for
  /// unresolved member expressions like ".internal" that refer to oneof
  /// members with non-unit type.
  FunctionReturningBaseType,
  /// \brief The overload choice equates the member type with a function
  /// from the base type to itself.
  IdentityFunction,
  /// \brief The overload choice indexes into a tuple. Index zero will
  /// have the value of this enumerator, index one will have the value of this
  /// enumerator + 1, and so on. Thus, this enumerator must always be last.
  TupleIndex
};

/// \brief Describes a particular choice within an overload set.
///
/// 
class OverloadChoice {
  /// \brief The base type to be used when referencing the declaration.
  Type Base;

  /// \brief Either the declaration pointer (if the low bit is clear) or the
  /// overload choice kind shifted by 1 with the low bit set.
  uintptr_t DeclOrKind;

public:
  OverloadChoice() : Base(), DeclOrKind() { }

  OverloadChoice(Type base, ValueDecl *value) : Base(base) {
    DeclOrKind = reinterpret_cast<uintptr_t>(value);
    assert((DeclOrKind & (uintptr_t)0x01) == 0 && "Badly aligned decl");
  }

  OverloadChoice(Type base, OverloadChoiceKind kind)
    : Base(base), DeclOrKind((uintptr_t)kind << 1 | (uintptr_t)0x01) {
    assert(base && "Must have a base type for overload choice");
    assert(kind != OverloadChoiceKind::Decl && "wrong constructor for decl");
  }

  OverloadChoice(Type base, unsigned index)
    : Base(base),
      DeclOrKind(((uintptr_t)index
                  + (uintptr_t)OverloadChoiceKind::TupleIndex) << 1
                 | (uintptr_t)0x01) {
    assert(base->getRValueType()->is<TupleType>() && "Must have tuple type");
  }

  /// \brief Retrieve the base type used to refer to the declaration.
  Type getBaseType() const { return Base; }

  /// \brief Determines the kind of overload choice this is.
  OverloadChoiceKind getKind() const {
    if (DeclOrKind & 0x01) {
      uintptr_t value = DeclOrKind >> 1;
      if (value >= (uintptr_t)OverloadChoiceKind::TupleIndex)
        return OverloadChoiceKind::TupleIndex;

      return (OverloadChoiceKind)value;
    }
    
    return OverloadChoiceKind::Decl;
  }

  /// \brief Retrieve the declaraton that corresponds to this overload choice.
  ValueDecl *getDecl() const {
    assert(getKind() == OverloadChoiceKind::Decl && "Not a declaration");
    return reinterpret_cast<ValueDecl *>(DeclOrKind);
  }

  /// \brief Retrieve the tuple index that corresponds to this overload
  /// choice.
  unsigned getTupleIndex() const {
    assert(getKind() == OverloadChoiceKind::TupleIndex);
    return (DeclOrKind >> 1) - (uintptr_t)OverloadChoiceKind::TupleIndex;
  }
};

/// \brief An overload set, which is a set of overloading choices from which
/// only one can be selected.
class OverloadSet {
  /// \brief ID number that uniquely identifies this overload set.
  unsigned ID;

  /// \brief The number of choices in the overload set.
  unsigned NumChoices;

  /// \brief The locator for this overload set.
  ConstraintLocator *Locator;

  /// \brief The type bound by this overload set.
  Type BoundType;
  
  /// \brief Overload sets are always allocated within a given constraint
  /// system.
  void *operator new(size_t) = delete;

  OverloadSet(unsigned ID, ConstraintLocator *locator,
              Type boundType, ArrayRef<OverloadChoice> choices)
    : ID(ID), NumChoices(choices.size()), Locator(locator),
      BoundType(boundType) {
    memcpy(this+1, choices.data(), sizeof(OverloadChoice)*choices.size());
  }

public:
  /// \brief Retrieve the locator that identifies where this overload set
  /// same from.
  ConstraintLocator *getLocator() const { return Locator; }

  /// \brief Retrieve the ID associated with this overload set.
  unsigned getID() const { return ID; }
  
  /// \brief Retrieve the set of choices provided by this overload set.
  ArrayRef<OverloadChoice> getChoices() const {
    return { reinterpret_cast<const OverloadChoice *>(this + 1),
             NumChoices };
  }

  /// \brief Retrieve the type that is bound (via a same-type
  /// constraint) by this overload set.
  Type getBoundType() const { return BoundType; }

  /// \brief Create a new overload set, using (and copying) the given choices.
  static OverloadSet *getNew(ConstraintSystem &CS,
                             Type boundType,
                             ConstraintLocator *locator,
                             ArrayRef<OverloadChoice> choices);
};

/// \brief A representative type variable with the list of constraints
/// that apply to it.
struct TypeVariableConstraints {
  TypeVariableConstraints(TypeVariableType *typeVar)
    : HasNonConcreteConstraints(false), TypeVar(typeVar) {}

  /// \brief Whether there are any non-concrete constraints placed on this
  /// type variable that aren't represented by the stored constraints.
  bool HasNonConcreteConstraints;

  /// \brief The representative type variable.
  TypeVariableType *TypeVar;

  /// \brief The set of constraints "above" the type variable.
  SmallVector<std::pair<Constraint *, Type>, 4> Above;

  /// \brief The set of constraints "below" the type variable.
  SmallVector<std::pair<Constraint *, Type>, 4> Below;

  /// \brief The set of protocol conformance constraints directly applicable
  /// to the type variable.
  SmallVector<Constraint *, 4> ConformsToConstraints;
  
  /// \brief The set of archetype and literal constraints directly
  /// applicable to the type variable T.
  SmallVector<Constraint *, 4> KindConstraints;
};

/// \brief The kind of type matching to perform in matchTypes().
enum class TypeMatchKind : char {
  /// \brief Bind the types together directly.
  BindType,
  /// \brief Require the types to match exactly, but strips lvalueness from
  /// a type when binding to a type variable.
  SameType,
  /// \brief Require the type of the first to match the rvalue type of the
  /// second.
  SameTypeRvalue,
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

/// \brief A complete solution to a constraint system.
///
/// A solution to a constraint system consists of type variable bindings to
/// concrete types for every type variable that is used in the constraint
/// system along with a set of mappings from each constraint locator
/// involving an overload set to the selected overload.
class Solution {
  /// \brief The constraint system this solution solves.
  ConstraintSystem *constraintSystem;

public:
  /// \brief Create a solution for the given constraint system.
  Solution(ConstraintSystem &cs) : constraintSystem(&cs) {}

  // Solution is a non-copyable type for performance reasons.
  Solution(const Solution &other) = delete;
  Solution &operator=(const Solution &other) = delete;

  Solution(Solution &&other)
    : constraintSystem(other.constraintSystem),
      typeBindings(std::move(other.typeBindings)),
      overloadChoices(std::move(other.overloadChoices))
  {
  }

  Solution &operator=(Solution &&other) {
    constraintSystem = other.constraintSystem;
    typeBindings = std::move(other.typeBindings);
    overloadChoices = std::move(other.overloadChoices);
    return *this;
  }

  /// \brief Retrieve the constraint system that this solution solves.
  ConstraintSystem &getConstraintSystem() const { return *constraintSystem; }

  /// \brief The set of type bindings.
  llvm::SmallDenseMap<TypeVariableType *, Type> typeBindings;

  /// \brief The set of overload choices along with their types.
  llvm::SmallDenseMap<ConstraintLocator *,
                      std::pair<OverloadChoice, Type>> overloadChoices;

  /// \brief Simplify the given type by substituting all occurrences of
  /// type variables for their fixed types.
  Type simplifyType(TypeChecker &tc, Type type) const;

  /// \brief Coerce the given expression to the given type.
  ///
  /// This operation cannot fail.
  ///
  /// \param expr The expression to coerce.
  /// \param toType The type to coerce the expression to.
  ///
  /// \returns the coerced expression, which will have type \c ToType.
  Expr *coerceToType(Expr *expr, Type toType) const;

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

  /// \brief Dump this solution to standard error.
  void dump(llvm::SourceMgr *sm) const LLVM_ATTRIBUTE_USED;
};

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

  /// \brief Counter for the overload sets introduced.
  unsigned OverloadSetCounter = 0;

  /// \brief Cached member lookups.
  llvm::DenseMap<std::pair<Type, Identifier>, std::unique_ptr<MemberLookup>>
    MemberLookups;

  /// \brief Cached literal checks.  The key is a canonical type + literal
  /// kind.  The value is a tristate of 0 -> uncomputed, 1 -> computed
  /// false, 2 -> computed true.
  llvm::DenseMap<std::pair<CanType, unsigned>, unsigned> LiteralChecks;

  /// \brief Folding set containing all of the locators used in this
  /// constraint system.
  llvm::FoldingSet<ConstraintLocator> ConstraintLocators;

  /// \brief Folding set containing all of the failures that have occurred
  /// while building and initially simplifying this constraint system.
  ///
  /// These failures are unavoidable, in the sense that they occur before
  /// we have made any (potentially incorrect) assumptions at all.
  llvm::SmallVector<Failure *, 1> unavoidableFailures;

  /// \brief Failures that occured while solving.
  ///
  /// FIXME: We really need to track overload sets and type variable bindings
  /// to make any sense of this data. Also, it probably belongs within
  /// SolverState.
  llvm::FoldingSet<Failure> failures;

  /// \brief A mapping from each overload set that is resolved in this
  /// constraint system to a pair (index, type), where index is the index of
  /// the overload choice (within the overload set) and type is the type
  /// implied by that overload.
  llvm::DenseMap<OverloadSet *, std::pair<unsigned, Type>> ResolvedOverloads;

  SmallVector<TypeVariableType *, 16> TypeVariables;
  SmallVector<Constraint *, 16> Constraints;
  SmallVector<OverloadSet *, 4> UnresolvedOverloadSets;
  llvm::DenseMap<ConstraintLocator *, OverloadSet *> GeneratedOverloadSets;

  typedef llvm::PointerUnion<TypeVariableType *, TypeBase *>
    RepresentativeOrFixed;

  // Valid everywhere, for debugging
  SmallVector<Constraint *, 16> SolvedConstraints;

  /// \brief Describes the current solver state.
  struct SolverState {
    /// \brief Depth of the solution stack.
    unsigned depth = 0;

    /// \brief Whether to record failures or not.
    bool recordFailures = false;

    /// \brief The overload sets that have been resolved along the current path.
    SmallVector<OverloadSet *, 4> resolvedOverloadSets;

    /// \brief The overload sets that were generated along the current path,
    /// indexed by locator.
    SmallVector<ConstraintLocator *, 4> generatedOverloadSets;

    /// \brief The set of constraints that were generated along the current
    /// path.
    SmallVector<Constraint *, 32> generatedConstraints;

    /// \brief The set of constraints that have been retired along the
    /// current path.
    SmallVector<Constraint *, 32> retiredConstraints;

    /// \brief The set of type variable bindings that have changed while
    /// processing this constraint system.
    SavedTypeVariableBindings savedBindings;
  };

  /// \brief The current solver state.
  ///
  /// This will be non-null when we're actively solving the constraint
  /// system, and carries temporary state related to the current path
  /// we're exploring. 
  SolverState *solverState = nullptr;

  unsigned assignTypeVariableID() {
    return TypeCounter++;
  }

  unsigned assignOverloadSetID() {
    return OverloadSetCounter++;
  }
  friend class OverloadSet;

public:
  /// \brief Introduces a new solver scope, which any changes to the
  /// solver state or constraint system are temporary and will be undone when
  /// this object is destroyed.
  ///
  ///
  class SolverScope {
    ConstraintSystem &cs;

    /// \brief The length of \c resolvedOverloadSets.
    unsigned numResolvedOverloadSets;

    /// \brief The length of \c TypeVariables.
    unsigned numTypeVariables;

    /// \brief The length of \c UnresolvedOverloadSets.
    unsigned numUnresolvedOverloadSets;

    /// \brief The length of \c generatedOverloadSets.
    unsigned numGeneratedOverloadSets;
    
    /// \brief The length of \c SavedBindings.
    unsigned numSavedBindings;

    /// \brief The length of \c generatedConstraints.
    unsigned numGeneratedConstraints;

    /// \brief The length of \c retiredConstraints.
    unsigned numRetiredConstraints;

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
  /// \returns an empty solution if this constraint system is unsolvable.
  Optional<Solution> finalize();

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

public:
  /// \brief Lookup for a member with the given name in the given base type.
  ///
  /// This routine caches the results of member lookups in the top constraint
  /// system, to avoid.
  ///
  /// FIXME: This caching should almost certainly be performed at the
  /// translation unit level, since type checking occurs after name binding,
  /// and no new names are introduced after name binding.
  ///
  /// \returns A reference to the member-lookup result.
  MemberLookup &lookupMember(Type base, Identifier name);

  /// \brief Retrieve an unresolved overload set.
  OverloadSet *getUnresolvedOverloadSet(unsigned Idx) const {
    return UnresolvedOverloadSets[Idx];
  }

  /// \brief Create a new type variable.
  template<typename ...Args>
  TypeVariableType *createTypeVariable(Args &&...args) {
    auto tv = TypeVariableType::getNew(TC.Context, assignTypeVariableID(),
                                       std::forward<Args>(args)...);
    TypeVariables.push_back(tv);
    return tv;
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
      unavoidableFailures.push_back(
        Failure::create(getAllocator(), locator, kind,
                        std::forward<Args>(args)...));
      return;
    }

    // Check whether we've recorded this failure already. If so, we're done.
    llvm::FoldingSetNodeID id;
    Failure::Profile(id, locator, kind, args...);
    void *insertPos = nullptr;
    if (failures.FindNodeOrInsertPos(id, insertPos))
      return;

    // Allocate a new failure and record it.
    auto failure = Failure::create(getAllocator(), locator, kind, args...);
    failures.InsertNode(failure, insertPos);
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
  LiteralKind simplifyFailureArg(LiteralKind arg) {
    return arg;
  }

  /// \brief Simplifies an argument to the failure (a no-op).
  Identifier simplifyFailureArg(Identifier arg) {
    return arg;
  }

public:
  /// \brief Whether we should be recording failures.
  bool shouldRecordFailures() {
    return !solverState || solverState->recordFailures;
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
    assert(second && "Missing first type");
    addConstraint(new (*this) Constraint(kind, first, second, Identifier(),
                                         locator));
  }

  ///\ brief Add a literal constraint to the constraint system.
  void addLiteralConstraint(Type type, LiteralKind kind,
                            ConstraintLocator *locator = nullptr) {
    assert(type && "missing type for literal constraint");
    addConstraint(new (*this) Constraint(type, kind, locator));
  }

  /// \brief Add a value member constraint to the constraint system.
  void addValueMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                                ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    assert(memberTy);
    assert(!name.empty());
    addConstraint(new (*this) Constraint(ConstraintKind::ValueMember,
                                         baseTy, memberTy, name, locator));
  }

  /// \brief Add a type member constraint to the constraint system.
  void addTypeMemberConstraint(Type baseTy, Identifier name, Type memberTy,
                               ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    assert(memberTy);
    assert(!name.empty());
    
    // The type of the type member is the metatype of the declared type.
    Type memberMetaTy = MetaTypeType::get(memberTy, getASTContext());
    
    addConstraint(new (*this) Constraint(ConstraintKind::TypeMember,
                                         baseTy, memberMetaTy, name,
                                         locator));
  }

  /// \brief Add an archetype constraint.
  void addArchetypeConstraint(Type baseTy, ConstraintLocator *locator = nullptr) {
    assert(baseTy);
    addConstraint(new (*this) Constraint(ConstraintKind::Archetype,
                                         baseTy, Type(), Identifier(),
                                         locator));
  }

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
                               TypeVariableType *typeVar2) {
    assert(typeVar1 == getRepresentative(typeVar1) &&
           "typeVar1 is not the representative");
    assert(typeVar2 == getRepresentative(typeVar2) &&
           "typeVar2 is not the representative");
    assert(typeVar1 != typeVar2 && "cannot merge type with itself");
    typeVar1->getImpl().mergeEquivalenceClasses(typeVar2, getSavedBindings());
  }

  /// \brief Retrieve the fixed type corresponding to the given type variable,
  /// or a null type if there is no fixed type.
  Type getFixedType(TypeVariableType *typeVar) {
    return typeVar->getImpl().getFixedType(getSavedBindings());
  }

  /// \brief Assign a fixed type to the given type variable.
  void assignFixedType(TypeVariableType *typeVar, Type type) {
    typeVar->getImpl().assignFixedType(type, getSavedBindings());
  }

  /// \brief "Open" the given type by replacing any occurrences of archetypes
  /// (including those implicit in unbound generic types) with fresh type
  /// variables.
  ///
  /// \param type The type to open.
  /// \returns The opened type, or \c type if there are no archetypes in it.
  Type openType(Type type) {
    llvm::DenseMap<ArchetypeType *, TypeVariableType *> replacements;
    return openType(type, { }, replacements);
  }

  /// \brief "Open" the given type by replacing any occurrences of archetypes
  /// (including those implicit in unbound generic types) with fresh type
  /// variables.
  ///
  /// \param type The type to open.
  ///
  /// \param archetypes The set of archetypes we're opening.
  ///
  /// \param replacements The mapping from opened archetypes to the type
  /// variables to which they were opened.
  ///
  /// \returns The opened type, or \c type if there are no archetypes in it.
  Type openType(Type type, ArrayRef<ArchetypeType *> archetypes,
         llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements);

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
  Type openBindingType(Type type);

  /// \brief "Open" the type of a declaration context, which must be a type or
  /// extension.
  ///
  /// \param dc The context to open.
  ///
  /// \param replacements Will receive the set of type variable replacements
  /// for each of the archetypes in \c dc.
  ///
  /// \param genericParams If non-null, will receive the set of generic
  /// parameters opened up by this routine.
  ///
  /// \returns The opened type of the base.
  Type openTypeOfContext(
         DeclContext *dc,
         llvm::DenseMap<ArchetypeType *, TypeVariableType *> &replacements,
         GenericParamList **genericParams);

  /// \brief Retrieve the type of a reference to the given value declaration.
  ///
  /// For references to polymorphic function types, this routine "opens up"
  /// the type by replacing each instance of an archetype with a fresh type
  /// variable.
  Type getTypeOfReference(ValueDecl *decl);

  /// \brief Retrieve the type of a reference to the given value declaration,
  /// as a member with a base of the given type.
  ///
  /// For references to polymorphic function types, this routine "opens up"
  /// the type by replacing each instance of an archetype with a fresh type
  /// variable.
  ///
  /// \param isTypeReference Indicates that we want to refer to the declared
  /// type of the type declaration rather than referring to it as a value.
  Type getTypeOfMemberReference(Type baseTy, ValueDecl *decl,
                                bool isTypeReference);

  /// \brief Add a new overload set to the list of unresolved overload
  /// sets.
  void addOverloadSet(OverloadSet *ovl);

  /// \brief Find the overload set generated by the given locator, if any.
  OverloadSet *getGeneratedOverloadSet(ConstraintLocator *locator);

  /// \brief Find the overload choice that was assumed by this constraint
  /// system (or one of its parents), along with the type it was given.
  Optional<std::pair<OverloadChoice, Type>>
             getSelectedOverloadFromSet(OverloadSet *ovl);

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

  /// \brief The result of attempting to resolve a constraint or set of
  /// constraints.
  enum class SolutionKind : char {
    /// \brief The constraint has been trivially solved, by not introducing
    /// any additional constraints.
    TriviallySolved,
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
  /// \returns an empty optional if the scalar-to-tuple conversion should be
  /// used instead. Otherwise, returns the solution result.
  Optional<SolutionKind> matchTupleTypes(TupleType *tuple1, TupleType *tuple2,
                                         TypeMatchKind kind, unsigned flags,
                                         ConstraintLocatorBuilder locator,
                                         bool &trivial);

  /// \brief Subroutine of \c matchTypes(), which matches up two function
  /// types.
  SolutionKind matchFunctionTypes(FunctionType *func1, FunctionType *func2,
                                  TypeMatchKind kind, unsigned flags,
                                  ConstraintLocatorBuilder locator,
                                  bool &trivial);

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
  /// \param trivial Will be set false if any non-trivial subtyping or
  /// conversion is applied.
  ///
  /// \returns the result of attempting to solve this constraint.
  SolutionKind matchTypes(Type type1, Type type2, TypeMatchKind kind,
                          unsigned flags, ConstraintLocatorBuilder locator,
                          bool &trivial);

public:
  /// \brief Determine whether a given type is a subtype of another.
  ///
  /// This operation is meant to be used only with concrete types.
  bool isSubtypeOf(Type type1, Type type2, bool &isTrivial) {
    isTrivial = true;
    switch (matchTypes(type1, type2, TypeMatchKind::Subtype, TMF_None,
                       nullptr, isTrivial)) {
    case SolutionKind::Error:
      return false;

    case SolutionKind::Solved:
    case SolutionKind::TriviallySolved:
      return true;

    case SolutionKind::Unsolved:
      llvm_unreachable("couldn't solve subtype problem");
    }
  }

  /// \brief Determine whether a given type is convertible to.
  ///
  /// This operation is meant to be used only with concrete types.
  bool isConvertibleTo(Type type1, Type type2, bool &isTrivial) {
    isTrivial = true;
    switch (matchTypes(type1, type2, TypeMatchKind::Conversion, TMF_None,
                       nullptr, isTrivial)) {
      case SolutionKind::Error:
        return false;

      case SolutionKind::Solved:
      case SolutionKind::TriviallySolved:
        return true;

      case SolutionKind::Unsolved:
        llvm_unreachable("couldn't solve subtype problem");
    }
  }

  /// \brief Resolve the given overload set to the choice with the given
  /// index within this constraint system.
  void resolveOverload(OverloadSet *ovl, unsigned idx);

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
  SolutionKind simplifyConformsToConstraint(Type type, ProtocolDecl *protocol,
                                            ConstraintLocator *locator);

  /// \brief Attempt to simplify the given literal constraint.
  SolutionKind simplifyLiteralConstraint(Type type, LiteralKind kind,
                                         ConstraintLocator *locator);

  /// \brief Attempt to simplify the given member constraint.
  SolutionKind simplifyMemberConstraint(const Constraint &constraint);

  /// \brief Attempt to simplify the given archetype constraint.
  SolutionKind simplifyArchetypeConstraint(const Constraint &constraint);

  /// \brief Simplify the given constaint.
  SolutionKind simplifyConstraint(const Constraint &constraint);

public:
  /// \brief Walks through the list of constraints, collecting the constraints
  /// that directly apply to each representative type variable.
  ///
  /// \param typeVarConstraints will be populated with a list of
  /// representative type variables and the constraints that apply directly
  /// to them.
  void collectConstraintsForTypeVariables(
         SmallVectorImpl<TypeVariableConstraints> &typeVarConstraints);

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

  /// \brief Solve the system of constraints.
  ///
  /// \param solutions The set of solutions to this system of constraints.
  ///
  /// \returns true if an error occurred, false otherwise.
  bool solve(SmallVectorImpl<Solution> &solutions);

private:
  /// \brief Determine whether the given \p type matches the default literal
  /// type for a literal constraint placed on the type variable \p tv.
  bool typeMatchesDefaultLiteralConstraint(TypeVariableType *tv,
                                           Type type);

  // \brief Compare two solutions to the same set of constraints.
  static SolutionCompareResult compareSolutions(ConstraintSystem &cs,
                                                const Solution &sol1,
                                                const Solution &sol2);

public:
  /// \brief Given a set of viable solutions, find the best
  /// solution.
  ///
  /// \returns the best solution, or null if there is no best solution.
  Solution *findBestSolution(SmallVectorImpl<Solution> &solutions);

  /// \brief Apply a given solution to the expression, producing a fully
  /// type-checked expression.
  Expr *applySolution(const Solution &solution, Expr *expr);

  /// \brief Apply a given solution to the expression to the top-level
  /// expression, producing a fully type-checked expression.
  Expr *applySolutionShallow(const Solution &solution, Expr *expr);

  void dump();
};

/// \brief Adjust lvalue types within the type of a reference to a declaration.
///
/// For an lvalue type, this routine adds the 'implicit' and 'nonheap' bits to
/// the lvalue.
///
/// For the function type of an assignment operator, makes the first argument
/// an implicit byref(settable).
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
/// \returns true if no tuple conversion is possible, false otherwise.
bool computeTupleShuffle(TupleType *fromTuple, TupleType *toTuple,
                         SmallVectorImpl<int> &sources,
                         SmallVectorImpl<unsigned> &variadicArgs);

} // end namespace constraints

template<typename ...Args>
TypeVariableType *TypeVariableType::getNew(ASTContext &C, Args &&...args) {
  // Allocate memory
  void *mem = C.Allocate(sizeof(TypeVariableType) + sizeof(Implementation),
                         alignof(TypeVariableType),
                         AllocationArena::ConstraintSolver);

  // Construct the type variable.
  auto *result = ::new (mem) TypeVariableType(C);

  // Construct the implementation object.
  new (result+1) TypeVariableType::Implementation(std::forward<Args>(args)...);

  return result;
}

} // end namespace swift

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_SYSTEM_H
