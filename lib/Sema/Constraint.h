//===--- Constraint.h - Constraint in the Type Checker ----------*- C++ -*-===//
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
// This file provides the \c Constraint class and its related types,
// which is used by the constraint-based type checker to describe a
// constraint that must be solved.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_CONSTRAINT_H
#define SWIFT_SEMA_CONSTRAINT_H

#include "OverloadChoice.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"

namespace llvm {

class raw_ostream;

}

namespace swift {

class ProtocolDecl;
class SourceManager;

namespace constraints {

class ConstraintLocator;
class ConstraintSystem;

/// \brief Describes the kind of constraint placed on one or more types.
enum class ConstraintKind : char {
  /// \brief The two types must be bound to the same type. This is the only
  /// truly symmetric constraint.
  Bind,
  /// \brief The two types must be bound to the same type, dropping
  /// lvalueness when comparing a type variable to a type.
  Equal,
  /// \brief The first type is a "trivial" subtype of the second type,
  /// meaning that it is a subtype that is also guaranteed to have the same
  /// in-memory representation.
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
  /// A checked cast from the first type to the second.
  CheckedCast,
  /// \brief The first type can act as the Self type of the second type (which
  /// is a protocol).
  ///
  /// This constraint is slightly looser than a conforms-to constraint, because
  /// an existential can be used as the Self of any protocol within the
  /// existential, even if it doesn't conform to that protocol (e.g., due to
  /// the use of associated types).
  SelfObjectOfProtocol,
  /// \brief Both types are function types. The first function type's
  /// input is the value being passed to the function and its output
  /// is a type variable that describes the output. The second
  /// function type is expected to become a function type. Note, we
  /// do not require the function type attributes to match.
  ApplicableFunction,
  /// \brief Binds the left-hand type to a particular overload choice.
  BindOverload,
  /// \brief The first type has a member with the given name, and the
  /// type of that member, when referenced as a value, is the second type.
  ValueMember,
  /// \brief The first type has a type member with the given name, and the
  /// type of that member, when referenced as a type, is the second type.
  TypeMember,
  /// \brief The first type must be an archetype.
  Archetype,
  /// \brief The first type is a class or an archetype of a class-bound
  /// protocol.
  Class,
  /// \brief The first type must be DynamicLookup or an implicit lvalue thereof.
  DynamicLookupValue,
  /// \brief A conjunction constraint that specifies that all of the stored
  /// constraints must hold.
  Conjunction,
  /// \brief A disjunction constraint that specifies that one or more of the
  /// stored constraints must hold.
  Disjunction
};

/// \brief Classification of the different kinds of constraints.
enum class ConstraintClassification : char {
  /// \brief A relational constraint, which relates two types.
  Relational,

  /// \brief A member constraint, which names a member of a type and assigns
  /// it a reference type.
  Member,

  /// \brief An property of a single type, such as whether it is an archetype.
  TypeProperty,

  /// \brief A conjunction constraint.
  Conjunction,

  /// \brief A disjunction constraint.
  Disjunction
};

/// Specifies a restriction on the kind of conversion that should be
/// performed between the types in a constraint.
///
/// It's common for there to be multiple potential conversions that can
/// apply between two types, e.g., given class types A and B, there might be
/// a superclass conversion from A to B or there might be a user-defined
/// conversion from A to B. The solver may need to explore both paths.
enum class ConversionRestrictionKind {
  /// Tuple-to-tuple conversion.
  TupleToTuple,
  /// Scalar-to-tuple conversion.
  ScalarToTuple,
  /// Tuple-to-scalar conversion.
  TupleToScalar,
  /// Deep equality comparison.
  DeepEquality,
  /// Subclass-to-superclass conversion.
  Superclass,
  /// Lvalue-to-rvalue conversion.
  LValueToRValue,
  /// Value to existential value conversion.
  Existential,
  /// T -> U? value to optional conversion.
  ValueToOptional,
  /// T? -> U? optional to optional conversion.
  OptionalToOptional,
  /// User-defined conversions.
  User
};

/// \brief A constraint between two type variables.
class Constraint : public llvm::ilist_node<Constraint> {
  /// \brief The kind of constraint.
  ConstraintKind Kind : 8;

  /// The kind of restricrion placed on this constraint.
  ConversionRestrictionKind Restriction : 7;

  /// Whether the \c Restriction field is valid.
  unsigned HasRestriction : 1;

  union {
    struct {
      /// \brief The first type.
      Type First;

      /// \brief The second type.
      Type Second;

      /// \brief If non-null, the name of a member of the first type is that
      /// being related to the second type.
      Identifier Member;
    } Types;

    /// The set of constraints for a disjunction.
    ArrayRef<Constraint *> Nested;

    struct {
      /// \brief The first type
      Type First;

      /// \brief The overload choice
      OverloadChoice Choice;
    } Overload;
  };

  /// \brief The locator that describes where in the expression this
  /// constraint applies.
  ConstraintLocator *Locator;

  /// \brief Constraints are always allocated within a given constraint
  /// system.
  void *operator new(size_t) = delete;

  Constraint(ConstraintKind kind, ArrayRef<Constraint *> disjunction,
             ConstraintLocator *locator)
    : Kind(kind), HasRestriction(false), Nested(disjunction), Locator(locator) {
    assert(kind == ConstraintKind::Conjunction ||
           kind == ConstraintKind::Disjunction);
  }

public:
  /// Constraint a new constraint.
  Constraint(ConstraintKind Kind, Type First, Type Second, Identifier Member,
             ConstraintLocator *locator);

  /// Construct a new overload-binding constraint.
  Constraint(Type type, OverloadChoice choice, ConstraintLocator *locator)
    : Kind(ConstraintKind::BindOverload), HasRestriction(false),
      Overload{type, choice}, Locator(locator) { }

  /// Constraint a restricted constraint.
  Constraint(ConstraintKind kind, ConversionRestrictionKind restriction,
             Type first, Type second, ConstraintLocator *locator)
    : Kind(kind), Restriction(restriction), HasRestriction(true),
      Types{ first, second, Identifier() }, Locator(locator) { }

  /// Create a new conjunction constraint.
  static Constraint *createConjunction(ConstraintSystem &cs,
                                       ArrayRef<Constraint *> constraints,
                                       ConstraintLocator *locator);

  /// Create a new disjunction constraint.
  static Constraint *createDisjunction(ConstraintSystem &cs,
                                       ArrayRef<Constraint *> constraints,
                                       ConstraintLocator *locator);

  /// \brief Determine the kind of constraint.
  ConstraintKind getKind() const { return Kind; }

  /// Retrieve the restriction placed on this constraint.
  Optional<ConversionRestrictionKind> getRestriction() const {
    if (!HasRestriction)
      return Nothing;

    return Restriction;
  }

  /// \brief Determine the classification of this constraint, providing
  /// a broader categorization than \c getKind().
  ConstraintClassification getClassification() const {
    switch (Kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::TrivialSubtype:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::Construction:
    case ConstraintKind::ConformsTo:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::SelfObjectOfProtocol:
    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload:
      return ConstraintClassification::Relational;

    case ConstraintKind::ValueMember:
    case ConstraintKind::TypeMember:
      return ConstraintClassification::Member;

    case ConstraintKind::Archetype:
    case ConstraintKind::Class:
    case ConstraintKind::DynamicLookupValue:
      return ConstraintClassification::TypeProperty;

    case ConstraintKind::Conjunction:
      return ConstraintClassification::Disjunction;

    case ConstraintKind::Disjunction:
      return ConstraintClassification::Disjunction;
    }
  }

  /// \brief Retrieve the first type in the constraint.
  Type getFirstType() const {
    assert(getKind() != ConstraintKind::Disjunction &&
           getKind() != ConstraintKind::Conjunction);

    if (getKind() == ConstraintKind::BindOverload)
      return Overload.First;

    return Types.First;
  }

  /// \brief Retrieve the second type in the constraint.
  Type getSecondType() const {
    assert(getKind() != ConstraintKind::Disjunction &&
           getKind() != ConstraintKind::Conjunction);
    return Types.Second;
  }

  /// \brief Retrieve the protocol in a conformance constraint.
  ProtocolDecl *getProtocol() const;

  /// \brief Retrieve the name of the member for a member constraint.
  Identifier getMember() const {
    assert(Kind == ConstraintKind::ValueMember ||
           Kind == ConstraintKind::TypeMember);
    return Types.Member;
  }

  /// \brief Determine whether this constraint kind has a second type.
  static bool hasMember(ConstraintKind kind) {
    return kind == ConstraintKind::ValueMember
        || kind == ConstraintKind::TypeMember;
  }

  /// Retrieve the set of constraints in a conjunction or disjunction.
  ArrayRef<Constraint *> getNestedConstraints() const {
    assert(Kind == ConstraintKind::Conjunction ||
           Kind == ConstraintKind::Disjunction);
    return Nested;
  }

  /// Retrieve the overload choice for an overload-binding constraint.
  OverloadChoice getOverloadChoice() const {
    assert(Kind == ConstraintKind::BindOverload);
    return Overload.Choice;
  }

  /// \brief Retrieve the locator for this constraint.
  ConstraintLocator *getLocator() const { return Locator; }

  void print(llvm::raw_ostream &Out, SourceManager *sm) const;

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *SM) const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");

  void *operator new(size_t bytes, ConstraintSystem& cs,
                     size_t alignment = alignof(Constraint));

  inline void operator delete(void *, const ConstraintSystem &cs, size_t) {}
};

} } // end namespace swift::constraints

namespace llvm {

/// Specialization of \c ilist_traits for constraints.
template<>
struct ilist_traits<swift::constraints::Constraint>
         : public ilist_default_traits<swift::constraints::Constraint> {
  typedef swift::constraints::Constraint Element;

  static Element *createNode(const Element &V) = delete;
  static void deleteNode(Element *V) { /* never deleted */ }

  Element *createSentinel() const { return static_cast<Element *>(&Sentinel); }
  static void destroySentinel(Element *) {}

  Element *provideInitialHead() const { return createSentinel(); }
  Element *ensureHead(Element *) const { return createSentinel(); }
  static void noteHead(Element *, Element *) {}

private:
  mutable ilist_half_node<Element> Sentinel;
};

} // end namespace llvm

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_H
