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
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"

namespace llvm {

class raw_ostream;

}

namespace swift {

class ProtocolDecl;
class SourceManager;
class TypeVariableType;

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
  /// \brief The first type is a subtype of the second type, i.e., a value
  /// of the type of the first type can be used wherever a value of the
  /// second type is expected.
  Subtype,
  /// \brief The first type is convertible to the second type.
  Conversion,
  /// \brief The first type is an argument type (or tuple) that is convertible
  /// to the second type (which represents the parameter type/tuple).
  ArgumentTupleConversion,
  /// \brief The first type is convertible to the second type, including inout.
  OperatorConversion,
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
  /// \brief The first type is the type of the dynamicType member of the
  /// second type.
  DynamicTypeOf,
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
  /// \brief The first type must be AnyObject or an implicit lvalue thereof.
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
  /// T -> U? value to optional conversion (or to implicitly unwrapped optional).
  ValueToOptional,
  /// T? -> U? optional to optional conversion (or unchecked to unchecked).
  OptionalToOptional,
  /// T! -> U? unchecked-optional to optional conversion
  ImplicitlyUnwrappedOptionalToOptional,
  /// T? -> U! optional to implicitly unwrapped optional conversion
  OptionalToImplicitlyUnwrappedOptional,
  /// Implicit forces of implicitly unwrapped optionals to their presumed values
  ForceUnchecked,
  /// Implicit upcast conversion of array types
  ArrayUpcast,
  /// Implicit bridged conversion between array types
  ArrayBridged,
  /// User-defined conversions.
  User
};

/// Return a string representation of a conversion restriction.
llvm::StringRef getName(ConversionRestrictionKind kind);

/// Should we record which choice was taken in this disjunction for
/// the purposes of applying it later?
enum RememberChoice_t : bool {
  ForgetChoice = false,
  RememberChoice = true
};

/// Describes the kind of fix to apply to the given constraint before
/// visiting it.
enum class FixKind : uint8_t {
  /// No fix, which is used as a placeholder indicating that future processing
  /// of this constraint should not attempt fixes.
  None,

  /// Introduce a call with no arguments, i.e., (), on the first type before
  /// applying the constraint to the second type.
  NullaryCall,

  /// Introduce a '!' to force an optional unwrap.
  ForceOptional,

  /// Introduce a '!' to force a downcast.
  ForceDowncast,

  /// Introduce a '&' to take the address of an lvalue.
  AddressOf,

  /// Remove a no-argument call to something that is not a function.
  RemoveNullaryCall,

  /// Relabel a tuple due to a tuple-to-scalar conversion.
  TupleToScalar,

  /// Relabel a tuple due to a scalar-to-tuple conversion.
  ScalarToTuple,

  /// Relabel a tuple due to a call
  RelabelCallTuple,
};

/// Desribes a fix that can be applied to a constraint before visiting it.
class Fix {
  FixKind Kind;
  uint16_t Data;

  Fix(FixKind kind, uint16_t data) : Kind(kind), Data(data){ }

  uint16_t getData() const { return Data; }

  friend class Constraint;

public:
  Fix() : Kind(FixKind::None), Data(0) { }
  
  Fix(FixKind kind) : Kind(kind), Data(0) { 
    assert(!isRelabelTuple() && "Use getRelabelTuple()");
  }

  /// Produce a new fix that relabels a tuple.
  static Fix getRelabelTuple(ConstraintSystem &cs, FixKind kind,
                             ArrayRef<Identifier> names);

  /// Retrieve the kind of fix.
  FixKind getKind() const { return Kind; }

  /// Whether the fix is a tuple-relabelling fix.
  bool isRelabelTuple() const { return isRelabelTupleKind(getKind()); }

  /// Whether the fix kind is a tuple-relabelling fix.
  static bool isRelabelTupleKind(FixKind kind) { 
    return kind == FixKind::TupleToScalar ||
           kind == FixKind::ScalarToTuple ||
           kind == FixKind::RelabelCallTuple;
  }

  /// For a relabel-tuple fix, retrieve the new names.
  ArrayRef<Identifier> getRelabelTupleNames(ConstraintSystem &cs) const;

  /// Return a string representation of a fix.
  static llvm::StringRef getName(FixKind kind);

  void print(llvm::raw_ostream &Out, ConstraintSystem *cs) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump(ConstraintSystem *cs) const 
                              LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");
};


/// \brief A constraint between two type variables.
class Constraint : public llvm::ilist_node<Constraint> {
  /// \brief The kind of constraint.
  ConstraintKind Kind : 8;

  /// The kind of restriction placed on this constraint.
  ConversionRestrictionKind Restriction : 8;

  /// The kind of fix to be applied to the constraint before visiting it.
  FixKind TheFix;

  /// Data associated with the fix.
  uint16_t FixData;

  /// Whether the \c Restriction field is valid.
  unsigned HasRestriction : 1;

  /// Whether the \c Fix field is valid.
  unsigned HasFix : 1;

  /// Whether this constraint is currently active, i.e., stored in the worklist.
  unsigned IsActive : 1;

  /// Whether the choice of this disjunction should be recorded in the
  /// solver state.
  unsigned RememberChoice : 1;

  /// The number of type variables referenced by this constraint.
  ///
  /// The type variables themselves are tail-allocated.
  unsigned NumTypeVariables : 12;

  union {
    struct {
      /// \brief The first type.
      Type First;

      /// \brief The second type.
      Type Second;

      /// \brief If non-null, the name of a member of the first type is that
      /// being related to the second type.
      DeclName Member;
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

  Constraint(ConstraintKind kind, ArrayRef<Constraint *> constraints,
             ConstraintLocator *locator, ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new constraint.
  Constraint(ConstraintKind kind, Type first, Type second, DeclName member,
             ConstraintLocator *locator, ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new overload-binding constraint.
  Constraint(Type type, OverloadChoice choice, ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a restricted constraint.
  Constraint(ConstraintKind kind, ConversionRestrictionKind restriction,
             Type first, Type second, ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);
  
  /// Construct a relational constraint with a fix.
  Constraint(ConstraintKind kind, Fix fix,
             Type first, Type second, ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Retrieve the type variables buffer, for internal mutation.
  MutableArrayRef<TypeVariableType *> getTypeVariablesBuffer() {
    return { reinterpret_cast<TypeVariableType **>(this + 1), NumTypeVariables };
  }

public:
  /// Create a new constraint.
  static Constraint *create(ConstraintSystem &cs, ConstraintKind Kind, 
                            Type First, Type Second, DeclName Member,
                            ConstraintLocator *locator);

  /// Create an overload-binding constraint.
  static Constraint *createBindOverload(ConstraintSystem &cs, Type type, 
                                        OverloadChoice choice, 
                                        ConstraintLocator *locator);

  /// Create a restricted relational constraint.
  static Constraint *createRestricted(ConstraintSystem &cs, ConstraintKind kind,
                                      ConversionRestrictionKind restriction,
                                      Type first, Type second, 
                                      ConstraintLocator *locator);

  /// Create a relational constraint with a fix.
  static Constraint *createFixed(ConstraintSystem &cs, ConstraintKind kind,
                                 Fix fix,
                                 Type first, Type second,
                                 ConstraintLocator *locator);

  /// Create a new conjunction constraint.
  static Constraint *createConjunction(ConstraintSystem &cs,
                                       ArrayRef<Constraint *> constraints,
                                       ConstraintLocator *locator);

  /// Create a new disjunction constraint.
  static Constraint *createDisjunction(ConstraintSystem &cs,
                                       ArrayRef<Constraint *> constraints,
                                       ConstraintLocator *locator,
                                       RememberChoice_t shouldRememberChoice
                                         = ForgetChoice);

  /// \brief Determine the kind of constraint.
  ConstraintKind getKind() const { return Kind; }

  /// Retrieve the restriction placed on this constraint.
  Optional<ConversionRestrictionKind> getRestriction() const {
    if (!HasRestriction)
      return Nothing;

    return Restriction;
  }

  /// Retrieve the fix associated with this constraint.
  Optional<Fix> getFix() const {
    if (!HasFix)
      return Nothing;

    return Fix(TheFix, FixData);
  }

  /// Whether this constraint is active, i.e., in the worklist.
  bool isActive() const { return IsActive; }

  /// Set whether this constraint is active or not.
  void setActive(bool active) { IsActive = active; }

  /// Whether the solver should remember which choice was taken for
  /// this constraint.
  bool shouldRememberChoice() const { return RememberChoice; }

  /// Retrieve the set of type variables referenced by this constraint.
  ArrayRef<TypeVariableType *> getTypeVariables() const {
    return { reinterpret_cast<TypeVariableType * const *>(this + 1), 
             NumTypeVariables };
  }

  /// \brief Determine the classification of this constraint, providing
  /// a broader categorization than \c getKind().
  ConstraintClassification getClassification() const {
    switch (Kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::ArgumentTupleConversion:
    case ConstraintKind::OperatorConversion:
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
    case ConstraintKind::DynamicTypeOf:
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
  DeclName getMember() const {
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

  void *operator new(size_t bytes, void *mem) { return mem; }
  void operator delete(void *mem) { }
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
