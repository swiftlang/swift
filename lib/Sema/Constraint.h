//===--- Constraint.h - Constraint in the Type Checker ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
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

#include "CSFix.h"
#include "OverloadChoice.h"
#include "swift/AST/FunctionRefKind.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
#include "swift/Basic/Debug.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/ilist.h"
#include "llvm/ADT/ilist_node.h"
#include "llvm/Support/TrailingObjects.h"

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

/// Describes the kind of constraint placed on one or more types.
enum class ConstraintKind : char {
  /// The two types must be bound to the same type. This is the only
  /// truly symmetric constraint.
  Bind,
  /// The two types must be bound to the same type, dropping
  /// lvalueness when comparing a type variable to a type.
  Equal,
  /// The first type is the type of a function parameter; the second
  /// type is the type of a reference to that parameter from within the
  /// function body. Specifically, the left type is an inout type iff the right
  /// type is an lvalue type with the same object type. Otherwise, the two
  /// types must be the same type.
  BindParam,
  /// Binds the first type to the element type of the second type.
  BindToPointerType,
  /// The first type is a subtype of the second type, i.e., a value
  /// of the type of the first type can be used wherever a value of the
  /// second type is expected.
  Subtype,
  /// The first type is convertible to the second type.
  Conversion,
  /// The first type can be bridged to the second type.
  BridgingConversion,
  /// The first type is the element of an argument tuple that is
  /// convertible to the second type (which represents the corresponding
  /// parameter type).
  ArgumentConversion,
  /// The first type is convertible to the second type, including inout.
  OperatorArgumentConversion,
  /// The first type must conform to the second type (which is a
  /// protocol type).
  ConformsTo,
  /// The first type describes a literal that conforms to the second
  /// type, which is one of the known expressible-by-literal protocols.
  LiteralConformsTo,
  /// A checked cast from the first type to the second.
  CheckedCast,
  /// The first type can act as the Self type of the second type (which
  /// is a protocol).
  ///
  /// This constraint is slightly looser than a conforms-to constraint, because
  /// an existential can be used as the Self of any protocol within the
  /// existential, even if it doesn't conform to that protocol (e.g., due to
  /// the use of associated types).
  SelfObjectOfProtocol,
  /// Both types are function types. The first function type's
  /// input is the value being passed to the function and its output
  /// is a type variable that describes the output. The second
  /// function type is expected to become a function type. Note, we
  /// do not require the function type attributes to match.
  ApplicableFunction,
  /// The first type is a function type whose input is the value passed
  /// to the function and whose output is a type variable describing the output.
  /// The second type is either a `@dynamicCallable` nominal type or the
  /// function type of a `dynamicallyCall` method defined on a
  /// `@dynamicCallable` nominal type.
  DynamicCallableApplicableFunction,
  /// The first type is the type of the dynamicType member of the
  /// second type.
  DynamicTypeOf,
  /// Binds the left-hand type to a particular overload choice.
  BindOverload,
  /// The first type has a member with the given name, and the
  /// type of that member, when referenced as a value, is the second type.
  ValueMember,
  /// The first type (which is implicit) has a member with the given
  /// name, and the type of that member, when referenced as a value, is the
  /// second type.
  UnresolvedValueMember,
  /// The first type can be defaulted to the second (which currently
  /// cannot be dependent).  This is more like a type property than a
  /// relational constraint.
  Defaultable,
  /// A disjunction constraint that specifies that one or more of the
  /// stored constraints must hold.
  Disjunction,
  /// The first type is an optional type whose object type is the second
  /// type, preserving lvalue-ness.
  OptionalObject,
  /// The first type is the same function type as the second type, but
  /// made @escaping.
  EscapableFunctionOf,
  /// The first type is an opened type from the second type (which is
  /// an existential).
  OpenedExistentialOf,
  /// A relation between three types. The first is the key path type,
  /// the second is the root type, and the third is the projected value type.
  /// The second and third types can be lvalues depending on the kind of key
  /// path.
  KeyPathApplication,
  /// A relation between three types. The first is the key path type,
  /// the second is its root type, and the third is the projected value type.
  /// The key path type is chosen based on the selection of overloads for the
  /// member references along the path.
  KeyPath,
  /// The first type is a function type, the second is the function's
  /// input type.
  FunctionInput,
  /// The first type is a function type, the second is the function's
  /// result type.
  FunctionResult,
  /// The first type is a type that's a candidate to be the underlying type of
  /// the second opaque archetype.
  OpaqueUnderlyingType,
  /// The first type will be equal to the second type, but only when the
  /// second type has been fully determined (and mapped down to a concrete
  /// type). At that point, this constraint will be treated like an `Equal`
  /// constraint.
  OneWayEqual,
};

/// Classification of the different kinds of constraints.
enum class ConstraintClassification : char {
  /// A relational constraint, which relates two types.
  Relational,

  /// A member constraint, which names a member of a type and assigns
  /// it a reference type.
  Member,

  /// A property of a single type, such as whether it is defaultable to
  /// a particular type.
  TypeProperty,

  /// A disjunction constraint.
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
  /// Deep equality comparison.
  DeepEquality,
  /// Subclass-to-superclass conversion.
  Superclass,
  /// Class metatype to AnyObject conversion.
  ClassMetatypeToAnyObject,
  /// Existential metatype to AnyObject conversion.
  ExistentialMetatypeToAnyObject,
  /// Protocol value metatype to Protocol class conversion.
  ProtocolMetatypeToProtocolClass,
  /// Inout-to-pointer conversion.
  InoutToPointer,
  /// Array-to-pointer conversion.
  ArrayToPointer,
  /// String-to-pointer conversion.
  StringToPointer,
  /// Pointer-to-pointer conversion.
  PointerToPointer,
  /// Value to existential value conversion, or existential erasure.
  Existential,
  /// Metatype to existential metatype conversion.
  MetatypeToExistentialMetatype,
  /// Existential metatype to metatype conversion.
  ExistentialMetatypeToMetatype,
  /// T -> U? value to optional conversion (or to implicitly unwrapped
  /// optional).
  ValueToOptional,
  /// T? -> U? optional to optional conversion (or unchecked to unchecked).
  OptionalToOptional,
  /// Implicit upcast conversion of array types.
  ArrayUpcast,
  /// Implicit upcast conversion of dictionary types, which includes
  /// bridging.
  DictionaryUpcast,
  /// Implicit upcast conversion of set types, which includes bridging.
  SetUpcast,
  /// T:Hashable -> AnyHashable conversion.
  HashableToAnyHashable,
  /// Implicit conversion from a CF type to its toll-free-bridged Objective-C
  /// class type.
  CFTollFreeBridgeToObjC,
  /// Implicit conversion from an Objective-C class type to its
  /// toll-free-bridged CF type.
  ObjCTollFreeBridgeToCF,
};

/// Specifies whether a given conversion requires the creation of a temporary
/// value which is only valid for a limited scope. For example, the
/// array-to-pointer conversion produces a pointer that is only valid for the
/// duration of the call that it's passed to. Such ephemeral conversions cannot
/// be passed to non-ephemeral parameters.
enum class ConversionEphemeralness {
  /// The conversion requires the creation of a temporary value.
  Ephemeral,
  /// The conversion does not require the creation of a temporary value.
  NonEphemeral,
  /// It is not currently known whether the conversion will produce a temporary
  /// value or not. This can occur for example with an inout-to-pointer
  /// conversion of a member whose base type is an unresolved type variable.
  Unresolved,
};

/// Return a string representation of a conversion restriction.
llvm::StringRef getName(ConversionRestrictionKind kind);

/// Should we record which choice was taken in this disjunction for
/// the purposes of applying it later?
enum RememberChoice_t : bool {
  ForgetChoice = false,
  RememberChoice = true
};

/// A constraint between two type variables.
class Constraint final : public llvm::ilist_node<Constraint>,
    private llvm::TrailingObjects<Constraint, TypeVariableType *> {
  friend TrailingObjects;

  /// The kind of constraint.
  ConstraintKind Kind : 8;

  /// The kind of restriction placed on this constraint.
  ConversionRestrictionKind Restriction : 8;

  /// The fix to be applied to the constraint before visiting it.
  ConstraintFix *TheFix = nullptr;

  /// Whether the \c Restriction field is valid.
  unsigned HasRestriction : 1;

  /// Whether this constraint is currently active, i.e., stored in the worklist.
  unsigned IsActive : 1;

  /// Was this constraint was determined to be inconsistent with the
  /// constraint graph during constraint propagation?
  unsigned IsDisabled : 1;

  /// Whether the choice of this disjunction should be recorded in the
  /// solver state.
  unsigned RememberChoice : 1;
  
  /// Whether or not this constraint is 'favored' in the sense that, if
  /// successfully applied, it should be preferred over any other constraints
  /// in its disjunction.
  unsigned IsFavored : 1;

  /// The number of type variables referenced by this constraint.
  ///
  /// The type variables themselves are tail-allocated.
  unsigned NumTypeVariables : 11;

  /// The kind of function reference, for member references.
  unsigned TheFunctionRefKind : 2;

  union {
    struct {
      /// The first type.
      Type First;

      /// The second type.
      Type Second;

      /// The third type, if any.
      Type Third;
    } Types;

    struct {
      /// The type of the base.
      Type First;

      /// The type of the member.
      Type Second;

      /// If non-null, the name of a member of the first type is that
      /// being related to the second type.
      DeclNameRef Member;

      /// The DC in which the use appears.
      DeclContext *UseDC;
    } Member;

    /// The set of constraints for a disjunction.
    ArrayRef<Constraint *> Nested;

    struct {
      /// The first type
      Type First;

      /// The overload choice
      OverloadChoice Choice;

      /// The DC in which the use appears.
      DeclContext *UseDC;
    } Overload;
  };

  /// The locator that describes where in the expression this
  /// constraint applies.
  ConstraintLocator *Locator;

  /// Constraints are always allocated within a given constraint
  /// system.
  void *operator new(size_t) = delete;

  Constraint(ConstraintKind kind, ArrayRef<Constraint *> constraints,
             ConstraintLocator *locator, ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new constraint.
  Constraint(ConstraintKind kind, Type first, Type second,
             ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new constraint.
  Constraint(ConstraintKind kind, Type first, Type second, Type third,
             ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new member constraint.
  Constraint(ConstraintKind kind, Type first, Type second, DeclNameRef member,
             DeclContext *useDC, FunctionRefKind functionRefKind,
             ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new overload-binding constraint, which might have a fix.
  Constraint(Type type, OverloadChoice choice, DeclContext *useDC,
             ConstraintFix *fix, ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a restricted constraint.
  Constraint(ConstraintKind kind, ConversionRestrictionKind restriction,
             Type first, Type second, ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);
  
  /// Construct a relational constraint with a fix.
  Constraint(ConstraintKind kind, ConstraintFix *fix, Type first, Type second,
             ConstraintLocator *locator, ArrayRef<TypeVariableType *> typeVars);

  /// Retrieve the type variables buffer, for internal mutation.
  MutableArrayRef<TypeVariableType *> getTypeVariablesBuffer() {
    return { getTrailingObjects<TypeVariableType *>(), NumTypeVariables };
  }

public:
  /// Create a new constraint.
  static Constraint *create(ConstraintSystem &cs, ConstraintKind Kind, 
                            Type First, Type Second,
                            ConstraintLocator *locator);

  /// Create a new constraint.
  static Constraint *create(ConstraintSystem &cs, ConstraintKind Kind, 
                            Type First, Type Second, Type Third,
                            ConstraintLocator *locator,
                            ArrayRef<TypeVariableType *> extraTypeVars = { });

  /// Create a new member constraint, or a disjunction of that with the outer
  /// alternatives.
  static Constraint *createMemberOrOuterDisjunction(
      ConstraintSystem &cs, ConstraintKind kind, Type first, Type second,
      DeclNameRef member, DeclContext *useDC, FunctionRefKind functionRefKind,
      ArrayRef<OverloadChoice> outerAlternatives, ConstraintLocator *locator);

  /// Create a new member constraint.
  static Constraint *createMember(ConstraintSystem &cs, ConstraintKind kind,
                                  Type first, Type second, DeclNameRef member,
                                  DeclContext *useDC,
                                  FunctionRefKind functionRefKind,
                                  ConstraintLocator *locator);

  /// Create an overload-binding constraint.
  static Constraint *createBindOverload(ConstraintSystem &cs, Type type, 
                                        OverloadChoice choice, 
                                        DeclContext *useDC,
                                        ConstraintLocator *locator);

  /// Create a restricted relational constraint.
  static Constraint *createRestricted(ConstraintSystem &cs, ConstraintKind kind,
                                      ConversionRestrictionKind restriction,
                                      Type first, Type second, 
                                      ConstraintLocator *locator);

  /// Create a relational constraint with a fix.
  static Constraint *createFixed(ConstraintSystem &cs, ConstraintKind kind,
                                 ConstraintFix *fix, Type first, Type second,
                                 ConstraintLocator *locator);

  /// Create a bind overload choice with a fix.
  /// Note: This constraint is going to be disabled by default.
  static Constraint *createFixedChoice(ConstraintSystem &cs, Type type,
                                       OverloadChoice choice,
                                       DeclContext *useDC, ConstraintFix *fix,
                                       ConstraintLocator *locator);

  /// Create a new disjunction constraint.
  static Constraint *createDisjunction(ConstraintSystem &cs,
                                       ArrayRef<Constraint *> constraints,
                                       ConstraintLocator *locator,
                                       RememberChoice_t shouldRememberChoice
                                         = ForgetChoice);

  /// Determine the kind of constraint.
  ConstraintKind getKind() const { return Kind; }

  /// Retrieve the restriction placed on this constraint.
  Optional<ConversionRestrictionKind> getRestriction() const {
    if (!HasRestriction)
      return None;

    return Restriction;
  }

  /// Retrieve the fix associated with this constraint.
  ConstraintFix *getFix() const { return TheFix; }

  /// Whether this constraint is active, i.e., in the worklist.
  bool isActive() const { return IsActive; }

  /// Set whether this constraint is active or not.
  void setActive(bool active) {
    assert(!isDisabled() && "Cannot activate a constraint that is disabled!");
    IsActive = active;
  }

  /// Whether this constraint is active, i.e., in the worklist.
  bool isDisabled() const { return IsDisabled; }

  /// Set whether this constraint is active or not.
  void setDisabled() {
    assert(!isActive() && "Cannot disable constraint marked as active!");
    IsDisabled = true;
  }

  void setEnabled() {
    assert(isDisabled() && "Can't re-enable already active constraint!");
    IsDisabled = false;
  }

  /// Mark or retrieve whether this constraint should be favored in the system.
  void setFavored(bool favored = true) { IsFavored = favored; }
  bool isFavored() const { return IsFavored; }

  /// Whether the solver should remember which choice was taken for
  /// this constraint.
  bool shouldRememberChoice() const { return RememberChoice; }

  /// Retrieve the set of type variables referenced by this constraint.
  ArrayRef<TypeVariableType *> getTypeVariables() const {
    return {getTrailingObjects<TypeVariableType*>(), NumTypeVariables};
  }

  /// Determine the classification of this constraint, providing
  /// a broader categorization than \c getKind().
  ConstraintClassification getClassification() const {
    switch (Kind) {
    case ConstraintKind::Bind:
    case ConstraintKind::Equal:
    case ConstraintKind::BindParam:
    case ConstraintKind::BindToPointerType:
    case ConstraintKind::Subtype:
    case ConstraintKind::Conversion:
    case ConstraintKind::BridgingConversion:
    case ConstraintKind::ArgumentConversion:
    case ConstraintKind::OperatorArgumentConversion:
    case ConstraintKind::ConformsTo:
    case ConstraintKind::LiteralConformsTo:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::SelfObjectOfProtocol:
    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::DynamicCallableApplicableFunction:
    case ConstraintKind::BindOverload:
    case ConstraintKind::OptionalObject:
    case ConstraintKind::OpaqueUnderlyingType:
    case ConstraintKind::OneWayEqual:
      return ConstraintClassification::Relational;

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
      return ConstraintClassification::Member;

    case ConstraintKind::DynamicTypeOf:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::OpenedExistentialOf:
    case ConstraintKind::KeyPath:
    case ConstraintKind::KeyPathApplication:
    case ConstraintKind::Defaultable:
    case ConstraintKind::FunctionInput:
    case ConstraintKind::FunctionResult:
      return ConstraintClassification::TypeProperty;

    case ConstraintKind::Disjunction:
      return ConstraintClassification::Disjunction;
    }

    llvm_unreachable("Unhandled ConstraintKind in switch.");
  }

  /// Retrieve the first type in the constraint.
  Type getFirstType() const {
    switch (getKind()) {
    case ConstraintKind::Disjunction:
      llvm_unreachable("disjunction constraints have no type operands");

    case ConstraintKind::BindOverload:
      return Overload.First;

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
      return Member.First;

    default:
      return Types.First;
    }
  }

  /// Retrieve the second type in the constraint.
  Type getSecondType() const {
    switch (getKind()) {
    case ConstraintKind::Disjunction:
    case ConstraintKind::BindOverload:
      llvm_unreachable("constraint has no second type");

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
      return Member.Second;

    default:
      return Types.Second;
    }
  }

  /// Retrieve the third type in the constraint.
  Type getThirdType() const {
    switch (getKind()) {
    case ConstraintKind::KeyPath:
    case ConstraintKind::KeyPathApplication:
      return Types.Third;
    default:
      llvm_unreachable("no third type");
    }
  }

  /// Retrieve the protocol in a conformance constraint.
  ProtocolDecl *getProtocol() const;

  /// Retrieve the name of the member for a member constraint.
  DeclNameRef getMember() const {
    assert(Kind == ConstraintKind::ValueMember ||
           Kind == ConstraintKind::UnresolvedValueMember);
    return Member.Member;
  }

  /// Determine whether this constraint kind has a second type.
  static bool hasMember(ConstraintKind kind) {
    return kind == ConstraintKind::ValueMember
        || kind == ConstraintKind::UnresolvedValueMember;
  }

  /// Determine the kind of function reference we have for a member reference.
  FunctionRefKind getFunctionRefKind() const {
    if (Kind == ConstraintKind::ValueMember ||
        Kind == ConstraintKind::UnresolvedValueMember)
      return static_cast<FunctionRefKind>(TheFunctionRefKind);

    // Conservative answer: drop all of the labels.
    return FunctionRefKind::Compound;
  }

  /// Retrieve the set of constraints in a disjunction.
  ArrayRef<Constraint *> getNestedConstraints() const {
    assert(Kind == ConstraintKind::Disjunction);
    return Nested;
  }

  unsigned countActiveNestedConstraints() const {
    unsigned count = 0;
    for (auto *constraint : Nested)
      if (!constraint->isDisabled())
        count++;

    return count;
  }

  /// Determine if this constraint represents explicit conversion,
  /// e.g. coercion constraint "as X" which forms a disjunction.
  bool isExplicitConversion() const;

  /// Whether this is a one-way constraint.
  bool isOneWayConstraint() const {
    return Kind == ConstraintKind::OneWayEqual;
  }

  /// Retrieve the overload choice for an overload-binding constraint.
  OverloadChoice getOverloadChoice() const {
    assert(Kind == ConstraintKind::BindOverload);
    return Overload.Choice;
  }

  /// Retrieve the DC in which the overload was used.
  DeclContext *getOverloadUseDC() const {
    assert(Kind == ConstraintKind::BindOverload);
    return Overload.UseDC;
  }

  /// Retrieve the DC in which the member was used.
  DeclContext *getMemberUseDC() const {
    assert(Kind == ConstraintKind::ValueMember ||
           Kind == ConstraintKind::UnresolvedValueMember);
    return Member.UseDC;
  }

  /// Retrieve the locator for this constraint.
  ConstraintLocator *getLocator() const { return Locator; }

  /// Clone the given constraint.
  Constraint *clone(ConstraintSystem &cs) const;

  void print(llvm::raw_ostream &Out, SourceManager *sm) const;

  SWIFT_DEBUG_DUMPER(dump(SourceManager *SM));

  SWIFT_DEBUG_DUMPER(dump(ConstraintSystem *CS));

  void *operator new(size_t bytes, ConstraintSystem& cs,
                     size_t alignment = alignof(Constraint));

  inline void operator delete(void *, const ConstraintSystem &cs, size_t) {}

  void *operator new(size_t bytes, void *mem) { return mem; }
  void operator delete(void *mem) { }
};

} // end namespace constraints
} // end namespace swift

namespace llvm {

/// Specialization of \c ilist_traits for constraints.
template<>
struct ilist_traits<swift::constraints::Constraint>
         : public ilist_node_traits<swift::constraints::Constraint> {
  using Element = swift::constraints::Constraint;

  static Element *createNode(const Element &V) = delete;
  static void deleteNode(Element *V) { /* never deleted */ }
};

} // end namespace llvm

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_H
