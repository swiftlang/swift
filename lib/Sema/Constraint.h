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

#include "OverloadChoice.h"
#include "swift/AST/FunctionRefKind.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Type.h"
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

/// \brief Describes the kind of constraint placed on one or more types.
enum class ConstraintKind : char {
  /// \brief The two types must be bound to the same type. This is the only
  /// truly symmetric constraint.
  Bind,
  /// \brief The two types must be bound to the same type, dropping
  /// lvalueness when comparing a type variable to a type.
  Equal,
  /// \brief The first type is the type of a function parameter; the second
  /// type is the type of a reference to that parameter from within the
  /// function body. Specifically, the left type is an inout type iff the right
  /// type is an lvalue type with the same object type. Otherwise, the two
  /// types must be the same type.
  BindParam,
  /// \brief Binds the first type to the element type of the second type.
  BindToPointerType,
  /// \brief The first type is a subtype of the second type, i.e., a value
  /// of the type of the first type can be used wherever a value of the
  /// second type is expected.
  Subtype,
  /// \brief The first type is convertible to the second type.
  Conversion,
  /// \brief The first type can be bridged to the second type.
  BridgingConversion,
  /// \brief The first type is the element of an argument tuple that is
  /// convertible to the second type (which represents the corresponding
  /// parameter type).
  ArgumentConversion,
  /// \brief The first type is an argument type (or tuple) that is convertible
  /// to the second type (which represents the parameter type/tuple).
  ArgumentTupleConversion,
  /// An argument tuple conversion for operators.
  OperatorArgumentTupleConversion,
  /// \brief The first type is convertible to the second type, including inout.
  OperatorArgumentConversion,
  /// \brief The first type must conform to the second type (which is a
  /// protocol type).
  ConformsTo,
  /// \brief The first type must conform to the layout defined by the second
  /// component representing a layout constraint.
  Layout,
  /// \brief The first type describes a literal that conforms to the second
  /// type, which is one of the known expressible-by-literal protocols.
  LiteralConformsTo,
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
  /// \brief The first type (which is implicit) has a member with the given
  /// name, and the type of that member, when referenced as a value, is the
  /// second type.
  UnresolvedValueMember,
  /// \brief The first type can be defaulted to the second (which currently
  /// cannot be dependent).  This is more like a type property than a
  /// relational constraint.
  Defaultable,
  /// \brief A disjunction constraint that specifies that one or more of the
  /// stored constraints must hold.
  Disjunction,
  /// \brief The first type is an optional type whose object type is the second
  /// type, preserving lvalue-ness.
  OptionalObject,
  /// \brief The first type is the same function type as the second type, but
  /// made @escaping.
  EscapableFunctionOf,
};

/// \brief Classification of the different kinds of constraints.
enum class ConstraintClassification : char {
  /// \brief A relational constraint, which relates two types.
  Relational,

  /// \brief A member constraint, which names a member of a type and assigns
  /// it a reference type.
  Member,

  /// \brief A property of a single type, such as whether it is defaultable to
  /// a particular type.
  TypeProperty,

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
  /// Lvalue-to-rvalue conversion.
  LValueToRValue,
  /// Value to existential value conversion, or existential erasure.
  Existential,
  /// Metatype to existential metatype conversion.
  MetatypeToExistentialMetatype,
  /// T -> U? value to optional conversion (or to implicitly unwrapped optional).
  ValueToOptional,
  /// T? -> U? optional to optional conversion (or unchecked to unchecked).
  OptionalToOptional,
  /// Implicit forces of implicitly unwrapped optionals to their presumed values
  ForceUnchecked,
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
  ObjCTollFreeBridgeToCF
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

  /// Introduce a '!' to force an optional unwrap.
  ForceOptional,
    
  /// Introduce a '?.' to begin optional chaining.
  OptionalChaining,

  /// Append 'as! T' to force a downcast to the specified type.
  ForceDowncast,

  /// Introduce a '&' to take the address of an lvalue.
  AddressOf,
  
  /// Replace a coercion ('as') with a forced checked cast ('as!').
  CoerceToCheckedCast,
};

/// Describes a fix that can be applied to a constraint before visiting it.
class Fix {
  FixKind Kind;
  uint16_t Data;

  Fix(FixKind kind, uint16_t data) : Kind(kind), Data(data){ }

  uint16_t getData() const { return Data; }

  friend class Constraint;

public:
  Fix() : Kind(FixKind::None), Data(0) { }
  
  Fix(FixKind kind) : Kind(kind), Data(0) { 
    assert(kind != FixKind::ForceDowncast && "Use getForceDowncast()");
  }

  /// Produce a new fix that performs a forced downcast to the given type.
  static Fix getForcedDowncast(ConstraintSystem &cs, Type toType);

  /// Retrieve the kind of fix.
  FixKind getKind() const { return Kind; }

  /// If this fix has a type argument, retrieve it.
  Type getTypeArgument(ConstraintSystem &cs) const;

  /// Return a string representation of a fix.
  static llvm::StringRef getName(FixKind kind);

  void print(llvm::raw_ostream &Out, ConstraintSystem *cs) const;

  LLVM_ATTRIBUTE_DEPRECATED(void dump(ConstraintSystem *cs) const 
                              LLVM_ATTRIBUTE_USED,
                            "only for use within the debugger");
};


/// \brief A constraint between two type variables.
class Constraint final : public llvm::ilist_node<Constraint>,
    private llvm::TrailingObjects<Constraint, TypeVariableType *> {
  friend TrailingObjects;

  /// \brief The kind of constraint.
  ConstraintKind Kind : 8;

  /// The kind of restriction placed on this constraint.
  ConversionRestrictionKind Restriction : 8;

  /// Data associated with the fix.
  uint16_t FixData;

  /// The kind of fix to be applied to the constraint before visiting it.
  FixKind TheFix;

  /// Whether the \c Restriction field is valid.
  unsigned HasRestriction : 1;

  /// Whether the \c Fix field is valid.
  unsigned HasFix : 1;

  /// Whether this constraint is currently active, i.e., stored in the worklist.
  unsigned IsActive : 1;

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
      /// \brief The first type.
      Type First;

      /// \brief The second type.
      Type Second;
    } Types;

    struct {
      /// \brief The type of the base.
      Type First;

      /// \brief The type of the member.
      Type Second;

      /// \brief If non-null, the name of a member of the first type is that
      /// being related to the second type.
      DeclName Member;

      /// \brief The DC in which the use appears.
      DeclContext *UseDC;
    } Member;

    /// The set of constraints for a disjunction.
    ArrayRef<Constraint *> Nested;

    struct {
      /// \brief The first type
      Type First;

      /// \brief The overload choice
      OverloadChoice Choice;

      /// \brief The DC in which the use appears.
      DeclContext *UseDC;
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
  Constraint(ConstraintKind kind, Type first, Type second,
             ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new member constraint.
  Constraint(ConstraintKind kind, Type first, Type second, DeclName member,
             DeclContext *useDC, FunctionRefKind functionRefKind,
             ConstraintLocator *locator,
             ArrayRef<TypeVariableType *> typeVars);

  /// Construct a new overload-binding constraint.
  Constraint(Type type, OverloadChoice choice, DeclContext *useDC,
             ConstraintLocator *locator, ArrayRef<TypeVariableType *> typeVars);

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
    return { getTrailingObjects<TypeVariableType *>(), NumTypeVariables };
  }

public:
  /// Create a new constraint.
  static Constraint *create(ConstraintSystem &cs, ConstraintKind Kind, 
                            Type First, Type Second,
                            ConstraintLocator *locator);

  /// Create a new member constraint.
  static Constraint *createMember(ConstraintSystem &cs, ConstraintKind kind,
                                  Type first, Type second, DeclName member,
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
                                 Fix fix,
                                 Type first, Type second,
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
      return None;

    return Restriction;
  }

  /// Retrieve the fix associated with this constraint.
  Optional<Fix> getFix() const {
    if (!HasFix)
      return None;

    return Fix(TheFix, FixData);
  }

  /// Whether this constraint is active, i.e., in the worklist.
  bool isActive() const { return IsActive; }

  /// Set whether this constraint is active or not.
  void setActive(bool active) { IsActive = active; }
  
  /// Mark or retrieve whether this constraint should be favored in the system.
  void setFavored() { IsFavored = true; }
  bool isFavored() const { return IsFavored; }

  /// Whether the solver should remember which choice was taken for
  /// this constraint.
  bool shouldRememberChoice() const { return RememberChoice; }

  /// Retrieve the set of type variables referenced by this constraint.
  ArrayRef<TypeVariableType *> getTypeVariables() const {
    return {getTrailingObjects<TypeVariableType*>(), NumTypeVariables};
  }

  /// \brief Determine the classification of this constraint, providing
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
    case ConstraintKind::ArgumentTupleConversion:
    case ConstraintKind::OperatorArgumentTupleConversion:
    case ConstraintKind::OperatorArgumentConversion:
    case ConstraintKind::ConformsTo:
    case ConstraintKind::Layout:
    case ConstraintKind::LiteralConformsTo:
    case ConstraintKind::CheckedCast:
    case ConstraintKind::SelfObjectOfProtocol:
    case ConstraintKind::ApplicableFunction:
    case ConstraintKind::BindOverload:
    case ConstraintKind::OptionalObject:
      return ConstraintClassification::Relational;

    case ConstraintKind::ValueMember:
    case ConstraintKind::UnresolvedValueMember:
      return ConstraintClassification::Member;

    case ConstraintKind::DynamicTypeOf:
    case ConstraintKind::EscapableFunctionOf:
    case ConstraintKind::Defaultable:
      return ConstraintClassification::TypeProperty;

    case ConstraintKind::Disjunction:
      return ConstraintClassification::Disjunction;
    }

    llvm_unreachable("Unhandled ConstraintKind in switch.");
  }

  /// \brief Retrieve the first type in the constraint.
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

  /// \brief Retrieve the second type in the constraint.
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

  /// \brief Retrieve the protocol in a conformance constraint.
  ProtocolDecl *getProtocol() const;

  /// \brief Retrieve the name of the member for a member constraint.
  DeclName getMember() const {
    assert(Kind == ConstraintKind::ValueMember ||
           Kind == ConstraintKind::UnresolvedValueMember);
    return Member.Member;
  }

  /// \brief Determine whether this constraint kind has a second type.
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

  /// \brief Retrieve the locator for this constraint.
  ConstraintLocator *getLocator() const { return Locator; }

  /// Clone the given constraint.
  Constraint *clone(ConstraintSystem &cs) const;

  void print(llvm::raw_ostream &Out, SourceManager *sm) const;

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(SourceManager *SM) const LLVM_ATTRIBUTE_USED,
      "only for use within the debugger");

  LLVM_ATTRIBUTE_DEPRECATED(
      void dump(ConstraintSystem *CS) const LLVM_ATTRIBUTE_USED,
    "only for use within the debugger");

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
         : public ilist_default_traits<swift::constraints::Constraint> {
  typedef swift::constraints::Constraint Element;

  static Element *createNode(const Element &V) = delete;
  static void deleteNode(Element *V) { /* never deleted */ }
};

} // end namespace llvm

#endif // LLVM_SWIFT_SEMA_CONSTRAINT_H
