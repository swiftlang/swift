//===--- Types.h - Swift Language Type ASTs ---------------------*- C++ -*-===//
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
// This file defines the TypeBase class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPES_H
#define SWIFT_TYPES_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/GenericParamKey.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/SubstitutionList.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/TrailingObjects.h"

namespace llvm {
  struct fltSemantics;
}
namespace swift {
  enum class AllocationArena;
  class ArchetypeType;
  class AssociatedTypeDecl;
  class ASTContext;
  class ClassDecl;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class GenericParamList;
  class GenericSignature;
  class Identifier;
  enum class ResilienceExpansion : unsigned;
  class SILModule;
  class SILType;
  class TypeAliasDecl;
  class TypeDecl;
  class NominalTypeDecl;
  class GenericTypeDecl;
  class EnumDecl;
  class EnumElementDecl;
  class StructDecl;
  class ProtocolDecl;
  class TypeVariableType;
  class ValueDecl;
  class ModuleDecl;
  class ModuleType;
  class ProtocolConformance;
  enum OptionalTypeKind : unsigned;
  enum PointerTypeKind : unsigned;
  struct ValueOwnershipKind;

  enum class TypeKind {
#define TYPE(id, parent) id,
#define TYPE_RANGE(Id, FirstId, LastId) \
  First_##Id##Type = FirstId, Last_##Id##Type = LastId,
#include "swift/AST/TypeNodes.def"
  };

/// Various properties of types that are primarily defined recursively
/// on structural types.
class RecursiveTypeProperties {
public:
  enum { BitWidth = 10 };

  /// A single property.
  ///
  /// Note that the property polarities should be chosen so that 0 is
  /// the correct default value and bitwise-or correctly merges things.
  enum Property : unsigned {
    /// This type expression contains a TypeVariableType.
    HasTypeVariable      = 0x01,

    /// This type expression contains an ArchetypeType.
    HasArchetype         = 0x02,

    /// This type expression contains a GenericTypeParamType.
    HasTypeParameter     = 0x04,

    /// This type expression contains an UnresolvedType.
    HasUnresolvedType    = 0x08,
   
    /// This type expression contains an InOutType other than as a
    /// function input.
    HasInOut             = 0x10,

    /// Whether this type expression contains an unbound generic type.
    HasUnboundGeneric    = 0x20,

    /// This type expression contains an LValueType other than as a
    /// function input, and can be loaded to convert to an rvalue.
    IsLValue             = 0x40,

    /// This type expression contains an opened existential ArchetypeType.
    HasOpenedExistential = 0x80,

    /// This type expression contains a DynamicSelf type.
    HasDynamicSelf       = 0x100,

    /// This type contains an Error type.
    HasError             = 0x200,

    IsNotMaterializable  = (HasInOut | IsLValue)
  };

private:
  unsigned Bits : BitWidth;

public:
  RecursiveTypeProperties() : Bits(0) {}
  RecursiveTypeProperties(Property prop) : Bits(prop) {}
  explicit RecursiveTypeProperties(unsigned bits) : Bits(bits) {}

  /// Return these properties as a bitfield.
  unsigned getBits() const { return Bits; }

  /// Does a type with these properties structurally contain a type
  /// variable?
  bool hasTypeVariable() const { return Bits & HasTypeVariable; }

  /// Does a type with these properties structurally contain an
  /// archetype?
  bool hasArchetype() const { return Bits & HasArchetype; }

  /// Does a type with these properties have a type parameter somewhere in it?
  bool hasTypeParameter() const { return Bits & HasTypeParameter; }

  /// Does a type with these properties have an unresolved type somewhere in it?
  bool hasUnresolvedType() const { return Bits & HasUnresolvedType; }

  /// Does a type with these properties structurally contain an
  /// inout, except as the parameter of a function?
  bool hasInOut() const { return Bits & HasInOut; }
  
  /// Is a type with these properties an lvalue?
  bool isLValue() const { return Bits & IsLValue; }

  /// Does this type contain an error?
  bool hasError() const { return Bits & HasError; }

  /// Is a type with these properties materializable: that is, is it a
  /// first-class value type?
  bool isMaterializable() const { return !(Bits & IsNotMaterializable); }

  /// Does a type with these properties structurally contain an
  /// archetype?
  bool hasOpenedExistential() const { return Bits & HasOpenedExistential; }

  /// Does a type with these properties structurally contain a
  /// reference to DynamicSelf?
  bool hasDynamicSelf() const { return Bits & HasDynamicSelf; }

  /// Does a type with these properties structurally contain an unbound
  /// generic type?
  bool hasUnboundGeneric() const { return Bits & HasUnboundGeneric; }

  /// Returns the set of properties present in either set.
  friend RecursiveTypeProperties operator|(Property lhs, Property rhs) {
    return RecursiveTypeProperties(unsigned(lhs) | unsigned(rhs));
  }
  friend RecursiveTypeProperties operator|(RecursiveTypeProperties lhs,
                                           RecursiveTypeProperties rhs) {
    return RecursiveTypeProperties(lhs.Bits | rhs.Bits);
  }

  /// Add any properties in the right-hand set to this set.
  RecursiveTypeProperties &operator|=(RecursiveTypeProperties other) {
    Bits |= other.Bits;
    return *this;
  }
  /// Restrict this to only the properties in the right-hand set.
  RecursiveTypeProperties &operator&=(RecursiveTypeProperties other) {
    Bits &= other.Bits;
    return *this;
  }

  /// Remove the HasTypeParameter property from this set.
  void removeHasTypeParameter() {
    Bits &= ~HasTypeParameter;
  }

  /// Test for a particular property in this set.
  bool operator&(Property prop) const {
    return Bits & prop;
  }
};

inline RecursiveTypeProperties operator~(RecursiveTypeProperties::Property P) {
  return RecursiveTypeProperties(~unsigned(P));
}
  
/// The result of a type trait check.
enum class TypeTraitResult {
  /// The type cannot have the trait.
  IsNot,
  /// The generic type can be bound to a type that has the trait.
  CanBe,
  /// The type has the trait irrespective of generic substitutions.
  Is,
};

/// Specifies which normally-unsafe type mismatches should be accepted when
/// checking overrides.
enum class OverrideMatchMode {
  /// Only accept overrides that are properly covariant.
  Strict,
  /// Allow a parameter with IUO type to be overridden by a parameter with non-
  /// optional type.
  AllowNonOptionalForIUOParam,
  /// Allow any mismatches of Optional or ImplicitlyUnwrappedOptional at the
  /// top level of a type.
  ///
  /// This includes function parameters and result types as well as tuple
  /// elements, but excludes generic parameters.
  AllowTopLevelOptionalMismatch
};

/// TypeBase - Base class for all types in Swift.
class alignas(1 << TypeAlignInBits) TypeBase {
  
  friend class ASTContext;
  TypeBase(const TypeBase&) = delete;
  void operator=(const TypeBase&) = delete;
  
  /// CanonicalType - This field is always set to the ASTContext for canonical
  /// types, and is otherwise lazily populated by ASTContext when the canonical
  /// form of a non-canonical type is requested.
  llvm::PointerUnion<TypeBase *, const ASTContext *> CanonicalType;

  /// Kind - The discriminator that indicates what subclass of type this is.
  const TypeKind Kind;

  struct TypeBaseBitfields {
    unsigned Properties : RecursiveTypeProperties::BitWidth;
  };

  enum { NumTypeBaseBits = RecursiveTypeProperties::BitWidth };
  static_assert(NumTypeBaseBits <= 32, "fits in an unsigned");

  /// Returns true if the given type is a sugared type.
  ///
  /// Only intended for use in compile-time assertions.
  // Specializations of this are at the end of the file.
  template <typename T>
  static constexpr bool isSugaredType() {
    return false;
  }

protected:
  struct ErrorTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// Whether there is an original type.
    unsigned HasOriginalType : 1;
  };
  enum { NumErrorTypeBits = NumTypeBaseBits + 1 };
  static_assert(NumErrorTypeBits <= 32, "fits in an unsigned");

  struct AnyFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// Extra information which affects how the function is called, like
    /// regparm and the calling convention.
    unsigned ExtInfo : 16;
  };
  enum { NumAnyFunctionTypeBits = NumTypeBaseBits + 16 };
  static_assert(NumAnyFunctionTypeBits <= 32, "fits in an unsigned");

  struct ArchetypeTypeBitfields {
    unsigned : NumTypeBaseBits;

    unsigned ExpandedNestedTypes : 1;
    unsigned HasSuperclass : 1;
    unsigned HasLayoutConstraint : 1;
    unsigned NumProtocols : 16;
  };
  enum { NumArchetypeTypeBitfields = NumTypeBaseBits + 19 };
  static_assert(NumArchetypeTypeBitfields <= 32, "fits in an unsigned");

  struct TypeVariableTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// \brief The unique number assigned to this type variable.
    unsigned ID : 32 - NumTypeBaseBits;
  };
  enum { NumTypeVariableTypeBits = NumTypeBaseBits + (32 - NumTypeBaseBits) };
  static_assert(NumTypeVariableTypeBits <= 32, "fits in an unsigned");

  struct SILFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;
    unsigned ExtInfo : 16;
    unsigned CalleeConvention : 3;
    unsigned HasErrorResult : 1;
  };
  enum { NumSILFunctionTypeBits = NumTypeBaseBits + 16+5 };
  static_assert(NumSILFunctionTypeBits <= 32, "fits in an unsigned");

  struct AnyMetatypeTypeBitfields {
    unsigned : NumTypeBaseBits;
    /// The representation of the metatype.
    ///
    /// Zero indicates that no representation has been set; otherwise,
    /// the value is the representation + 1
    unsigned Representation : 2;
  };
  enum { NumAnyMetatypeTypeBits = NumTypeBaseBits + 2 };
  static_assert(NumAnyMetatypeTypeBits <= 32, "fits in an unsigned");
  
  union {
    TypeBaseBitfields TypeBaseBits;
    ErrorTypeBitfields ErrorTypeBits;
    AnyFunctionTypeBitfields AnyFunctionTypeBits;
    TypeVariableTypeBitfields TypeVariableTypeBits;
    ArchetypeTypeBitfields ArchetypeTypeBits;
    SILFunctionTypeBitfields SILFunctionTypeBits;
    AnyMetatypeTypeBitfields AnyMetatypeTypeBits;
  };

protected:
  TypeBase(TypeKind kind, const ASTContext *CanTypeCtx,
           RecursiveTypeProperties properties)
    : CanonicalType((TypeBase*)nullptr), Kind(kind) {
    // If this type is canonical, switch the CanonicalType union to ASTContext.
    if (CanTypeCtx)
      CanonicalType = CanTypeCtx;
    setRecursiveProperties(properties);
  }

  void setRecursiveProperties(RecursiveTypeProperties properties) {
    TypeBaseBits.Properties = properties.getBits();
  }

public:
  /// getKind - Return what kind of type this is.
  TypeKind getKind() const { return Kind; }

  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return CanonicalType.is<const ASTContext *>(); }
  
  /// hasCanonicalTypeComputed - Return true if we've already computed a
  /// canonical version of this type.
  bool hasCanonicalTypeComputed() const { return !CanonicalType.isNull(); }
  
  /// getCanonicalType - Return the canonical version of this type, which has
  /// sugar from all levels stripped off.
  CanType getCanonicalType();

  /// getASTContext - Return the ASTContext that this type belongs to.
  ASTContext &getASTContext() {
    // If this type is canonical, it has the ASTContext in it.
    if (CanonicalType.is<const ASTContext *>())
      return const_cast<ASTContext&>(*CanonicalType.get<const ASTContext *>());
    // If not, canonicalize it to get the Context.
    return const_cast<ASTContext&>(*getCanonicalType()->
                                   CanonicalType.get<const ASTContext *>());
  }
  
  /// isEqual - Return true if these two types are equal, ignoring sugar.
  bool isEqual(Type Other);
  
  /// isSpelledLike - Return true if these two types share a sugared spelling.
  bool isSpelledLike(Type Other);
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();
  
  /// If this type is a (potentially sugared) type of the specified kind, remove
  /// the minimal amount of sugar required to get a pointer to the type.
  template <typename T>
  T *getAs() {
    static_assert(!isSugaredType<T>(), "getAs desugars types");
    return dyn_cast<T>(getDesugaredType());
  }

  template <typename T>
  bool is() {
    static_assert(!isSugaredType<T>(), "isa desugars types");
    return isa<T>(getDesugaredType());
  }
  
  template <typename T>
  T *castTo() {
    static_assert(!isSugaredType<T>(), "castTo desugars types");
    return cast<T>(getDesugaredType());
  }

  /// getRecursiveProperties - Returns the properties defined on the
  /// structure of this type.
  RecursiveTypeProperties getRecursiveProperties() const {
    return RecursiveTypeProperties(TypeBaseBits.Properties);
  }

  /// isMaterializable - Is this type 'materializable' according to
  /// the rules of the language?  Basically, does it not contain any
  /// l-value types?
  bool isMaterializable() const {
    return getRecursiveProperties().isMaterializable();
  }

  /// hasReferenceSemantics() - Do objects of this type have reference
  /// semantics?
  bool hasReferenceSemantics();

  /// Is this an uninhabited type, such as 'Never'?
  bool isUninhabited();

  /// Is this the 'Any' type?
  bool isAny();

  /// Are values of this type essentially just class references,
  /// possibly with some sort of additional information?
  ///
  ///   - any of the builtin reference types
  ///   - a class type
  ///   - a bound generic class type
  ///   - a class-bounded archetype type
  ///   - a class-bounded existential type
  ///   - a dynamic Self type
  bool isAnyClassReferenceType();
  
  /// allowsOwnership() - Are variables of this type permitted to have
  /// ownership attributes?
  bool allowsOwnership();  

  /// \brief Determine whether this type involves a type variable.
  bool hasTypeVariable() const {
    return getRecursiveProperties().hasTypeVariable();
  }

  /// \brief Determine where this type is a type variable or a dependent
  /// member root in a type variable.
  bool isTypeVariableOrMember();

  /// \brief Determine whether this type involves a UnresolvedType.
  bool hasUnresolvedType() const {
    return getRecursiveProperties().hasUnresolvedType();
  }

  /// \brief Determine whether the type involves an inout type, except
  /// as a function input.
  bool hasInOut() const {
    return getRecursiveProperties().hasInOut();
  }

  /// \brief Determine whether the type involves an archetype.
  bool hasArchetype() const {
    return getRecursiveProperties().hasArchetype();
  }

  /// Determine whether the type involves an opened existential archetype.
  bool hasOpenedExistential() const {
    return getRecursiveProperties().hasOpenedExistential();
  }

  /// Determine whether the type involves the given opened existential
  /// archetype.
  bool hasOpenedExistential(ArchetypeType *opened);

  /// Determine whether the type is an opened existential type.
  ///
  /// To determine whether there is an opened existential type
  /// anywhere in the type, use \c hasOpenedExistential.
  bool isOpenedExistential();

  /// Retrieve the set of opened existential archetypes that occur
  /// within this type.
  void getOpenedExistentials(SmallVectorImpl<ArchetypeType *> &opened);

  /// Erase the given opened existential type by replacing it with its
  /// existential type throughout the given type.
  Type eraseOpenedExistential(ArchetypeType *opened);

  /// Erase DynamicSelfType from the given type by replacing it with its
  /// underlying type.
  Type eraseDynamicSelfType();

  /// \brief Compute and return the set of type variables that occur within this
  /// type.
  ///
  /// \param typeVariables This vector is populated with the set of
  /// type variables referenced by this type.
  void getTypeVariables(SmallVectorImpl<TypeVariableType *> &typeVariables);

  /// Determine whether this type is a type parameter, which is either a
  /// GenericTypeParamType or a DependentMemberType.
  ///
  /// Note that this routine will return \c false for types that include type
  /// parameters in nested positions, e.g, \c T is a type parameter but
  /// \c X<T> is not a type parameter. Use \c hasTypeParameter to determine
  /// whether a type parameter exists at any position.
  bool isTypeParameter();

  /// Determine whether this type contains a type parameter somewhere in it.
  bool hasTypeParameter() {
    return getRecursiveProperties().hasTypeParameter();
  }

  /// Determines whether this type is an lvalue. This includes both straight
  /// lvalue types as well as tuples or optionals of lvalues.
  bool isLValueType() {
    return getRecursiveProperties().isLValue();
  }

  /// Determine whether the type is dependent on DynamicSelf.
  bool hasDynamicSelfType() const {
    return getRecursiveProperties().hasDynamicSelf();
  }

  /// Determine whether the type contains an unbound generic type.
  bool hasUnboundGenericType() const {
    return getRecursiveProperties().hasUnboundGeneric();
  }

  /// Determine whether this type contains an error type.
  bool hasError() const {
    return getRecursiveProperties().hasError();
  }

  /// \brief Check if this type is a valid type for the LHS of an assignment.
  /// This mainly means isLValueType(), but empty tuples and tuples of empty
  /// tuples also qualify.
  bool isAssignableType();

  /// isExistentialType - Determines whether this type is an existential type,
  /// whose real (runtime) type is unknown but which is known to conform to
  /// some set of protocols. Protocol and protocol-conformance types are
  /// existential types.
  bool isExistentialType();

  /// isAnyExistentialType - Determines whether this type is any kind of
  /// existential type: a protocol type, a protocol composition type, or
  /// an existential metatype.
  bool isAnyExistentialType();

  /// isObjCExistentialType - Determines whether this type is an
  /// class-bounded existential type whose required conformances are
  /// all @objc.  Such types are compatible with ObjC.
  bool isObjCExistentialType();
  
  /// Determines whether this type is an existential type with a class protocol
  /// bound.
  bool isClassExistentialType();

  /// isExistentialType - Determines whether this type is an existential type,
  /// whose real (runtime) type is unknown but which is known to conform to
  /// some set of protocols. Protocol and protocol-conformance types are
  /// existential types.
  ///
  /// \param Protocols If the type is an existential type, this vector is
  /// populated with the set of protocols
  bool isExistentialType(SmallVectorImpl<ProtocolDecl *> &Protocols);

  /// isAnyExistentialType - Determines whether this type is any kind of
  /// existential type: a protocol type, a protocol composition type, or
  /// an existential metatype.
  ///
  /// \param protocols If the type is an existential type, this vector is
  /// populated with the set of protocols.
  bool isAnyExistentialType(SmallVectorImpl<ProtocolDecl *> &protocols);

  /// Given that this type is any kind of existential type, produce
  /// its list of protocols.
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

  /// Determines the element type of a known *UnsafeMutablePointer
  /// variant, or returns null if the type is not a pointer.
  Type getAnyPointerElementType(PointerTypeKind &PTK);
  Type getAnyPointerElementType() {
    PointerTypeKind Ignore;
    return getAnyPointerElementType(Ignore);
  }
  
  /// \brief Determine whether the given type is "specialized", meaning that
  /// it involves generic types for which generic arguments have been provided.
  /// For example, the types Vector<Int> and Vector<Int>.Element are both
  /// specialized, but the type Vector is not.
  bool isSpecialized();

  /// Gather all of the substitutions used to produce the given specialized type
  /// from its unspecialized type.
  ///
  /// \returns ASTContext-allocated substitutions.
  SubstitutionList gatherAllSubstitutions(
                           ModuleDecl *module,
                           LazyResolver *resolver,
                           DeclContext *gpContext = nullptr);

  /// \brief Determine whether the given type is "generic", meaning that
  /// it involves generic types for which generic arguments have not been
  /// provided.
  /// For example, the type Vector and Vector<Int>.InnerGeneric are both
  /// unspecialized generic, but the type Vector<Int> is not.
  bool isUnspecializedGeneric();

  /// \brief Determine whether this type is a legal, lowered SIL type.
  ///
  /// A type is SIL-illegal if it is:
  ///   - an l-value type,
  ///   - an AST function type (i.e. subclasses of AnyFunctionType), or
  ///   - a tuple type with a SIL-illegal element type.
  bool isLegalSILType();

  /// \brief Check if this type is equal to the empty tuple type.
  bool isVoid();

  /// \brief Check if this type is equal to Swift.Bool.
  bool isBool();

  /// \brief Check if this type is equal to Builtin.IntN.
  bool isBuiltinIntegerType(unsigned bitWidth);

  /// \brief If this is a class type or a bound generic class type, returns the
  /// (possibly generic) class.
  ClassDecl *getClassOrBoundGenericClass();
  
  /// \brief If this is a struct type or a bound generic struct type, returns
  /// the (possibly generic) class.
  StructDecl *getStructOrBoundGenericStruct();
  
  /// \brief If this is an enum or a bound generic enum type, returns the
  /// (possibly generic) enum.
  EnumDecl *getEnumOrBoundGenericEnum();
  
  /// \brief Determine whether this type may have a superclass, which holds for
  /// classes, bound generic classes, and archetypes that are only instantiable
  /// with a class type.
  bool mayHaveSuperclass();

  /// \brief Determine whether this type can be used as a base type for AST
  /// name lookup, which is the case for nominal types, protocol compositions
  /// and archetypes.
  ///
  /// Generally, the static vs instance and mutating vs nonmutating distinction
  /// is handled elsewhere, so metatypes, lvalue types and inout types are not
  /// allowed here.
  ///
  /// Similarly, tuples formally have members, but this does not go through
  /// name lookup.
  bool mayHaveMembers() {
    return (is<ArchetypeType>() ||
            is<ModuleType>() ||
            isExistentialType() ||
            getAnyNominal());
  }

  /// \brief Retrieve the superclass of this type.
  ///
  /// \param resolver The resolver for lazy type checking, or null if the
  ///                 AST is already type-checked.
  ///
  /// \returns The superclass of this type, or a null type if it has no
  ///          superclass.
  Type getSuperclass(LazyResolver *resolver);
  
  /// \brief True if this type is the exact superclass of another type.
  ///
  /// \param ty       The potential subclass.
  /// \param resolver The resolver for lazy type checking, or null if the
  ///                 AST is already type-checked.
  ///
  /// \returns True if this type is \c ty or a superclass of \c ty.
  ///
  /// If this type is a bound generic class \c Foo<T>, the method only
  /// returns true if the generic parameters of \c ty exactly match the
  /// superclass of \c ty. For instance, if \c ty is a
  /// class DerivedClass: Base<Int>, then \c Base<T> (where T is an archetype)
  /// will return false. `isBindableToSuperclassOf` should be used
  /// for queries that care whether a generic class type can be substituted into
  /// a type's subclass.
  bool isExactSuperclassOf(Type ty, LazyResolver *resolver);

  /// \brief Get the substituted base class type, starting from a base class
  /// declaration and a substituted derived class type.
  ///
  /// For example, given the following declarations:
  ///
  /// class A<T, U> {}
  /// class B<V> : A<Int, V> {}
  /// class C<X, Y> : B<Y> {}
  ///
  /// Calling `C<String, NSObject>`->getSuperclassForDecl(`A`) will return
  /// `A<Int, NSObject>`.
  Type getSuperclassForDecl(const ClassDecl *classDecl, LazyResolver *resolver);

  /// \brief True if this type is the superclass of another type, or a generic
  /// type that could be bound to the superclass.
  ///
  /// \param ty       The potential subclass.
  /// \param resolver The resolver for lazy type checking, or null if the
  ///                 AST is already type-checked.
  ///
  /// \returns True if this type is \c ty, a superclass of \c ty, or an
  ///          archetype-parameterized type that can be bound to a superclass
  ///          of \c ty.
  bool isBindableToSuperclassOf(Type ty, LazyResolver *resolver);

  /// True if this type contains archetypes that could be substituted with
  /// concrete types to form the argument type.
  bool isBindableTo(Type ty, LazyResolver *resolver);

  /// \brief Retrieve the layout constraint of this type.
  ///
  /// \returns The layout constraint of this type, or a null layout constraint
  ///          if it has no layout constraint.
  LayoutConstraint getLayoutConstraint();

  /// \brief Determines whether this type is permitted as a method override
  /// of the \p other.
  bool canOverride(Type other, OverrideMatchMode matchMode,
                   LazyResolver *resolver);

  /// \brief Determines whether this type has a retainable pointer
  /// representation, i.e. whether it is representable as a single,
  /// possibly nil pointer that can be unknown-retained and
  /// unknown-released.
  bool hasRetainablePointerRepresentation();

  /// \brief Given that this type is a reference type, is it known to use
  /// Swift-native reference counting?
  bool usesNativeReferenceCounting(ResilienceExpansion resilience);

  /// Determines whether this type has a bridgeable object
  /// representation, i.e., whether it is always represented as a single
  /// (non-nil) pointer that can be unknown-retained and
  /// unknown-released.
  ///
  /// This predicate covers all types that can be placed into an
  /// AnyObject without ever requiring a representation change. Note that this
  /// excludes ObjC class metatypes, which may need to be wrapped or unwrapped
  /// when converting from native representation to AnyObject representation.
  bool isBridgeableObjectType();

  /// Determine whether this type is a potentially-bridged value type.
  ///
  /// This predicate doesn't guarantee that the type is bridged, but rather is
  /// a quick way to check whether the type is a value type that could
  /// conceivably be bridged to an Objective-C class type.
  bool isPotentiallyBridgedValueType();

  /// \brief If this is a nominal type or a bound generic nominal type,
  /// returns the (possibly generic) nominal type declaration.
  NominalTypeDecl *getNominalOrBoundGenericNominal();

  /// \brief If this is a nominal type, bound generic nominal type, or
  /// unbound generic nominal type, return the (possibly generic) nominal type
  /// declaration.
  NominalTypeDecl *getAnyNominal();

  /// Determine whether the given type is representable in the given
  /// foreign language.
  std::pair<ForeignRepresentableKind, ProtocolConformance *>
  getForeignRepresentableIn(ForeignLanguage language, const DeclContext *dc);

  /// Determines whether the given Swift type is representable within
  /// the given foreign language.
  ///
  /// A given Swift type is representable in the given foreign
  /// language if the Swift type can be used from source code written
  /// in that language.
  bool isRepresentableIn(ForeignLanguage language, const DeclContext *dc);

  /// Determines whether the type is trivially representable within
  /// the foreign language, meaning that it is both representable in
  /// that language and that the runtime representations are
  /// equivalent.
  bool isTriviallyRepresentableIn(ForeignLanguage language,
                                  const DeclContext *dc);

  /// \brief Given that this is a nominal type or bound generic nominal
  /// type, return its parent type; this will be a null type if the type
  /// is not a nested type.
  Type getNominalParent();

  /// \brief If this is a GenericType, bound generic nominal type, or
  /// unbound generic nominal type, return the (possibly generic) nominal type
  /// declaration.
  GenericTypeDecl *getAnyGeneric();

  /// getUnlabeledType - Retrieve a version of this type with all labels
  /// removed at every level. For example, given a tuple type 
  /// \code
  /// (p : (x : int, y : int))
  /// \endcode
  /// the result would be the (parenthesized) type ((int, int)).
  Type getUnlabeledType(ASTContext &Context);

  /// Retrieve the type without any labels around it. For example, given
  /// \code
  /// (p : int)
  /// \endcode
  /// the result would be the (unparenthesized) type 'int'.
  Type getWithoutImmediateLabel();

  /// Retrieve the type without any parentheses around it.
  Type getWithoutParens();

  /// Replace the base type of the result type of the given function
  /// type with a new result type, as per a DynamicSelf or other
  /// covariant return transformation.  The optionality of the
  /// existing result will be preserved.
  ///
  /// \param newResultType The new result type.
  ///
  /// \param uncurryLevel The number of uncurry levels to apply before
  /// replacing the type. With uncurry level == 0, this simply
  /// replaces the current type with the new result type.
  Type replaceCovariantResultType(Type newResultType,
                                  unsigned uncurryLevel);

  /// Returns a new function type exactly like this one but with the self
  /// parameter replaced. Only makes sense for function members of types.
  Type replaceSelfParameterType(Type newSelf);

  /// getRValueType - For an @lvalue type, retrieves the underlying object type.
  /// Otherwise, returns the type itself.
  Type getRValueType();

  /// getRValueType - For an @lvalue type or tuple containing a single
  /// non-variadic element, retrieves the underlying object type.
  /// Otherwise, returns the type itself.
  Type getRValueObjectType();

  /// getInOutObjectType - For an inout type, retrieves the underlying object
  /// type.  Otherwise, returns the type itself.
  Type getInOutObjectType();

  /// getLValueOrInOutObjectType - For an @lvalue or inout type, retrieves the
  /// underlying object type. Otherwise, returns the type itself.
  Type getLValueOrInOutObjectType();

  /// Retrieves the rvalue instance type, looking through single-element
  /// tuples, inout types, and metatypes.
  Type getRValueInstanceType();

  /// For a ReferenceStorageType like @unowned, this returns the referent.
  /// Otherwise, it returns the type itself.
  Type getReferenceStorageReferent();

  /// Determine the set of substitutions that should be applied to a
  /// type spelled within the given DeclContext to treat it as a
  /// member of this type.
  ///
  /// For example, given:
  /// \code
  /// struct X<T, U> { }
  /// extension X {
  ///   typealias SomeArray = [T]
  /// }
  /// \endcode
  ///
  /// Asking for the member substitutions of \c X<Int,String> within
  /// the context of the extension above will produce substitutions T
  /// -> Int and U -> String suitable for mapping the type of
  /// \c SomeArray.
  SubstitutionMap getContextSubstitutionMap(ModuleDecl *module,
                                            const DeclContext *dc);

  /// Deprecated version of the above.
  TypeSubstitutionMap getContextSubstitutions(const DeclContext *dc);

  /// Get the substitutions to apply to the type of the given member as seen
  /// from this base type.
  ///
  /// \param genericEnv If non-null, generic parameters of the member are
  /// mapped to context archetypes of this generic environment.
  SubstitutionMap getMemberSubstitutionMap(ModuleDecl *module,
                                           const ValueDecl *member,
                                           GenericEnvironment *genericEnv=nullptr);

  /// Deprecated version of the above.
  TypeSubstitutionMap getMemberSubstitutions(const ValueDecl *member,
                                             GenericEnvironment *genericEnv=nullptr);

  /// Retrieve the type of the given member as seen through the given base
  /// type, substituting generic arguments where necessary.
  ///
  /// This routine allows one to take a concrete type (the "this" type) and
  /// and a member of that type (or one of its superclasses), then determine
  /// what type an access to that member through the base type will have.
  /// For example, given:
  ///
  /// \code
  /// class Vector<T> {
  ///   func add(value : T) { }
  /// }
  /// \endcode
  ///
  /// Given the type \c Vector<Int> and the member \c add, the resulting type
  /// of the member will be \c (self : Vector<Int>) -> (value : Int) -> ().
  ///
  /// \param module The module in which the substitution occurs.
  ///
  /// \param member The member whose type we are substituting.
  ///
  /// \param memberType The type of the member, in which archetypes will be
  /// replaced by the generic arguments provided by the base type. If null,
  /// the member's type will be used.
  ///
  /// \returns the resulting member type.
  Type getTypeOfMember(ModuleDecl *module, const ValueDecl *member,
                       Type memberType = Type());

  /// Get the type of a superclass member as seen from the subclass,
  /// substituting generic parameters, dynamic Self return, and the
  /// 'self' argument type as appropriate.
  Type adjustSuperclassMemberDeclType(const ValueDecl *baseDecl,
                                      const ValueDecl *derivedDecl,
                                      Type memberType,
                                      LazyResolver *resolver);

  /// Return T if this type is Optional<T>; otherwise, return the null type.
  Type getOptionalObjectType();

  /// Return T if this type is ImplicitlyUnwrappedOptional<T>; otherwise, return
  /// the null type.
  Type getImplicitlyUnwrappedOptionalObjectType();

  /// Return T if this type is Optional<T> or ImplicitlyUnwrappedOptional<T>;
  /// otherwise, return the null type.
  Type getAnyOptionalObjectType(OptionalTypeKind &kind);
  Type getAnyOptionalObjectType() {
    OptionalTypeKind ignored;
    return getAnyOptionalObjectType(ignored);
  }

  // Return type underlying type of a swift_newtype annotated imported struct;
  // otherwise, return the null type.
  Type getSwiftNewtypeUnderlyingType();

  /// Return the type T after looking through all of the optional or
  /// implicitly-unwrapped optional types.
  Type lookThroughAllAnyOptionalTypes();

  /// Return the type T after looking through all of the optional or
  /// implicitly-unwrapped optional types.
  Type lookThroughAllAnyOptionalTypes(SmallVectorImpl<Type> &optionals);

  /// Whether this is the AnyObject type.
  bool isAnyObject();

  /// Whether this is an existential composition containing
  /// Error.
  bool isExistentialWithError();

  void dump() const LLVM_ATTRIBUTE_USED;
  void dump(raw_ostream &os, unsigned indent = 0) const;

  void dumpPrint() const LLVM_ATTRIBUTE_USED;
  void print(raw_ostream &OS,
             const PrintOptions &PO = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &PO) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;
  
  /// Return whether this type is or can be substituted for a bridgeable
  /// object type.
  TypeTraitResult canBeClass();

private:
  // Make vanilla new/delete illegal for Types.
  void *operator new(size_t Bytes) throw() = delete;
  void operator delete(void *Data) throw() = delete;
public:
  // Only allow allocation of Types using the allocator in ASTContext
  // or by doing a placement new.
  void *operator new(size_t bytes, const ASTContext &ctx,
                     AllocationArena arena, unsigned alignment = 8);
  void *operator new(size_t Bytes, void *Mem) throw() { return Mem; }
};

/// ErrorType - This represents a type that was erroneously constructed.  This
/// is produced when parsing types and when name binding type aliases, and is
/// installed in declaration that use these erroneous types.  All uses of a
/// declaration of invalid type should be ignored and not re-diagnosed.
class ErrorType : public TypeBase {
  friend class ASTContext;
  // The Error type is always canonical.
  ErrorType(ASTContext &C, Type originalType,
            RecursiveTypeProperties properties)
      : TypeBase(TypeKind::Error, &C, properties) {
    assert(properties.hasError());
    if (originalType) {
      ErrorTypeBits.HasOriginalType = true;
      *reinterpret_cast<Type *>(this + 1) = originalType;
    } else {
      ErrorTypeBits.HasOriginalType = false;
    }
  }

public:
  static Type get(const ASTContext &C);

  /// Produce an error type which records the original type we were trying to
  /// substitute when we ran into a problem.
  static Type get(Type originalType);

  /// Retrieve the original type that this error type replaces, or none if
  /// there is no such type.
  Type getOriginalType() const {
    if (ErrorTypeBits.HasOriginalType)
      return *reinterpret_cast<const Type *>(this + 1);

    return Type();
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Error;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ErrorType, Type)

/// UnresolvedType - This represents a type variable that cannot be resolved to
/// a concrete type because the expression is ambiguous.  This is produced when
/// parsing expressions and producing diagnostics.  Any instance of this should
/// cause the entire expression to be ambiguously typed.
class UnresolvedType : public TypeBase {
  friend class ASTContext;
  // The Unresolved type is always canonical.
  UnresolvedType(ASTContext &C)
    : TypeBase(TypeKind::Unresolved, &C,
       RecursiveTypeProperties(RecursiveTypeProperties::HasUnresolvedType)) { }
public:
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Unresolved;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(UnresolvedType, Type)

  
/// BuiltinType - An abstract class for all the builtin types.
class BuiltinType : public TypeBase {
protected:
  BuiltinType(TypeKind kind, const ASTContext &canTypeCtx)
  : TypeBase(kind, &canTypeCtx, RecursiveTypeProperties()) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_BuiltinType &&
           T->getKind() <= TypeKind::Last_BuiltinType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinType, Type)

/// BuiltinRawPointerType - The builtin raw (and dangling) pointer type.  This
/// pointer is completely unmanaged and is equivalent to i8* in LLVM IR.
class BuiltinRawPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinRawPointerType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinRawPointer, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinRawPointer;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinRawPointerType, BuiltinType);

/// BuiltinNativeObjectType - The builtin opaque object-pointer type.
/// Useful for keeping an object alive when it is otherwise being
/// manipulated via an unsafe pointer type.
class BuiltinNativeObjectType : public BuiltinType {
  friend class ASTContext;
  BuiltinNativeObjectType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinNativeObject, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinNativeObject;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinNativeObjectType, BuiltinType);

/// A type that contains an owning reference to a heap object packed with
/// additional bits. The type uses a bit to discriminate native Swift objects
/// from Objective-C object pointers or tagged pointers.
class BuiltinBridgeObjectType : public BuiltinType {
  friend class ASTContext;
  BuiltinBridgeObjectType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinBridgeObject, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinBridgeObject;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinBridgeObjectType, BuiltinType);

/// BuiltinUnknownObjectType - The builtin opaque Objective-C pointer type.
/// Useful for pushing an Objective-C type through swift.
class BuiltinUnknownObjectType : public BuiltinType {
  friend class ASTContext;
  BuiltinUnknownObjectType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinUnknownObject, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinUnknownObject;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinUnknownObjectType, BuiltinType);

/// BuiltinUnsafeValueBufferType - The builtin opaque fixed-size value
/// buffer type, into which storage for an arbitrary value can be
/// allocated using Builtin.allocateValueBuffer.
///
/// This type is unsafe because it does not permit ordinary value
/// operations.  It is essentially an Any without any type
/// information.  It should only be used in narrow circumstances in
/// carefully-written SIL.
class BuiltinUnsafeValueBufferType : public BuiltinType {
  friend class ASTContext;
  BuiltinUnsafeValueBufferType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinUnsafeValueBuffer, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinUnsafeValueBuffer;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinUnsafeValueBufferType, BuiltinType);

/// \brief A builtin vector type.
class BuiltinVectorType : public BuiltinType, public llvm::FoldingSetNode {
  Type elementType;
  unsigned numElements;

  friend class ASTContext;

  BuiltinVectorType(const ASTContext &context, Type elementType,
                    unsigned numElements)
    : BuiltinType(TypeKind::BuiltinVector, context),
      elementType(elementType), numElements(numElements) { }

public:
  static BuiltinVectorType *get(const ASTContext &context, Type elementType,
                                unsigned numElements);

  /// \brief Retrieve the type of this vector's elements.
  Type getElementType() const { return elementType; }

  /// \brief Retrieve the number of elements in this vector.
  unsigned getNumElements() const { return numElements; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getElementType(), getNumElements());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, Type elementType,
                      unsigned numElements) {
    ID.AddPointer(elementType.getPointer());
    ID.AddInteger(numElements);
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinVector;
  }
};
BEGIN_CAN_TYPE_WRAPPER(BuiltinVectorType, BuiltinType)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getElementType)
END_CAN_TYPE_WRAPPER(BuiltinVectorType, BuiltinType)

/// Size descriptor for a builtin integer type. This is either a fixed bit
/// width or an abstract target-dependent value such as "size of a pointer".
class BuiltinIntegerWidth {
  /// Tag values for abstract integer sizes.
  enum : unsigned {
    Least_SpecialValue = ~2U,
    /// The size of a pointer on the target system.
    PointerWidth = ~0U,
    
    /// Inhabitants stolen for use as DenseMap special values.
    DenseMapEmpty = ~1U,
    DenseMapTombstone = ~2U,
  };
  
  unsigned RawValue;
  
  friend struct llvm::DenseMapInfo<swift::BuiltinIntegerWidth>;
  
  /// Private constructor from a raw symbolic value.
  explicit BuiltinIntegerWidth(unsigned RawValue) : RawValue(RawValue) {}
public:
  BuiltinIntegerWidth() : RawValue(0) {}
  
  static BuiltinIntegerWidth fixed(unsigned bitWidth) {
    assert(bitWidth < Least_SpecialValue && "invalid bit width");
    return BuiltinIntegerWidth(bitWidth);
  }
  
  static BuiltinIntegerWidth pointer() {
    return BuiltinIntegerWidth(PointerWidth);
  }
  
  /// Is this a fixed width?
  bool isFixedWidth() const { return RawValue < Least_SpecialValue; }

  /// Get the fixed width value. Fails if the width is abstract.
  unsigned getFixedWidth() const {
    assert(isFixedWidth() && "not fixed-width");
    return RawValue;
  }
  
  /// Is this the abstract target pointer width?
  bool isPointerWidth() const { return RawValue == PointerWidth; }
  
  /// Get the least supported value for the width.
  ///
  /// FIXME: This should be build-configuration-dependent.
  unsigned getLeastWidth() const {
    if (isFixedWidth())
      return getFixedWidth();
    if (isPointerWidth())
      return 32;
    llvm_unreachable("impossible width value");
  }
  
  /// Get the greatest supported value for the width.
  ///
  /// FIXME: This should be build-configuration-dependent.
  unsigned getGreatestWidth() const {
    if (isFixedWidth())
      return getFixedWidth();
    if (isPointerWidth())
      return 64;
    llvm_unreachable("impossible width value");
  }
  
  friend bool operator==(BuiltinIntegerWidth a, BuiltinIntegerWidth b) {
    return a.RawValue == b.RawValue;
  }
  friend bool operator!=(BuiltinIntegerWidth a, BuiltinIntegerWidth b) {
    return a.RawValue != b.RawValue;
  }
};

/// The builtin integer types.  These directly correspond
/// to LLVM IR integer types.  They lack signedness and have an arbitrary
/// bitwidth.
class BuiltinIntegerType : public BuiltinType {
  friend class ASTContext;
private:
  BuiltinIntegerWidth Width;
  BuiltinIntegerType(BuiltinIntegerWidth BitWidth, const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinInteger, C), Width(BitWidth) {}
  
public:
  /// Get a builtin integer type.
  static BuiltinIntegerType *get(BuiltinIntegerWidth BitWidth,
                                 const ASTContext &C);
  
  /// Get a builtin integer type of fixed width.
  static BuiltinIntegerType *get(unsigned BitWidth, const ASTContext &C) {
    return get(BuiltinIntegerWidth::fixed(BitWidth), C);
  }
  
  /// Get the target-pointer-width builtin integer type.
  static BuiltinIntegerType *getWordType(const ASTContext &C) {
    return get(BuiltinIntegerWidth::pointer(), C);
  }
  
  /// Return the bit width of the integer.
  BuiltinIntegerWidth getWidth() const {
    return Width;
  }
  
  /// Is the integer fixed-width?
  bool isFixedWidth() const {
    return Width.isFixedWidth();
  }
  
  /// Is the integer fixed-width with the given width?
  bool isFixedWidth(unsigned width) const {
    return Width.isFixedWidth() && Width.getFixedWidth() == width;
  }
  
  /// Get the fixed integer width. Fails if the integer has abstract width.
  unsigned getFixedWidth() const {
    return Width.getFixedWidth();
  }
  
  /// Return the least supported width of the integer.
  ///
  /// FIXME: This should be build-configuration-dependent.
  unsigned getLeastWidth() const {
    return Width.getLeastWidth();
  }
  
  /// Return the greatest supported width of the integer.
  ///
  /// FIXME: This should be build-configuration-dependent.
  unsigned getGreatestWidth() const {
    return Width.getGreatestWidth();
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinInteger;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinIntegerType, BuiltinType)
  
class BuiltinFloatType : public BuiltinType {
  friend class ASTContext;
public:
  enum FPKind {
    IEEE16, IEEE32, IEEE64, IEEE80, IEEE128, /// IEEE floating point types.
    PPC128   /// PowerPC "double double" type.
  };
private:
  FPKind Kind;
  
  BuiltinFloatType(FPKind Kind, const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinFloat, C), Kind(Kind) {}
public:
  
  /// getFPKind - Get the 
  FPKind getFPKind() const {
    return Kind;
  }

  const llvm::fltSemantics &getAPFloatSemantics() const;

  unsigned getBitWidth() const {
    switch (Kind) {
    case IEEE16: return 16;
    case IEEE32: return 32;
    case IEEE64: return 64;
    case IEEE80: return 80;
    case IEEE128:
    case PPC128: return 128;
    }
    llvm_unreachable("bad FPKind");
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinFloat;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinFloatType, BuiltinType)
  
/// NameAliasType - An alias type is a name for another type, just like a
/// typedef in C.
class NameAliasType : public TypeBase {
  friend class TypeAliasDecl;
  // NameAliasType are never canonical.
  NameAliasType(TypeAliasDecl *d) 
    : TypeBase(TypeKind::NameAlias, nullptr, RecursiveTypeProperties()),
      TheDecl(d) {}
  TypeAliasDecl *const TheDecl;

public:
  TypeAliasDecl *getDecl() const { return TheDecl; }

  using TypeBase::setRecursiveProperties;
   
  /// Remove one level of top-level sugar from this type.
  TypeBase *getSinglyDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::NameAlias;
  }
};

// TODO: As part of AST modernization, replace with a proper
// 'ParameterTypeElt' or similar, and have FunctionTypes only have a list
// of 'ParameterTypeElt's. Then, this information can be removed from
// TupleTypeElt.
//
/// Provide parameter type relevant flags, i.e. variadic, autoclosure, and
/// escaping.
class ParameterTypeFlags {
  enum ParameterFlags : uint8_t {
    None = 0,
    Variadic = 1 << 0,
    AutoClosure = 1 << 1,
    Escaping = 1 << 2,

    NumBits = 3
  };
  OptionSet<ParameterFlags> value;
  static_assert(NumBits < 8*sizeof(OptionSet<ParameterFlags>), "overflowed");

  ParameterTypeFlags(OptionSet<ParameterFlags, uint8_t> val) : value(val) {}

public:
  ParameterTypeFlags() = default;

  ParameterTypeFlags(bool variadic, bool autoclosure, bool escaping)
      : value((variadic ? Variadic : 0) |
              (autoclosure ? AutoClosure : 0) |
              (escaping ? Escaping : 0)) {}

  /// Create one from what's present in the parameter type
  inline static ParameterTypeFlags fromParameterType(Type paramTy,
                                                     bool isVariadic);

  bool isNone() const { return !value; }
  bool isVariadic() const { return value.contains(Variadic); }
  bool isAutoClosure() const { return value.contains(AutoClosure); }
  bool isEscaping() const { return value.contains(Escaping); }

  ParameterTypeFlags withEscaping(bool escaping) const {
    return ParameterTypeFlags(escaping ? value | ParameterTypeFlags::Escaping
                                       : value - ParameterTypeFlags::Escaping);
  }

  bool operator ==(const ParameterTypeFlags &other) const {
    return value.toRaw() == other.value.toRaw();
  }

  uint8_t toRaw() const { return value.toRaw(); }
};

/// ParenType - A paren type is a type that's been written in parentheses.
class ParenType : public TypeBase {
  Type UnderlyingType;
  ParameterTypeFlags parameterFlags;

  friend class ASTContext;
  ParenType(Type UnderlyingType, RecursiveTypeProperties properties,
            ParameterTypeFlags flags)
      : TypeBase(TypeKind::Paren, nullptr, properties),
        UnderlyingType(UnderlyingType), parameterFlags(flags) {}

public:
  Type getUnderlyingType() const { return UnderlyingType; }

  static ParenType *get(const ASTContext &C, Type underlying,
                        ParameterTypeFlags flags = {});

  /// Remove one level of top-level sugar from this type.
  TypeBase *getSinglyDesugaredType();

  /// Get the parameter flags
  ParameterTypeFlags getParameterFlags() const { return parameterFlags; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Paren;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
  /// An optional name for the field.
  Identifier Name;

  /// \brief This is the type of the field.
  Type ElementType;

  /// Flags that are specific to and relevant for parameter types
  ParameterTypeFlags Flags;

  friend class TupleType;

public:
  TupleTypeElt() = default;
  inline /*implicit*/ TupleTypeElt(Type ty, Identifier name,
                                   bool isVariadic, bool isAutoClosure,
                                   bool isEscaping);

  TupleTypeElt(Type ty, Identifier name = Identifier(),
               ParameterTypeFlags PTFlags = {})
      : Name(name), ElementType(ty), Flags(PTFlags) {}

  /*implicit*/ TupleTypeElt(TypeBase *Ty)
    : Name(Identifier()), ElementType(Ty), Flags() { }

  bool hasName() const { return !Name.empty(); }
  Identifier getName() const { return Name; }

  Type getType() const { return ElementType.getPointer(); }

  ParameterTypeFlags getParameterFlags() const { return Flags; }

  /// Determine whether this field is variadic.
  bool isVararg() const { return Flags.isVariadic(); }

  /// Determine whether this field is an autoclosure parameter closure.
  bool isAutoClosure() const { return Flags.isAutoClosure(); }

  /// Determine whether this field is an escaping parameter closure.
  bool isEscaping() const { return Flags.isEscaping(); }

  static inline Type getVarargBaseTy(Type VarArgT);

  /// Remove the type of this varargs element designator, without the array
  /// type wrapping it.
  Type getVarargBaseTy() const {
    assert(isVararg());
    return getVarargBaseTy(getType());
  }

  /// Retrieve a copy of this tuple type element with the type replaced.
  TupleTypeElt getWithType(Type T) const {
    return TupleTypeElt(T, getName(), getParameterFlags());
  }

  /// Retrieve a copy of this tuple type element with the name replaced.
  TupleTypeElt getWithName(Identifier name) const {
    return TupleTypeElt(getType(), name, getParameterFlags());
  }

  /// Retrieve a copy of this tuple type element with no name
  TupleTypeElt getWithoutName() const { return getWithName(Identifier()); }
};

inline Type getTupleEltType(const TupleTypeElt &elt) {
  return elt.getType();
}
typedef ArrayRefView<TupleTypeElt,Type,getTupleEltType> TupleEltTypeArrayRef;

inline CanType getCanTupleEltType(const TupleTypeElt &elt) {
  return CanType(elt.getType());
}
typedef ArrayRefView<TupleTypeElt,CanType,getCanTupleEltType>
  CanTupleEltTypeArrayRef;

/// TupleType - A tuple is a parenthesized list of types where each name has an
/// optional name.
///
class TupleType : public TypeBase, public llvm::FoldingSetNode {
  const ArrayRef<TupleTypeElt> Elements;
  
public:
  /// get - Return the uniqued tuple type with the specified elements.
  /// Returns a ParenType instead if there is exactly one element which
  /// is unlabeled and not varargs, so it doesn't accidentally construct
  /// a tuple which is impossible to write.
  static Type get(ArrayRef<TupleTypeElt> Elements, const ASTContext &C);

  /// getEmpty - Return the empty tuple type '()'.
  static CanTypeWrapper<TupleType> getEmpty(const ASTContext &C);

  /// getFields - Return the fields of this tuple.
  ArrayRef<TupleTypeElt> getElements() const { return Elements; }

  unsigned getNumElements() const { return Elements.size(); }

  const TupleTypeElt &getElement(unsigned i) const { return Elements[i]; }

  /// getElementType - Return the type of the specified element.
  Type getElementType(unsigned ElementNo) const {
    return Elements[ElementNo].getType();
  }

  TupleEltTypeArrayRef getElementTypes() const {
    return TupleEltTypeArrayRef(getElements());
  }
  
  /// getNamedElementId - If this tuple has an element with the specified name,
  /// return the element index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// getElementForScalarInit - If a tuple of this type can be initialized with
  /// a scalar, return the element number that the scalar is assigned to.  If
  /// not, return -1.
  int getElementForScalarInit() const;
  
  /// If this tuple has a varargs element to it, return the base type of the
  /// varargs element (i.e., if it is "Int...", this returns Int, not [Int]).
  /// Otherwise, this returns Type().
  Type getVarArgsBaseType() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Tuple;
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Elements);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      ArrayRef<TupleTypeElt> Elements);
  
private:
  TupleType(ArrayRef<TupleTypeElt> elements, const ASTContext *CanCtx,
            RecursiveTypeProperties properties)
     : TypeBase(TypeKind::Tuple, CanCtx, properties), Elements(elements) {
  }
};
BEGIN_CAN_TYPE_WRAPPER(TupleType, Type)
  CanType getElementType(unsigned elementNo) const {
    return CanType(getPointer()->getElementType(elementNo));
  }
  CanTupleEltTypeArrayRef getElementTypes() const {
    return CanTupleEltTypeArrayRef(getPointer()->getElements());
  }
END_CAN_TYPE_WRAPPER(TupleType, Type)

/// UnboundGenericType - Represents a generic type where the type arguments have
/// not yet been resolved.
class UnboundGenericType : public TypeBase, public llvm::FoldingSetNode {
  GenericTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

private:
  UnboundGenericType(GenericTypeDecl *TheDecl, Type Parent, const ASTContext &C,
                     RecursiveTypeProperties properties)
    : TypeBase(TypeKind::UnboundGeneric,
               (!Parent || Parent->isCanonical())? &C : nullptr,
               properties | RecursiveTypeProperties::HasUnboundGeneric),
      TheDecl(TheDecl), Parent(Parent) { }

public:
  static UnboundGenericType* get(GenericTypeDecl *TheDecl, Type Parent,
                                 const ASTContext &C);

  /// \brief Returns the declaration that declares this type.
  GenericTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the bound type Dictionary<String, Int>.Inner would be
  /// represented as an UnboundGenericType with Dictionary<String, Int> as its
  /// parent type.
  Type getParent() const { return Parent; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, GenericTypeDecl *D,
                      Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnboundGeneric;
  }
};
BEGIN_CAN_TYPE_WRAPPER(UnboundGenericType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getParent)
END_CAN_TYPE_WRAPPER(UnboundGenericType, Type)

inline CanType getAsCanType(const Type &type) { return CanType(type); }
typedef ArrayRefView<Type,CanType,getAsCanType> CanTypeArrayRef;

/// BoundGenericType - An abstract class for applying a generic type to the
/// given type arguments.
class BoundGenericType : public TypeBase, public llvm::FoldingSetNode {
  NominalTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;
  
  ArrayRef<Type> GenericArgs;
  

protected:
  BoundGenericType(TypeKind theKind, NominalTypeDecl *theDecl, Type parent,
                   ArrayRef<Type> genericArgs, const ASTContext *context,
                   RecursiveTypeProperties properties);

public:
  static BoundGenericType* get(NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the bound type Dictionary<String, Int>.Inner<Int> would be
  /// represented as a BoundGenericType with Dictionary<String, Int> as its
  /// parent type.
  Type getParent() const { return Parent; }

  /// Retrieve the set of generic arguments provided at this level.
  ArrayRef<Type> getGenericArgs() const { return GenericArgs; }

  void Profile(llvm::FoldingSetNodeID &ID) {
    RecursiveTypeProperties properties;
    Profile(ID, TheDecl, Parent, GenericArgs, properties);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *TheDecl,
                      Type Parent, ArrayRef<Type> GenericArgs,
                      RecursiveTypeProperties &properties);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_BoundGenericType &&
           T->getKind() <= TypeKind::Last_BoundGenericType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(BoundGenericType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getParent)
  CanTypeArrayRef getGenericArgs() const {
    return CanTypeArrayRef(getPointer()->getGenericArgs());
  }
END_CAN_TYPE_WRAPPER(BoundGenericType, Type)


/// BoundGenericClassType - A subclass of BoundGenericType for the case
/// when the nominal type is a generic class type.
class BoundGenericClassType : public BoundGenericType {
private:
  BoundGenericClassType(ClassDecl *theDecl, Type parent,
                        ArrayRef<Type> genericArgs, const ASTContext *context,
                        RecursiveTypeProperties properties)
    : BoundGenericType(TypeKind::BoundGenericClass,
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, properties) {}
  friend class BoundGenericType;

public:
  static BoundGenericClassType* get(ClassDecl *theDecl, Type parent,
                                    ArrayRef<Type> genericArgs) {
    return cast<BoundGenericClassType>(
             BoundGenericType::get(reinterpret_cast<NominalTypeDecl*>(theDecl),
                                   parent, genericArgs));
  }

  /// \brief Returns the declaration that declares this type.
  ClassDecl *getDecl() const {
    return reinterpret_cast<ClassDecl*>(BoundGenericType::getDecl());
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BoundGenericClass;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BoundGenericClassType, BoundGenericType)

/// BoundGenericEnumType - A subclass of BoundGenericType for the case
/// when the nominal type is a generic enum type.
class BoundGenericEnumType : public BoundGenericType {
private:
  BoundGenericEnumType(EnumDecl *theDecl, Type parent,
                       ArrayRef<Type> genericArgs, const ASTContext *context,
                       RecursiveTypeProperties properties)
    : BoundGenericType(TypeKind::BoundGenericEnum,
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, properties) {}
  friend class BoundGenericType;

public:
  static BoundGenericEnumType* get(EnumDecl *theDecl, Type parent,
                                    ArrayRef<Type> genericArgs) {
    return cast<BoundGenericEnumType>(
             BoundGenericType::get(reinterpret_cast<NominalTypeDecl*>(theDecl),
                                   parent, genericArgs));
  }

  /// \brief Returns the declaration that declares this type.
  EnumDecl *getDecl() const {
    return reinterpret_cast<EnumDecl*>(BoundGenericType::getDecl());
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BoundGenericEnum;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BoundGenericEnumType, BoundGenericType)

/// BoundGenericStructType - A subclass of BoundGenericType for the case
/// when the nominal type is a generic struct type.
class BoundGenericStructType : public BoundGenericType {
private:
  BoundGenericStructType(StructDecl *theDecl, Type parent,
                         ArrayRef<Type> genericArgs, const ASTContext *context,
                         RecursiveTypeProperties properties)
    : BoundGenericType(TypeKind::BoundGenericStruct, 
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, properties) {}
  friend class BoundGenericType;

public:
  static BoundGenericStructType* get(StructDecl *theDecl, Type parent,
                                    ArrayRef<Type> genericArgs) {
    return cast<BoundGenericStructType>(
             BoundGenericType::get(reinterpret_cast<NominalTypeDecl*>(theDecl),
                                   parent, genericArgs));
  }

  /// \brief Returns the declaration that declares this type.
  StructDecl *getDecl() const {
    return reinterpret_cast<StructDecl*>(BoundGenericType::getDecl());
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BoundGenericStruct;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BoundGenericStructType, BoundGenericType)

/// NominalType - Represents a type with a name that is significant, such that
/// the name distinguishes it from other structurally-similar types that have
/// different names. Nominal types are always canonical.
class NominalType : public TypeBase {
  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  NominalTypeDecl * const TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

protected:
  NominalType(TypeKind K, const ASTContext *C, NominalTypeDecl *TheDecl,
              Type Parent, RecursiveTypeProperties properties)
    : TypeBase(K, (!Parent || Parent->isCanonical())? C : nullptr,
               properties),
      TheDecl(TheDecl), Parent(Parent) { }

public:
  static NominalType *get(NominalTypeDecl *D, Type Parent, const ASTContext &C);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

  /// \brief Returns the type of the parent of this type. This will
  /// be null for top-level types or local types, and for non-generic types
  /// will simply be the same as the declared type of the declaration context
  /// of TheDecl. For types nested within generic types, however, this will
  /// involve \c BoundGenericType nodes that provide context for the nested
  /// type, e.g., the type Dictionary<String, Int>.ItemRange would be
  /// represented as a NominalType with Dictionary<String, Int> as its parent
  /// type.
  Type getParent() const { return Parent; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_NominalType &&
           T->getKind() <= TypeKind::Last_NominalType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(NominalType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getParent)
END_CAN_TYPE_WRAPPER(NominalType, Type)

/// EnumType - This represents the type declared by an EnumDecl.
class EnumType : public NominalType, public llvm::FoldingSetNode {
public:
  /// getDecl() - Returns the decl which declares this type.
  EnumDecl *getDecl() const {
    return reinterpret_cast<EnumDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given enum
  /// declaration in the parent type \c Parent.
  static EnumType *get(EnumDecl *D, Type Parent, const ASTContext &C);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, EnumDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Enum;
  }

private:
  EnumType(EnumDecl *TheDecl, Type Parent, const ASTContext &Ctx,
            RecursiveTypeProperties properties);
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(EnumType, NominalType)

/// StructType - This represents the type declared by a StructDecl.
class StructType : public NominalType, public llvm::FoldingSetNode {  
public:
  /// getDecl() - Returns the decl which declares this type.
  StructDecl *getDecl() const {
    return reinterpret_cast<StructDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given struct
  /// declaration in the parent type \c Parent.
  static StructType *get(StructDecl *D, Type Parent, const ASTContext &C);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, StructDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Struct;
  }
  
private:
  StructType(StructDecl *TheDecl, Type Parent, const ASTContext &Ctx,
             RecursiveTypeProperties properties);
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(StructType, NominalType)

/// ClassType - This represents the type declared by a ClassDecl.
class ClassType : public NominalType, public llvm::FoldingSetNode {  
public:
  /// getDecl() - Returns the decl which declares this type.
  ClassDecl *getDecl() const {
    return reinterpret_cast<ClassDecl *>(NominalType::getDecl());
  }

  /// \brief Retrieve the type when we're referencing the given class
  /// declaration in the parent type \c Parent.
  static ClassType *get(ClassDecl *D, Type Parent, const ASTContext &C);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, ClassDecl *D, Type Parent);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Class;
  }
  
private:
  ClassType(ClassDecl *TheDecl, Type Parent, const ASTContext &Ctx,
            RecursiveTypeProperties properties);
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ClassType, NominalType)

/// Describes the representation of a metatype.
///
/// There are several potential representations for metatypes within
/// SIL, which are distinguished by the metatype representation. This
/// enumeration captures the different representations. Some
/// conversions between representations are possible: for example, one
/// can convert a thin representation to a thick one (but not
/// vice-versa), and different representations are required in
/// different places.
enum class MetatypeRepresentation : char {
  /// A thin metatype requires no runtime information, because the
  /// type itself provides no dynamic behavior.
  ///
  /// Struct and enum metatypes are thin, because dispatch to static
  /// struct and enum members is completely static.
  Thin,
  /// A thick metatype refers to a complete metatype representation
  /// that allows introspection and dynamic dispatch. 
  ///
  /// Thick metatypes are used for class and existential metatypes,
  /// which permit dynamic behavior.
  Thick,
  /// An Objective-C metatype refers to an Objective-C class object.
  ObjC
};

/// AnyMetatypeType - A common parent class of MetatypeType and
/// ExistentialMetatypeType.
class AnyMetatypeType : public TypeBase {
  Type InstanceType;
protected:
  AnyMetatypeType(TypeKind kind, const ASTContext *C,
                  RecursiveTypeProperties properties,
                  Type instanceType,
                  Optional<MetatypeRepresentation> repr);


public:
  Type getInstanceType() const { return InstanceType; }

  /// Does this metatype have a representation?
  ///
  /// Only SIL metatype types have a representation.
  bool hasRepresentation() const {
    return AnyMetatypeTypeBits.Representation > 0;
  }
  
  /// Retrieve the metatype representation.
  ///
  /// The metatype representation is a SIL-only property. Thin
  /// metatypes can be lowered away to empty types in IR, unless a
  /// metatype value is required at an abstraction level.
  MetatypeRepresentation getRepresentation() const {
    assert(AnyMetatypeTypeBits.Representation &&
           "metatype has no representation");
    return static_cast<MetatypeRepresentation>(
             AnyMetatypeTypeBits.Representation - 1);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Metatype ||
           T->getKind() == TypeKind::ExistentialMetatype;
  }
};
BEGIN_CAN_TYPE_WRAPPER(AnyMetatypeType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getInstanceType)
END_CAN_TYPE_WRAPPER(AnyMetatypeType, Type)

/// MetatypeType - This is the type given to a metatype value.  When a type is
/// declared, a 'metatype' value is injected into the value namespace to
/// resolve references to the type.  An example:
///
///  struct x { ... }  // declares type 'x' and metatype 'x'.
///  x.a()             // use of the metatype value since its a value context.
///
/// In general, this is spelled X.Type, unless X is an existential
/// type, in which case the ordinary metatype is spelled X.Protocol
/// and X.Type connotes the ExistentialMetatypeType.
class MetatypeType : public AnyMetatypeType {
  static MetatypeType *get(Type T, Optional<MetatypeRepresentation> Repr,
                           const ASTContext &C);

public:
  /// \brief Return the MetatypeType for the specified type declaration.
  ///
  /// This leaves the 'representation' property unavailable.
  static MetatypeType *get(Type T, const ASTContext &C) {
    return get(T, None, C);
  }
  
  /// Return the MetatypeType for the specified type declaration with
  /// the given representation.
  ///
  /// Metatype representation is a SIL-only property. Thin metatypes
  /// can be lowered away to empty types in IR.
  static MetatypeType *get(Type T,
                           Optional<MetatypeRepresentation> repr = None) {
    return get(T, repr, T->getASTContext());
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Metatype;
  }
  
private:
  MetatypeType(Type T, const ASTContext *C,
               RecursiveTypeProperties properties,
               Optional<MetatypeRepresentation> repr);
  friend class TypeDecl;
};
BEGIN_CAN_TYPE_WRAPPER(MetatypeType, AnyMetatypeType)
  static CanMetatypeType get(CanType type) {
    return CanMetatypeType(MetatypeType::get(type));
  }
  static CanMetatypeType get(CanType type, MetatypeRepresentation repr) {
    return CanMetatypeType(MetatypeType::get(type, repr));
  }
END_CAN_TYPE_WRAPPER(MetatypeType, AnyMetatypeType)

/// ExistentialMetatypeType - This is the type given to an existential
/// metatype value, i.e. the type of the dynamic type of an
/// existential value.  The instance type must be an existential type
/// of some sort.
///
/// Formally, this type is \exists t : T... . t.Type.  In contrast,
/// the MetatypeType for a ProtocolType is a singleton.
///
/// This is spelled X.Type, where X is an existential type.
///
/// The representation of an existential metatype cannot be thin.
class ExistentialMetatypeType : public AnyMetatypeType {
public:
  static ExistentialMetatypeType *get(Type T,
                                      Optional<MetatypeRepresentation> Repr,
                                      const ASTContext &C);

  /// Return the ExistentialMetatypeType for the specified type
  /// with the given representation.
  ///
  /// Metatype representation is a SIL-only property. Existential
  /// metatypes cannot be thin.
  static ExistentialMetatypeType *get(Type T,
                                      Optional<MetatypeRepresentation> repr
                                        = None) {
    return get(T, repr, T->getASTContext());
  }

  /// Return the canonicalized list of protocols.
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protos) {
    getInstanceType()->getAnyExistentialTypeProtocols(protos);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ExistentialMetatype;
  }
  
private:
  ExistentialMetatypeType(Type T, const ASTContext *C,
                          RecursiveTypeProperties properties,
                          Optional<MetatypeRepresentation> repr);
  friend class TypeDecl;
};
BEGIN_CAN_TYPE_WRAPPER(ExistentialMetatypeType, AnyMetatypeType)
  static CanExistentialMetatypeType get(CanType type) {
    return CanExistentialMetatypeType(ExistentialMetatypeType::get(type));
  }
  static CanExistentialMetatypeType get(CanType type,
                                        MetatypeRepresentation repr) {
    return CanExistentialMetatypeType(ExistentialMetatypeType::get(type, repr));
  }
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protocols) {
    getInstanceType().getAnyExistentialTypeProtocols(protocols);
  }
END_CAN_TYPE_WRAPPER(ExistentialMetatypeType, AnyMetatypeType)
  
/// ModuleType - This is the type given to a module value, e.g. the "Builtin" in
/// "Builtin.int".  This is typically given to a ModuleExpr, but can also exist
/// on ParenExpr, for example.
class ModuleType : public TypeBase {
  ModuleDecl *const TheModule;
  
public:
  /// get - Return the ModuleType for the specified module.
  static ModuleType *get(ModuleDecl *M);

  ModuleDecl *getModule() const { return TheModule; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Module;
  }
  
private:
  ModuleType(ModuleDecl *M, const ASTContext &Ctx)
    : TypeBase(TypeKind::Module, &Ctx, // Always canonical
               RecursiveTypeProperties()),
      TheModule(M) {
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ModuleType, Type)
  
/// The type given to a dynamic \c Self return type.
///
/// Example:
/// \code
/// class X {
///   class func factory() -> Self { ... }
/// };
/// \endcode
///
/// In this example, \c Self is represented by a 
/// \c DynamicSelfType node whose self type is \c X.
class DynamicSelfType : public TypeBase {
  Type SelfType;

public:
  /// \brief Return the DynamicSelf for the specified self type.
  static DynamicSelfType *get(Type selfType, const ASTContext &ctx);

  /// Retrieve the (static) self type for this dynamic self type.
  Type getSelfType() const { return SelfType; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::DynamicSelf;
  }
  
private:
  DynamicSelfType(Type selfType, const ASTContext &ctx,
                  RecursiveTypeProperties properties)
    : TypeBase(TypeKind::DynamicSelf, selfType->isCanonical()? &ctx : 0,
               properties | RecursiveTypeProperties(
                 RecursiveTypeProperties::HasDynamicSelf)),
      SelfType(selfType) { }

  friend class TypeDecl;
};
BEGIN_CAN_TYPE_WRAPPER(DynamicSelfType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getSelfType)

  static CanDynamicSelfType get(CanType selfType, const ASTContext &ctx) {
    return CanDynamicSelfType(DynamicSelfType::get(selfType, ctx));
  }
END_CAN_TYPE_WRAPPER(DynamicSelfType, Type)

/// A language-level calling convention.
enum class SILFunctionLanguage : unsigned char {
  /// A variation of the Swift calling convention.
  Swift = 0,

  /// A variation of the C calling convention.
  C,
};

/// The representation form of a function.
enum class FunctionTypeRepresentation : uint8_t {
  /// A "thick" function that carries a context pointer to reference captured
  /// state. The default native function representation.
  Swift = 0,
  
  /// A thick function that is represented as an Objective-C block.
  Block,
  
  /// A "thin" function that needs no context.
  Thin,
  
  /// A C function pointer, which is thin and also uses the C calling
  /// convention.
  CFunctionPointer,
  
  /// The value of the greatest AST function representation.
  Last = CFunctionPointer,
};

/// The representation form of a SIL function.
///
/// This is a superset of FunctionTypeRepresentation. The common representations
/// must share an enum value.
///
/// TODO: The overlap of SILFunctionTypeRepresentation and
/// FunctionTypeRepresentation is a total hack necessitated by the way SIL
/// TypeLowering is currently written. We ought to refactor TypeLowering so that
/// it is not necessary to distinguish these cases.
enum class SILFunctionTypeRepresentation : uint8_t {
  /// A freestanding thick function.
  Thick = uint8_t(FunctionTypeRepresentation::Swift),
  
  /// A thick function that is represented as an Objective-C block.
  Block = uint8_t(FunctionTypeRepresentation::Block),
  
  /// A freestanding thin function that needs no context.
  Thin = uint8_t(FunctionTypeRepresentation::Thin),
  
  /// A C function pointer, which is thin and also uses the C calling
  /// convention.
  CFunctionPointer = uint8_t(FunctionTypeRepresentation::CFunctionPointer),
  
  /// The value of the greatest AST function representation.
  LastAST = CFunctionPointer,

  /// The value of the least SIL-only function representation.
  FirstSIL = 8,
  
  /// A Swift instance method.
  Method = FirstSIL,
  
  /// An Objective-C method.
  ObjCMethod,
  
  /// A Swift protocol witness.
  WitnessMethod,
  
  /// A closure invocation function that has not been bound to a context.
  Closure,
};

/// Can this calling convention result in a function being called indirectly
/// through the runtime.
inline bool canBeCalledIndirectly(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
  case SILFunctionTypeRepresentation::Closure:
    return false;
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    return true;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

/// Map a SIL function representation to the base language calling convention
/// it uses.
inline SILFunctionLanguage
getSILFunctionLanguage(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    return SILFunctionLanguage::C;
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
  case SILFunctionTypeRepresentation::Closure:
    return SILFunctionLanguage::Swift;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

/// AnyFunctionType - A function type has a single input and result, but
/// these types may be tuples, for example:
///   "(int) -> int" or "(a : int, b : int) -> (int, int)".
/// Note that the parser requires that the input to a function type be a Tuple
/// or ParenType, but ParenType desugars to its element, so the input to a
/// function may be an arbitrary type.
///
/// There are two kinds of function types:  monomorphic (FunctionType) and
/// polymorphic (GenericFunctionType). Both type families additionally can
/// be 'thin', indicating that a function value has no capture context and can be
/// represented at the binary level as a single function pointer.
class AnyFunctionType : public TypeBase {
  const Type Input;
  const Type Output;

public:
  using Representation = FunctionTypeRepresentation;
  
  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // Feel free to rearrange or add bits, but if you go over 15,
    // you'll need to adjust both the Bits field below and
    // BaseType::AnyFunctionTypeBits.

    //   |representation|isAutoClosure|noEscape|throws|
    //   |    0 .. 3    |      4      |    5   |   6  |
    //
    enum : uint16_t { RepresentationMask     = 0x00F };
    enum : uint16_t { AutoClosureMask        = 0x010 };
    enum : uint16_t { NoEscapeMask           = 0x020 };
    enum : uint16_t { ThrowsMask             = 0x040 };

    uint16_t Bits;

    ExtInfo(unsigned Bits) : Bits(static_cast<uint16_t>(Bits)) {}

    friend class AnyFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) {
      assert(getRepresentation() == Representation::Swift);
    }

    // Constructor for polymorphic type.
    ExtInfo(Representation Rep, bool Throws) {
      Bits = ((unsigned) Rep) | (Throws ? ThrowsMask : 0);
    }

    // Constructor with no defaults.
    ExtInfo(Representation Rep,
            bool IsAutoClosure, bool IsNoEscape,
            bool Throws)
      : ExtInfo(Rep, Throws) {
      Bits |= (IsAutoClosure ? AutoClosureMask : 0);
      Bits |= (IsNoEscape ? NoEscapeMask : 0);
    }

    bool isAutoClosure() const { return Bits & AutoClosureMask; }
    bool isNoEscape() const { return Bits & NoEscapeMask; }
    bool throws() const { return Bits & ThrowsMask; }
    Representation getRepresentation() const {
      unsigned rawRep = Bits & RepresentationMask;
      assert(rawRep <= unsigned(Representation::Last)
             && "unexpected SIL representation");
      return Representation(rawRep);
    }

    bool hasSelfParam() const {
      switch (getSILRepresentation()) {
      case SILFunctionTypeRepresentation::Thick:
      case SILFunctionTypeRepresentation::Block:
      case SILFunctionTypeRepresentation::Thin:
      case SILFunctionTypeRepresentation::CFunctionPointer:
      case SILFunctionTypeRepresentation::Closure:
        return false;
      case SILFunctionTypeRepresentation::ObjCMethod:
      case SILFunctionTypeRepresentation::Method:
      case SILFunctionTypeRepresentation::WitnessMethod:
        return true;
      }

      llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
    }

    /// True if the function representation carries context.
    bool hasContext() const {
      switch (getSILRepresentation()) {
      case SILFunctionTypeRepresentation::Thick:
      case SILFunctionTypeRepresentation::Block:
        return true;
      case SILFunctionTypeRepresentation::Thin:
      case SILFunctionTypeRepresentation::Method:
      case SILFunctionTypeRepresentation::ObjCMethod:
      case SILFunctionTypeRepresentation::WitnessMethod:
      case SILFunctionTypeRepresentation::CFunctionPointer:
      case SILFunctionTypeRepresentation::Closure:
        return false;
      }

      llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
    }
    
    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    ExtInfo withRepresentation(Representation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    ExtInfo withIsAutoClosure(bool IsAutoClosure = true) const {
      if (IsAutoClosure)
        return ExtInfo(Bits | AutoClosureMask);
      else
        return ExtInfo(Bits & ~AutoClosureMask);
    }
    ExtInfo withNoEscape(bool NoEscape = true) const {
      if (NoEscape)
        return ExtInfo(Bits | NoEscapeMask);
      else
        return ExtInfo(Bits & ~NoEscapeMask);
    }
    ExtInfo withThrows(bool Throws = true) const {
      if (Throws)
        return ExtInfo(Bits | ThrowsMask);
      else
        return ExtInfo(Bits & ~ThrowsMask);
    }

    uint16_t getFuncAttrKey() const {
      return Bits;
    }
    
    /// Put a SIL representation in the ExtInfo.
    ///
    /// SIL type lowering transiently generates AST function types with SIL
    /// representations. However, they shouldn't persist in the AST, and
    /// don't need to be parsed, printed, or serialized.
    ExtInfo withSILRepresentation(SILFunctionTypeRepresentation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    
    SILFunctionTypeRepresentation getSILRepresentation() const {
      unsigned rawRep = Bits & RepresentationMask;
      return SILFunctionTypeRepresentation(rawRep);
    }

    bool operator==(ExtInfo Other) const {
      return Bits == Other.Bits;
    }
    bool operator!=(ExtInfo Other) const {
      return Bits != Other.Bits;
    }
  };

protected:
  AnyFunctionType(TypeKind Kind, const ASTContext *CanTypeContext,
                  Type Input, Type Output, RecursiveTypeProperties properties,
                  const ExtInfo &Info)
  : TypeBase(Kind, CanTypeContext, properties), Input(Input), Output(Output) {
    AnyFunctionTypeBits.ExtInfo = Info.Bits;
  }

public:

  Type getInput() const { return Input; }
  Type getResult() const { return Output; }

  ExtInfo getExtInfo() const {
    return ExtInfo(AnyFunctionTypeBits.ExtInfo);
  }

  /// \brief Get the representation of the function type.
  Representation getRepresentation() const {
    return getExtInfo().getRepresentation();
  }
  
  /// \brief True if this type allows an implicit conversion from a function
  /// argument expression of type T to a function of type () -> T.
  bool isAutoClosure() const {
    return getExtInfo().isAutoClosure();
  }

  /// \brief True if the parameter declaration it is attached to is guaranteed
  /// to not persist the closure for longer than the duration of the call.
  bool isNoEscape() const {
    return getExtInfo().isNoEscape();
  }

  bool throws() const {
    return getExtInfo().throws();
  }

  /// Returns a new function type exactly like this one but with the ExtInfo
  /// replaced.
  AnyFunctionType *withExtInfo(ExtInfo info) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_AnyFunctionType &&
           T->getKind() <= TypeKind::Last_AnyFunctionType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(AnyFunctionType, Type)
  typedef AnyFunctionType::ExtInfo ExtInfo;
  PROXY_CAN_TYPE_SIMPLE_GETTER(getInput)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getResult)
  
  CanAnyFunctionType withExtInfo(ExtInfo info) const {
    return CanAnyFunctionType(getPointer()->withExtInfo(info));
  }
END_CAN_TYPE_WRAPPER(AnyFunctionType, Type)

/// FunctionType - A monomorphic function type, specified with an arrow.
///
/// For example:
///   let x : (Float, Int) -> Int
class FunctionType : public AnyFunctionType {
public:
  /// 'Constructor' Factory Function
  static FunctionType *get(Type Input, Type Result) {
    return get(Input, Result, ExtInfo());
  }

  static FunctionType *get(Type Input, Type Result, const ExtInfo &Info);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Function;
  }
  
private:
  FunctionType(Type Input, Type Result,
               RecursiveTypeProperties properties,
               const ExtInfo &Info);
};
BEGIN_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)
  static CanFunctionType get(CanType input, CanType result) {
    return CanFunctionType(FunctionType::get(input, result));
  }
  static CanFunctionType get(CanType input, CanType result,
                             const ExtInfo &info) {
    return CanFunctionType(FunctionType::get(input, result, info));
  }
  
  CanFunctionType withExtInfo(ExtInfo info) const {
    return CanFunctionType(cast<FunctionType>(getPointer()->withExtInfo(info)));
  }
END_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)

/// A call argument or parameter.
struct CallArgParam {
  /// The type of the argument or parameter. For a variadic parameter,
  /// this is the element type.
  Type Ty;

  // The label associated with the argument or parameter, if any.
  Identifier Label;

  /// Whether the parameter has a default argument.  Not valid for arguments.
  bool HasDefaultArgument = false;

  /// Parameter specific flags, not valid for arguments
  ParameterTypeFlags parameterFlags = {};

  /// Whether the argument or parameter has a label.
  bool hasLabel() const { return !Label.empty(); }

  /// Whether the parameter is varargs
  bool isVariadic() const { return parameterFlags.isVariadic(); }

  /// Whether the parameter is autoclosure
  bool isAutoClosure() const { return parameterFlags.isAutoClosure(); }

  /// Whether the parameter is escaping
  bool isEscaping() const { return parameterFlags.isEscaping(); }
};

/// Break an argument type into an array of \c CallArgParams.
///
/// \param type The type to decompose.
/// \param argumentLabels The argument labels to use.
SmallVector<CallArgParam, 4>
decomposeArgType(Type type, ArrayRef<Identifier> argumentLabels);

/// Break a parameter type into an array of \c CallArgParams.
///
/// \param paramOwner The declaration that owns this parameter.
/// \param level The level of parameters that are being decomposed.
SmallVector<CallArgParam, 4>
decomposeParamType(Type type, const ValueDecl *paramOwner, unsigned level);

/// Turn a param list into a symbolic and printable representation that does not
/// include the types, something like (: , b:, c:)
std::string getParamListAsString(ArrayRef<CallArgParam> parameters);

/// Describes a generic function type.
///
/// A generic function type describes a function that is polymorphic with
/// respect to some set of generic parameters and the requirements placed
/// on those parameters and dependent member types thereof. The input and
/// output types of the generic function can be expressed in terms of those
/// generic parameters.
class GenericFunctionType : public AnyFunctionType,
                            public llvm::FoldingSetNode
{
  GenericSignature *Signature;

  /// Construct a new generic function type.
  GenericFunctionType(GenericSignature *sig,
                      Type input,
                      Type result,
                      const ExtInfo &info,
                      const ASTContext *ctx,
                      RecursiveTypeProperties properties);
public:
  /// Create a new generic function type.
  static GenericFunctionType *get(GenericSignature *sig,
                                  Type input,
                                  Type result,
                                  const ExtInfo &info);

  /// Retrieve the generic signature of this function type.
  GenericSignature *getGenericSignature() const {
    return Signature;
  }
  
  /// Retrieve the generic parameters of this polymorphic function type.
  ArrayRef<GenericTypeParamType *> getGenericParams() const;

  /// Retrieve the requirements of this polymorphic function type.
  ArrayRef<Requirement> getRequirements() const;
                              
  /// Substitute the given generic arguments into this generic
  /// function type and return the resulting non-generic type.
  FunctionType *substGenericArgs(SubstitutionList subs);

  /// Substitute the given generic arguments into this generic
  /// function type and return the resulting non-generic type.
  FunctionType *substGenericArgs(const SubstitutionMap &subs);

  /// Substitute the given generic arguments into this generic
  /// function type using the given substitution and conformance lookup
  /// callbacks.
  FunctionType *substGenericArgs(TypeSubstitutionFn subs,
                                 LookupConformanceFn conformances);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericSignature(), getInput(), getResult(),
            getExtInfo());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericSignature *sig,
                      Type input,
                      Type result,
                      const ExtInfo &info);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::GenericFunction;
  }
};

BEGIN_CAN_TYPE_WRAPPER(GenericFunctionType, AnyFunctionType)
  static CanGenericFunctionType get(CanGenericSignature sig,
                                    CanType input, CanType result,
                                    const ExtInfo &info) {
    // Knowing that the argument types are independently canonical is
    // not sufficient to guarantee that the function type will be canonical.
    auto fnType = GenericFunctionType::get(sig, input, result, info);
    return cast<GenericFunctionType>(fnType->getCanonicalType());
  }
  
  CanGenericSignature getGenericSignature() const {
    return CanGenericSignature(getPointer()->getGenericSignature());
  }
  
  ArrayRef<CanTypeWrapper<GenericTypeParamType>> getGenericParams() const {
    return getGenericSignature().getGenericParams();
  }

  CanGenericFunctionType withExtInfo(ExtInfo info) const {
    return CanGenericFunctionType(
                    cast<GenericFunctionType>(getPointer()->withExtInfo(info)));
  }
END_CAN_TYPE_WRAPPER(GenericFunctionType, AnyFunctionType)

/// Conventions for passing arguments as parameters.
enum class ParameterConvention {
  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory.  The callee is responsible for destroying the
  /// object.  The callee may assume that the address does not alias any valid
  /// object.
  Indirect_In,

  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory.  The callee may not modify and does not destroy
  /// the object.
  Indirect_In_Guaranteed,

  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory.  The object is always valid, but the callee may
  /// assume that the address does not alias any valid object and reorder loads
  /// stores to the parameter as long as the whole object remains valid. Invalid
  /// single-threaded aliasing may produce inconsistent results, but should
  /// remain memory safe.
  Indirect_Inout,
  
  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory. The object is allowed to be aliased by other
  /// well-typed references, but is not allowed to be escaped. This is the
  /// convention used by mutable captures in @noescape closures.
  Indirect_InoutAliasable,

  /// This argument is passed directly.  Its type is non-trivial, and the callee
  /// is responsible for destroying it.
  Direct_Owned,

  /// This argument is passed directly.  Its type may be trivial, or it may
  /// simply be that the callee is not responsible for destroying it.  Its
  /// validity is guaranteed only at the instant the call begins.
  Direct_Unowned,

  /// This argument is passed directly.  Its type is non-trivial, and the caller
  /// guarantees its validity for the entirety of the call.
  Direct_Guaranteed,
};
// Check that the enum values fit inside SILFunctionTypeBits.
static_assert(unsigned(ParameterConvention::Direct_Guaranteed) < (1<<3),
              "fits in SILFunctionTypeBits");

// Does this parameter convention require indirect storage? This reflects a
// SILFunctionType's formal (immutable) conventions, as opposed to the transient
// SIL conventions that dictate SILValue types.
inline bool isIndirectFormalParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
  case ParameterConvention::Indirect_In_Guaranteed:
    return true;

  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
    return false;
  }
  llvm_unreachable("covered switch isn't covered?!");
}
inline bool isConsumedParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Direct_Owned:
    return true;

  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Indirect_In_Guaranteed:
    return false;
  }
  llvm_unreachable("bad convention kind");
}

/// Returns true if conv is a guaranteed parameter. This may look unnecessary
/// but this will allow code to generalize to handle Indirect_Guaranteed
/// parameters when they are added.
inline bool isGuaranteedParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Indirect_In_Guaranteed:
    return true;

  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_InoutAliasable:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Owned:
    return false;
  }
  llvm_unreachable("bad convention kind");
}

/// A parameter type and the rules for passing it.
class SILParameterInfo {
  CanType Ty;
  ParameterConvention Convention;
public:
  SILParameterInfo() : Ty(), Convention((ParameterConvention)0) {}
  SILParameterInfo(CanType type, ParameterConvention conv)
    : Ty(type), Convention(conv) {
    assert(type->isLegalSILType() && "SILParameterInfo has illegal SIL type");
  }

  CanType getType() const {
    return Ty;
  }
  ParameterConvention getConvention() const {
    return Convention;
  }
  // Does this parameter convention require indirect storage? This reflects a
  // SILFunctionType's formal (immutable) conventions, as opposed to the
  // transient SIL conventions that dictate SILValue types.
  bool isFormalIndirect() const {
    return isIndirectFormalParameter(getConvention());
  }

  bool isIndirectInGuaranteed() const {
    return getConvention() == ParameterConvention::Indirect_In_Guaranteed;
  }

  bool isIndirectInOut() const {
    return getConvention() == ParameterConvention::Indirect_Inout;
  }
  bool isIndirectMutating() const {
    return getConvention() == ParameterConvention::Indirect_Inout
        || getConvention() == ParameterConvention::Indirect_InoutAliasable;
  }

  /// True if this parameter is consumed by the callee, either
  /// indirectly or directly.
  bool isConsumed() const {
    return isConsumedParameter(getConvention());
  }

  /// Returns true if this parameter is guaranteed, either indirectly or
  /// directly.
  bool isGuaranteed() const {
    return isGuaranteedParameter(getConvention());
  }

  /// The SIL storage type determines the ABI for arguments based purely on the
  /// formal parameter conventions. The actual SIL type for the argument values
  /// may differ in canonical SIL. In particular, opaque values require indirect
  /// storage. Therefore they will be passed using an indirect formal
  /// convention, and this method will return an address type. However, in
  /// canonical SIL the opaque arguments might not have an address type.
  SILType getSILStorageType() const; // in SILFunctionConventions.h

  /// Return a version of this parameter info with the type replaced.
  SILParameterInfo getWithType(CanType type) const {
    return SILParameterInfo(type, getConvention());
  }

  /// Transform this SILParameterInfo by applying the user-provided
  /// function to its type.
  ///
  /// Note that this does not perform a recursive transformation like
  /// Type::transform does.
  template<typename F>
  SILParameterInfo map(const F &fn) const {
    return getWithType(fn(getType()));
  }

  void profile(llvm::FoldingSetNodeID &id) {
    id.AddPointer(Ty.getPointer());
    id.AddInteger((unsigned)Convention);
  }

  void dump() const;
  void print(llvm::raw_ostream &out,
             const PrintOptions &options = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;
  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       SILParameterInfo type) {
    type.print(out);
    return out;
  }

  bool operator==(SILParameterInfo rhs) const {
    return Ty == rhs.Ty && Convention == rhs.Convention;
  }
  bool operator!=(SILParameterInfo rhs) const {
    return !(*this == rhs);
  }
};

/// Conventions for returning values.
enum class ResultConvention {
  /// This result is returned indirectly, i.e. by passing the address
  /// of an uninitialized object in memory.  The callee is responsible
  /// for leaving an initialized object at this address.  The callee
  /// may assume that the address does not alias any valid object.
  Indirect,

  /// The caller is responsible for destroying this return value.
  /// Its type is non-trivial.
  Owned,

  /// The caller is not responsible for destroying this return value.
  /// Its type may be trivial, or it may simply be offered unsafely.
  /// It is valid at the instant of the return, but further operations
  /// may invalidate it.
  Unowned,

  /// The caller is not responsible for destroying this return value.
  /// The validity of the return value is dependent on the 'self' parameter,
  /// so it may be invalidated if that parameter is released.
  UnownedInnerPointer,
  
  /// This value has been (or may have been) returned autoreleased.
  /// The caller should make an effort to reclaim the autorelease.
  /// The type must be a class or class existential type, and this
  /// must be the only return value.
  Autoreleased,
};

// Does this result require indirect storage for the purpose of reabstraction?
inline bool isIndirectFormalResult(ResultConvention convention) {
  return convention == ResultConvention::Indirect;
}

/// A result type and the rules for returning it.
class SILResultInfo {
  llvm::PointerIntPair<CanType, 3, ResultConvention> TypeAndConvention;
public:
  SILResultInfo() = default;
  SILResultInfo(CanType type, ResultConvention conv)
    : TypeAndConvention(type, conv) {
    assert(type->isLegalSILType() && "SILResultInfo has illegal SIL type");
  }

  CanType getType() const {
    return TypeAndConvention.getPointer();
  }
  ResultConvention getConvention() const {
    return TypeAndConvention.getInt();
  }
  /// The SIL storage type determines the ABI for arguments based purely on the
  /// formal result conventions. The actual SIL type for the result values may
  /// differ in canonical SIL. In particular, opaque values require indirect
  /// storage. Therefore they will be returned using an indirect formal
  /// convention, and this method will return an address type. However, in
  /// canonical SIL the opaque results might not have an address type.
  SILType getSILStorageType() const; // in SILFunctionConventions.h

  /// Return a version of this result info with the type replaced.
  SILResultInfo getWithType(CanType type) const {
    return SILResultInfo(type, getConvention());
  }

  // Does this result convention require indirect storage? This reflects a
  // SILFunctionType's formal (immutable) conventions, as opposed to the
  // transient SIL conventions that dictate SILValue types.
  bool isFormalIndirect() const {
    return isIndirectFormalResult(getConvention());
  }
  bool isFormalDirect() const {
    return !isIndirectFormalResult(getConvention());
  }

  /// Transform this SILResultInfo by applying the user-provided
  /// function to its type.
  ///
  /// Note that this does not perform a recursive transformation like
  /// Type::transform does.
  template <typename F>
  SILResultInfo map(F &&fn) const {
    return getWithType(fn(getType()));
  }

  void profile(llvm::FoldingSetNodeID &id) {
    id.AddPointer(TypeAndConvention.getOpaqueValue());
  }

  void dump() const;
  void print(llvm::raw_ostream &out,
             const PrintOptions &options = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &Options) const;
  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       SILResultInfo type) {
    type.print(out);
    return out;
  }

  ValueOwnershipKind
  getOwnershipKind(SILModule &,
                   CanGenericSignature sig) const; // in SILType.cpp

  bool operator==(SILResultInfo rhs) const {
    return TypeAndConvention == rhs.TypeAndConvention;
  }
  bool operator!=(SILResultInfo rhs) const {
    return !(*this == rhs);
  }
};
  
class SILFunctionType;
typedef CanTypeWrapper<SILFunctionType> CanSILFunctionType;
class SILFunctionConventions;

/// SILFunctionType - The lowered type of a function value, suitable
/// for use by SIL.
///
/// This type is defined by the AST library because it must be capable
/// of appearing in secondary positions, e.g. within tuple and
/// function parameter and result types.
class SILFunctionType final : public TypeBase, public llvm::FoldingSetNode,
    private llvm::TrailingObjects<SILFunctionType, SILParameterInfo,
                                  SILResultInfo> {
  friend TrailingObjects;

  size_t numTrailingObjects(OverloadToken<SILParameterInfo>) const {
    return NumParameters;
  }

  size_t numTrailingObjects(OverloadToken<SILResultInfo>) const {
    return hasErrorResult() ? 1 : 0;
  }

public:
  using Language = SILFunctionLanguage;
  using Representation = SILFunctionTypeRepresentation;

  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // Feel free to rearrange or add bits, but if you go over 15,
    // you'll need to adjust both the Bits field below and
    // TypeBase::AnyFunctionTypeBits.

    //   |representation|pseudogeneric|
    //   |    0 .. 3    |      4      |
    //
    enum : uint16_t { RepresentationMask = 0x00F };
    enum : uint16_t { PseudogenericMask  = 0x010 };

    uint16_t Bits;

    ExtInfo(unsigned Bits) : Bits(static_cast<uint16_t>(Bits)) {}

    friend class SILFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) { }

    // Constructor for polymorphic type.
    ExtInfo(Representation rep, bool isPseudogeneric) {
      Bits = ((unsigned) rep) |
             (isPseudogeneric ? PseudogenericMask : 0);
    }

    /// Is this function pseudo-generic?  A pseudo-generic function
    /// is not permitted to dynamically depend on its type arguments.
    bool isPseudogeneric() const { return Bits & PseudogenericMask; }

    /// What is the abstract representation of this function value?
    Representation getRepresentation() const {
      return Representation(Bits & RepresentationMask);
    }
    Language getLanguage() const {
      return getSILFunctionLanguage(getRepresentation());
    }

    bool hasSelfParam() const {
      switch (getRepresentation()) {
      case Representation::Thick:
      case Representation::Block:
      case Representation::Thin:
      case Representation::CFunctionPointer:
      case Representation::Closure:
        return false;
      case Representation::ObjCMethod:
      case Representation::Method:
      case Representation::WitnessMethod:
        return true;
      }

      llvm_unreachable("Unhandled Representation in switch.");
    }

    bool hasGuaranteedSelfParam() const {
      switch (getRepresentation()) {
      case Representation::Thick:
      case Representation::Block:
      case Representation::Thin:
      case Representation::CFunctionPointer:
      case Representation::ObjCMethod:
      case Representation::Closure:
        return false;
      case Representation::Method:
      case Representation::WitnessMethod:
        return true;
      }

      llvm_unreachable("Unhandled Representation in switch.");
    }

    /// True if the function representation carries context.
    bool hasContext() const {
      switch (getRepresentation()) {
      case Representation::Thick:
      case Representation::Block:
        return true;
      case Representation::Thin:
      case Representation::CFunctionPointer:
      case Representation::ObjCMethod:
      case Representation::Method:
      case Representation::WitnessMethod:
      case Representation::Closure:
        return false;
      }

      llvm_unreachable("Unhandled Representation in switch.");
    }
    
    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    ExtInfo withRepresentation(Representation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    ExtInfo withIsPseudogeneric(bool isPseudogeneric = true) const {
      if (isPseudogeneric)
        return ExtInfo(Bits | PseudogenericMask);
      else
        return ExtInfo(Bits & ~PseudogenericMask);
    }

    uint16_t getFuncAttrKey() const {
      return Bits;
    }

    bool operator==(ExtInfo Other) const {
      return Bits == Other.Bits;
    }
    bool operator!=(ExtInfo Other) const {
      return Bits != Other.Bits;
    }
  };

private:
  unsigned NumParameters;
  unsigned NumResults : 16;               // Not including the ErrorResult.
  unsigned NumIndirectFormalResults : 16; // Subset of NumResults.

  // The layout of a SILFunctionType in memory is:
  //   SILFunctionType
  //   SILParameterInfo[NumParameters]
  //   SILResultInfo[NumResults]
  //   SILResultInfo?                 // if hasErrorResult()
  //   CanType?                       // if NumResults > 1, formal result cache
  //   CanType?                       // if NumResults > 1, all result cache

  CanGenericSignature GenericSig;

  MutableArrayRef<SILParameterInfo> getMutableParameters() {
    return {getTrailingObjects<SILParameterInfo>(), NumParameters};
  }

  MutableArrayRef<SILResultInfo> getMutableResults() {
    auto *ptr = reinterpret_cast<SILResultInfo *>(getMutableParameters().end());
    return MutableArrayRef<SILResultInfo>(ptr, getNumResults());
  }

  SILResultInfo *getEndOfNormalResults() { return getMutableResults().end(); }

  SILResultInfo &getMutableErrorResult() {
    assert(hasErrorResult());
    return *getEndOfNormalResults();
  }

  bool hasResultCache() const { return NumResults > 1; }

  CanType &getMutableFormalResultsCache() const {
    assert(hasResultCache());
    auto *ptr = const_cast<SILFunctionType *>(this)->getEndOfNormalResults()
                + size_t(hasErrorResult());
    return *reinterpret_cast<CanType*>(ptr);
  }

  CanType &getMutableAllResultsCache() const {
    assert(hasResultCache());
    auto *ptr = const_cast<SILFunctionType *>(this)->getEndOfNormalResults()
                + size_t(hasErrorResult());
    return *(reinterpret_cast<CanType *>(ptr) + 1);
  }

  SILFunctionType(GenericSignature *genericSig, ExtInfo ext,
                  ParameterConvention calleeConvention,
                  ArrayRef<SILParameterInfo> params,
                  ArrayRef<SILResultInfo> normalResults,
                  Optional<SILResultInfo> errorResult, const ASTContext &ctx,
                  RecursiveTypeProperties properties);

public:
  static CanSILFunctionType get(GenericSignature *genericSig,
                                ExtInfo ext,
                                ParameterConvention calleeConvention,
                                ArrayRef<SILParameterInfo> interfaceParams,
                                ArrayRef<SILResultInfo> interfaceResults,
                                Optional<SILResultInfo> interfaceErrorResult,
                                const ASTContext &ctx);

  /// Given that this function type uses a C-language convention, return its
  /// formal semantic result type.
  ///
  /// C functions represented in SIL are always in one of three cases:
  ///   - no results at all; this corresponds to a void result type;
  ///   - a single direct result and no indirect results; or
  ///   - a single indirect result and no direct results.
  ///
  /// If the result is formally indirect, return the empty tuple.
  SILType getFormalCSemanticResult();

  /// Return the convention under which the callee is passed, if this
  /// is a thick non-block callee.
  ParameterConvention getCalleeConvention() const {
    return ParameterConvention(SILFunctionTypeBits.CalleeConvention);
  }
  bool isCalleeConsumed() const {
    return getCalleeConvention() == ParameterConvention::Direct_Owned;
  }

  /// Return the array of all result information. This may contain inter-mingled
  /// direct and indirect results.
  ArrayRef<SILResultInfo> getResults() const {
    return const_cast<SILFunctionType *>(this)->getMutableResults();
  }
  unsigned getNumResults() const { return NumResults; }

  /// Given that this function type has exactly one result, return it.
  /// This is a common situation when working with a function with a known
  /// signature.  It is *not* safe to assume that C functions satisfy
  /// this, because void functions have zero results.
  SILResultInfo getSingleResult() const {
    assert(getNumResults() == 1);
    return getResults()[0];
  }

  /// Given that this function type has exactly one formally direct result,
  /// return it. Some formal calling conventions only apply when a single
  /// direct result is present.
  SILResultInfo getSingleDirectFormalResult() const {
    assert(getNumDirectFormalResults() == 1);
    for (auto &result : getResults()) {
      if (!result.isFormalIndirect())
        return result;
    }
    llvm_unreachable("Missing indirect result");
  }

  // Get the number of results that require a formal indirect calling
  // convention regardless of whether SIL requires address types. Even if the
  // substituted SIL types match, a formal direct argument may not be passed
  // to a formal indirect parameter and vice-versa. Hence, the formally
  // indirect property, not the SIL indirect property, should be consulted to
  // determine whether function reabstraction is necessary.
  unsigned getNumIndirectFormalResults() const {
    return NumIndirectFormalResults;
  }
  /// Does this function have any formally indirect results?
  bool hasIndirectFormalResults() const {
    return getNumIndirectFormalResults() != 0;
  }
  unsigned getNumDirectFormalResults() const {
    return NumResults - NumIndirectFormalResults;
  }

  struct IndirectFormalResultFilter {
    bool operator()(SILResultInfo result) const {
      return result.isFormalIndirect();
    }
  };
  using IndirectFormalResultIter =
      llvm::filter_iterator<const SILResultInfo *, IndirectFormalResultFilter>;
  using IndirectFormalResultRange = IteratorRange<IndirectFormalResultIter>;

  /// A range of SILResultInfo for all formally indirect results.
  IndirectFormalResultRange getIndirectFormalResults() const {
    auto filter =
        llvm::make_filter_range(getResults(), IndirectFormalResultFilter());
    return makeIteratorRange(filter.begin(), filter.end());
  }

  struct DirectFormalResultFilter {
    bool operator()(SILResultInfo result) const {
      return !result.isFormalIndirect();
    }
  };
  using DirectFormalResultIter =
      llvm::filter_iterator<const SILResultInfo *, DirectFormalResultFilter>;
  using DirectFormalResultRange = IteratorRange<DirectFormalResultIter>;

  /// A range of SILResultInfo for all formally direct results.
  DirectFormalResultRange getDirectFormalResults() const {
    auto filter =
        llvm::make_filter_range(getResults(), DirectFormalResultFilter());
    return makeIteratorRange(filter.begin(), filter.end());
  }

  /// Get a single non-address SILType that represents all formal direct
  /// results. The actual SIL result type of an apply instruction that calls
  /// this function depends on the current SIL stage and is known by
  /// SILFunctionConventions. It may be a wider tuple that includes formally
  /// indirect results.
  SILType getDirectFormalResultsType();

  /// Get a single non-address SILType for all SIL results regardless of whether
  /// they are formally indirect. The actual SIL result type of an apply
  /// instruction that calls this function depends on the current SIL stage and
  /// is known by SILFunctionConventions. It may be a narrower tuple that omits
  /// formally indirect results.
  SILType getAllResultsType();

  /// Does this function have a blessed Swift-native error result?
  bool hasErrorResult() const {
    return SILFunctionTypeBits.HasErrorResult;
  }
  SILResultInfo getErrorResult() const {
    return const_cast<SILFunctionType*>(this)->getMutableErrorResult();
  }
  Optional<SILResultInfo> getOptionalErrorResult() const {
    if (hasErrorResult()) {
      return getErrorResult();
    } else {
      return None;
    }
  }

  /// Returns the number of function parameters, not including any formally
  /// indirect results.
  unsigned getNumParameters() const { return NumParameters; }

  ArrayRef<SILParameterInfo> getParameters() const {
    return const_cast<SILFunctionType*>(this)->getMutableParameters();
  }

  /// Returns the 'self' parameter, assuming that this is the type of
  /// a method.
  SILParameterInfo getSelfParameter() const {
    return getParameters().back();
  }

  bool isPolymorphic() const { return GenericSig != nullptr; }
  CanGenericSignature getGenericSignature() const { return GenericSig; }

  CanType getSelfInstanceType() const;

  /// If this a @convention(witness_method) function with an abstract
  /// self parameter, return the protocol constraint for the Self type.
  ProtocolDecl *getDefaultWitnessMethodProtocol(ModuleDecl &M) const;

  ExtInfo getExtInfo() const { return ExtInfo(SILFunctionTypeBits.ExtInfo); }

  /// \brief Returns the language-level calling convention of the function.
  Language getLanguage() const {
    return getExtInfo().getLanguage();
  }

  bool hasSelfParam() const {
    return getExtInfo().hasSelfParam();
  }

  /// \brief Get the representation of the function type.
  Representation getRepresentation() const {
    return getExtInfo().getRepresentation();
  }

  bool isPseudogeneric() const {
    return getExtInfo().isPseudogeneric();
  }

  bool isNoReturnFunction(); // Defined in SILType.cpp

  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                      SubstitutionList subs);
  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                      const SubstitutionMap &subs);
  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                      TypeSubstitutionFn subs,
                                      LookupConformanceFn conformances);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericSignature(), getExtInfo(), getCalleeConvention(),
            getParameters(), getResults(), getOptionalErrorResult());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericSignature *genericSig,
                      ExtInfo info,
                      ParameterConvention calleeConvention,
                      ArrayRef<SILParameterInfo> params,
                      ArrayRef<SILResultInfo> result,
                      Optional<SILResultInfo> errorResult);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILFunction;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILFunctionType, Type)

class SILBoxType;
class SILLayout; // From SIL
class SILModule; // From SIL
typedef CanTypeWrapper<SILBoxType> CanSILBoxType;

/// The SIL-only type for boxes, which represent a reference to a (non-class)
/// refcounted value referencing an aggregate with a given lowered layout.
class SILBoxType final : public TypeBase,
                         public llvm::FoldingSetNode,
                         private llvm::TrailingObjects<SILBoxType,Substitution>
{
  friend TrailingObjects;
  
  SILLayout *Layout;
  unsigned NumGenericArgs;

  static RecursiveTypeProperties
  getRecursivePropertiesFromSubstitutions(SubstitutionList Args);

  SILBoxType(ASTContext &C,
             SILLayout *Layout, SubstitutionList Args);

public:
  static CanSILBoxType get(ASTContext &C,
                           SILLayout *Layout,
                           SubstitutionList Args);

  SILLayout *getLayout() const { return Layout; }
  SubstitutionList getGenericArgs() const {
    return llvm::makeArrayRef(getTrailingObjects<Substitution>(),
                              NumGenericArgs);
  }
  
  // In SILType.h:
  CanType getFieldLoweredType(SILModule &M, unsigned index) const;
  SILType getFieldType(SILModule &M, unsigned index) const;

  // TODO: SILBoxTypes should be explicitly constructed in terms of specific
  // layouts. As a staging mechanism, we expose the old single-boxed-type
  // interface.
  
  static CanSILBoxType get(CanType BoxedType);

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILBox;
  }
  
  /// Produce a profile of this box, for use in a folding set.
  static void Profile(llvm::FoldingSetNodeID &id,
                      SILLayout *Layout,
                      SubstitutionList Args);
  
  /// \brief Produce a profile of this box, for use in a folding set.
  void Profile(llvm::FoldingSetNodeID &id) {
    Profile(id, getLayout(), getGenericArgs());
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILBoxType, Type)

class SILBlockStorageType;
typedef CanTypeWrapper<SILBlockStorageType> CanSILBlockStorageType;
  
/// The SIL-only type @block_storage T, which represents the layout of an
/// on-stack block that captures a value of type T.
///
/// This type does not have to be able to appear positionally, unlike
/// SILFunctionType, so it is only parsed and defined within the SIL library.
class SILBlockStorageType : public TypeBase {
  CanType CaptureType;
  
  SILBlockStorageType(CanType CaptureType)
    : TypeBase(TypeKind::SILBlockStorage,
               &CaptureType->getASTContext(),
               CaptureType->getRecursiveProperties()),
      CaptureType(CaptureType) {}
  
public:
  static CanSILBlockStorageType get(CanType CaptureType);
                      
  CanType getCaptureType() const { return CaptureType; }
  // In SILType.h
  SILType getCaptureAddressType() const;
  
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILBlockStorage;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILBlockStorageType, Type)
  

/// A type with a special syntax that is always sugar for a library type.
///
/// The prime examples are arrays ([T] -> Array<T>) and
/// optionals (T? -> Optional<T>).
class SyntaxSugarType : public TypeBase {
  Type Base;
  llvm::PointerUnion<Type, const ASTContext *> ImplOrContext;

protected:
  // Syntax sugar types are never canonical.
  SyntaxSugarType(TypeKind K, const ASTContext &ctx, Type base,
                  RecursiveTypeProperties properties)
    : TypeBase(K, nullptr, properties), Base(base), ImplOrContext(&ctx) {}

public:
  Type getBaseType() const {
    return Base;
  }

  TypeBase *getSinglyDesugaredType();

  Type getImplementationType();

  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SyntaxSugarType &&
           T->getKind() <= TypeKind::Last_SyntaxSugarType;
  }
};
  
/// The type [T], which is always sugar for a library type.
class ArraySliceType : public SyntaxSugarType {
  ArraySliceType(const ASTContext &ctx, Type base,
                 RecursiveTypeProperties properties)
    : SyntaxSugarType(TypeKind::ArraySlice, ctx, base, properties) {}

public:
  /// Return a uniqued array slice type with the specified base type.
  static ArraySliceType *get(Type baseTy);

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ArraySlice;
  }
};

/// The type T?, which is always sugar for a library type.
class OptionalType : public SyntaxSugarType {
  OptionalType(const ASTContext &ctx,Type base,
               RecursiveTypeProperties properties)
    : SyntaxSugarType(TypeKind::Optional, ctx, base, properties) {}

public:
  /// Return a uniqued optional type with the specified base type.
  static OptionalType *get(Type baseTy);

  /// Build one of the optional type sugar kinds.
  ///
  /// It's a bit unnatural to have this on OptionalType, but we don't
  /// have an abstract common class, and polluting TypeBase with it
  /// would be unfortunate.  If we ever make an AnyOptionalType,
  /// we can move it there.
  ///
  /// \param kind - can't be OTK_None
  static Type get(OptionalTypeKind kind, Type baseTy);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Optional;
  }
};

/// The type T!, which is always sugar for a library type.
class ImplicitlyUnwrappedOptionalType : public SyntaxSugarType {
  ImplicitlyUnwrappedOptionalType(const ASTContext &ctx, Type base,
                        RecursiveTypeProperties properties)
    : SyntaxSugarType(TypeKind::ImplicitlyUnwrappedOptional, ctx, base, properties) {}

public:
  /// Return a uniqued optional type with the specified base type.
  static ImplicitlyUnwrappedOptionalType *get(Type baseTy);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ImplicitlyUnwrappedOptional;
  }
};

/// The dictionary type [K : V], which is syntactic sugar for Dictionary<K, V>.
///
/// Example:
/// \code
/// var dict: [String : Int] = ["hello" : 0, "world" : 1]
/// \endcode
class DictionaryType : public TypeBase {
  Type Key;
  Type Value;
  llvm::PointerUnion<Type, const ASTContext *> ImplOrContext;

protected:
  // Syntax sugar types are never canonical.
  DictionaryType(const ASTContext &ctx, Type key, Type value,
                 RecursiveTypeProperties properties)
    : TypeBase(TypeKind::Dictionary, nullptr, properties), 
      Key(key), Value(value), ImplOrContext(&ctx) {}

public:
  /// Return a uniqued dictionary type with the specified key and value types.
  static DictionaryType *get(Type keyTy, Type valueTy);

  Type getKeyType() const { return Key; }
  Type getValueType() const { return Value; }

  TypeBase *getSinglyDesugaredType();

  Type getImplementationType();

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Dictionary;
  }

  static bool classof(const DictionaryType *T) {
    return true;
  }
};

/// ProtocolType - A protocol type describes an abstract interface implemented
/// by another type.
class ProtocolType : public NominalType, public llvm::FoldingSetNode {
public:
  /// \brief Retrieve the type when we're referencing the given protocol.
  /// declaration.
  static ProtocolType *get(ProtocolDecl *D, Type Parent, const ASTContext &C);

  ProtocolDecl *getDecl() const {
    return reinterpret_cast<ProtocolDecl *>(NominalType::getDecl());
  }

  /// True if only classes may conform to the protocol.
  bool requiresClass() const;

  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protos) {
    protos.push_back(getDecl());
  }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Protocol;
  }

  /// Canonicalizes the given set of protocols by eliminating any mentions
  /// of protocols that are already covered by inheritance due to other entries
  /// in the protocol list, then sorting them in some stable order.
  static void canonicalizeProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

  /// Visit all of the protocols in the given list of protocols, along with their
  ///
  /// \param fn Visitor function called for each protocol (just once). If it
  /// returns \c true, the visit operation will abort and return \c true.
  ///
  /// \returns \c true if any invocation of \c fn returns \c true, and \c false
  /// otherwise.
  static bool visitAllProtocols(ArrayRef<ProtocolDecl *> protocols,
                                llvm::function_ref<bool(ProtocolDecl *)> fn);

  /// Compare two protocols to provide them with a stable ordering for
  /// use in sorting.
  static int compareProtocols(ProtocolDecl * const* PP1,
                              ProtocolDecl * const* PP2);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, ProtocolDecl *D, Type Parent);

private:
  friend class NominalTypeDecl;
  ProtocolType(ProtocolDecl *TheDecl, Type Parent, const ASTContext &Ctx,
               RecursiveTypeProperties properties);
};
BEGIN_CAN_TYPE_WRAPPER(ProtocolType, NominalType)
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protos) {
    getPointer()->getAnyExistentialTypeProtocols(protos);
  }
END_CAN_TYPE_WRAPPER(ProtocolType, NominalType)

/// ProtocolCompositionType - A type that composes some number of protocols
/// together to represent types that conform to all of the named protocols.
///
/// \code
/// protocol P { /* ... */ }
/// protocol Q { /* ... */ }
/// var x : P & Q
/// \endcode
///
/// Here, the type of x is a composition of the protocols 'P' and 'Q'.
///
/// The canonical form of a protocol composition type is based on a sorted (by
/// module and name), minimized (based on redundancy due to protocol
/// inheritance) protocol list. If the sorted, minimized list is a single
/// protocol, then the canonical type is that protocol type. Otherwise, it is
/// a composition of the protocols in that list.
class ProtocolCompositionType : public TypeBase, public llvm::FoldingSetNode {
  ArrayRef<Type> Protocols;
  
public:
  /// \brief Retrieve an instance of a protocol composition type with the
  /// given set of protocols.
  static Type get(const ASTContext &C, ArrayRef<Type> Protocols);
  
  /// \brief Retrieve the set of protocols composed to create this type.
  ArrayRef<Type> getProtocols() const { return Protocols; }

  /// \brief Return the protocols of this type in canonical order.
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protos);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Protocols);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, ArrayRef<Type> Protocols);

  /// True if one or more of the protocols is class.
  bool requiresClass() const;
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ProtocolComposition;
  }
  
private:
  static ProtocolCompositionType *build(const ASTContext &C,
                                        ArrayRef<Type> Protocols);

  ProtocolCompositionType(const ASTContext *Ctx, ArrayRef<Type> Protocols)
    : TypeBase(TypeKind::ProtocolComposition, /*Context=*/Ctx,
               RecursiveTypeProperties()),
      Protocols(Protocols) { }
};
BEGIN_CAN_TYPE_WRAPPER(ProtocolCompositionType, Type)
  /// In the canonical representation, these are all ProtocolTypes.
  CanTypeArrayRef getProtocols() const {
    return CanTypeArrayRef(getPointer()->getProtocols());
  }
  void getAnyExistentialTypeProtocols(SmallVectorImpl<ProtocolDecl *> &protos) {
    for (CanType proto : getProtocols()) {
      cast<ProtocolType>(proto).getAnyExistentialTypeProtocols(protos);
    }
  }
END_CAN_TYPE_WRAPPER(ProtocolCompositionType, Type)

/// LValueType - An l-value is a handle to a physical object.  The
/// type of that object uniquely determines the type of an l-value
/// for it.
///
/// L-values are not fully first-class in Swift:
///
///  A type is said to "carry" an l-value if
///   - it is an l-value type or
///   - it is a tuple and at least one of its element types
///     carries an l-value.
///
/// The type of a function argument may carry an l-value.  This is done by
/// annotating the bound variable with InOutType.
///
/// The type of a return value, local variable, or field may not
/// carry an l-value.
///
/// When inferring a value type from an expression whose type
/// carries an l-value, the carried l-value types are converted
/// to their object type.
class LValueType : public TypeBase {
  Type ObjectTy;

  LValueType(Type objectTy, const ASTContext *canonicalContext,
             RecursiveTypeProperties properties)
    : TypeBase(TypeKind::LValue, canonicalContext, properties),
      ObjectTy(objectTy) {}

public:
  static LValueType *get(Type type);

  Type getObjectType() const { return ObjectTy; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *type) {
    return type->getKind() == TypeKind::LValue;
  }
};
BEGIN_CAN_TYPE_WRAPPER(LValueType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getObjectType)
  static CanLValueType get(CanType type) {
    return CanLValueType(LValueType::get(type));
  }
END_CAN_TYPE_WRAPPER(LValueType, Type)
  
/// InOutType - An inout qualified type is an argument to a function passed
/// with an explicit "Address of" operator.  It is read in and then written back
/// to after the callee function is done.  This also models the receiver of
/// @mutable methods on value types.
///
class InOutType : public TypeBase {
  Type ObjectTy;
  
  InOutType(Type objectTy, const ASTContext *canonicalContext,
            RecursiveTypeProperties properties)
  : TypeBase(TypeKind::InOut, canonicalContext, properties),
    ObjectTy(objectTy) {}
  
public:
  static InOutType *get(Type type);
  
  Type getObjectType() const { return ObjectTy; }
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *type) {
    return type->getKind() == TypeKind::InOut;
  }
};
BEGIN_CAN_TYPE_WRAPPER(InOutType, Type)
PROXY_CAN_TYPE_SIMPLE_GETTER(getObjectType)
static CanInOutType get(CanType type) {
  return CanInOutType(InOutType::get(type));
}
END_CAN_TYPE_WRAPPER(InOutType, Type)


/// SubstitutableType - A reference to a type that can be substituted, i.e.,
/// an archetype or a generic parameter.
class SubstitutableType : public TypeBase {
protected:
  SubstitutableType(TypeKind K, const ASTContext *C,
                    RecursiveTypeProperties properties)
    : TypeBase(K, C, properties) { }

public:
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SubstitutableType &&
           T->getKind() <= TypeKind::Last_SubstitutableType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SubstitutableType, Type)

/// An archetype is a type that represents a runtime type that is
/// known to conform to some set of requirements.
///
/// Archetypes are used to represent generic type parameters and their
/// associated types, as well as the runtime type stored within an
/// existential container.
class ArchetypeType final : public SubstitutableType,
  private llvm::TrailingObjects<ArchetypeType, ProtocolDecl *,
                                Type, LayoutConstraint, UUID> {
  friend TrailingObjects;

  size_t numTrailingObjects(OverloadToken<ProtocolDecl *>) const {
    return ArchetypeTypeBits.NumProtocols;
  }

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return ArchetypeTypeBits.HasSuperclass ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<LayoutConstraint>) const {
    return ArchetypeTypeBits.HasLayoutConstraint ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<UUID>) const {
    return getOpenedExistentialType() ? 1 : 0;
  }

  llvm::PointerUnion3<ArchetypeType *, TypeBase *,
                      GenericEnvironment *> ParentOrOpenedOrEnvironment;
  llvm::PointerUnion<AssociatedTypeDecl *, Identifier> AssocTypeOrName;
  MutableArrayRef<std::pair<Identifier, Type>> NestedTypes;

  void populateNestedTypes() const;
  void resolveNestedType(std::pair<Identifier, Type> &nested) const;

public:
  /// getNew - Create a new nested archetype with the given associated type.
  ///
  /// The ConformsTo array will be copied into the ASTContext by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               AssociatedTypeDecl *AssocType,
                               SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                               Type Superclass, LayoutConstraint Layout);

  /// getNew - Create a new primary archetype with the given name.
  ///
  /// The ConformsTo array will be minimized then copied into the ASTContext
  /// by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx,
                               GenericEnvironment *genericEnvironment,
                               Identifier Name,
                               SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                               Type Superclass, LayoutConstraint Layout);

  /// Create a new archetype that represents the opened type
  /// of an existential value.
  ///
  /// \param existential The existential type to open.
  ///
  /// \param knownID When non-empty, the known ID of the archetype. When empty,
  /// a fresh archetype with a unique ID will be opened.
  static CanTypeWrapper<ArchetypeType>
                        getOpened(Type existential, 
                                  Optional<UUID> knownID = None);

  /// Create a new archetype that represents the opened type
  /// of an existential value.
  ///
  /// \param existential The existential type or existential metatype to open.
  static CanType getAnyOpened(Type existential);

  /// \brief Retrieve the name of this archetype.
  Identifier getName() const;

  /// \brief Retrieve the fully-dotted name that should be used to display this
  /// archetype.
  std::string getFullName() const;

  /// \brief Retrieve the parent of this archetype, or null if this is a
  /// primary archetype.
  ArchetypeType *getParent() const { 
    return ParentOrOpenedOrEnvironment.dyn_cast<ArchetypeType *>();
  }

  /// Retrieve the opened existential type 
  Type getOpenedExistentialType() const {
    return ParentOrOpenedOrEnvironment.dyn_cast<TypeBase *>();
  }

  /// Retrieve the generic environment in which this archetype resides.
  ///
  /// Note: opened archetypes currently don't have generic environments.
  GenericEnvironment *getGenericEnvironment() const;

  /// Retrieve the associated type to which this archetype (if it is a nested
  /// archetype) corresponds.
  ///
  /// This associated type will have the same name as the archetype and will
  /// be a member of one of the protocols to which the parent archetype
  /// conforms.
  AssociatedTypeDecl *getAssocType() const {
    return AssocTypeOrName.dyn_cast<AssociatedTypeDecl *>();
  }

  /// getConformsTo - Retrieve the set of protocols to which this substitutable
  /// type shall conform.
  ArrayRef<ProtocolDecl *> getConformsTo() const {
    return { getTrailingObjects<ProtocolDecl *>(),
             ArchetypeTypeBits.NumProtocols };
  }
  
  /// requiresClass - True if the type can only be substituted with class types.
  /// This is true if the type conforms to one or more class protocols or has
  /// a superclass constraint.
  bool requiresClass() const;

  /// \brief Retrieve the superclass of this type, if such a requirement exists.
  Type getSuperclass() const {
    if (!ArchetypeTypeBits.HasSuperclass) return Type();

    return *getTrailingObjects<Type>();
  }

  /// \brief Retrieve the layout constraint of this type, if such a requirement exists.
  LayoutConstraint getLayoutConstraint() const {
    if (!ArchetypeTypeBits.HasLayoutConstraint) return LayoutConstraint();

    return *getTrailingObjects<LayoutConstraint>();
  }

  /// \brief Return true if the archetype has any requirements at all.
  bool hasRequirements() const {
    return !getConformsTo().empty() || getSuperclass();
  }

  /// \brief Retrieve the nested type with the given name.
  Type getNestedType(Identifier Name) const;

  /// \brief Retrieve the nested type with the given name, if it's already
  /// known.
  ///
  /// This is an implementation detail used by the generic signature builder.
  Optional<Type> getNestedTypeIfKnown(Identifier Name) const;

  /// \brief Check if the archetype contains a nested type with the given name.
  bool hasNestedType(Identifier Name) const;

  /// \brief Retrieve the known nested types of this archetype.
  ///
  /// Useful only for debugging dumps; all other queries should attempt to
  /// find a particular nested type by name, directly, or look at the
  /// protocols to which this archetype conforms.
  ArrayRef<std::pair<Identifier, Type>>
  getKnownNestedTypes(bool resolveTypes = true) const {
    return getAllNestedTypes(/*resolveTypes=*/false);
  }

  /// \brief Retrieve the nested types of this archetype.
  ///
  /// \param resolveTypes Whether to eagerly resolve the nested types
  /// (defaults to \c true). Otherwise, the nested types might be
  /// null.
  ///
  /// FIXME: This operation should go away, because it breaks recursive
  /// protocol constraints.
  ArrayRef<std::pair<Identifier, Type>>
  getAllNestedTypes(bool resolveTypes = true) const;

  /// \brief Set the nested types to a copy of the given array of
  /// archetypes.
  void setNestedTypes(ASTContext &Ctx,
                      ArrayRef<std::pair<Identifier, Type>> Nested);

  /// Register a nested type with the given name.
  void registerNestedType(Identifier name, Type nested);

  /// isPrimary - Determine whether this is the archetype for a 'primary'
  /// archetype, e.g., one that is not nested within another archetype and is
  /// not an opened existential.
  bool isPrimary() const { 
    return ParentOrOpenedOrEnvironment.is<GenericEnvironment *>();
  }

  /// getPrimary - Return the primary archetype parent of this archetype.
  ArchetypeType *getPrimary() const {
    assert(!getOpenedExistentialType() && "Check for opened existential first");

    auto *archetype = this;
    while (auto *parent = archetype->getParent())
      archetype = parent;
    return const_cast<ArchetypeType *>(archetype);
  }

  /// Retrieve the ID number of this opened existential.
  UUID getOpenedExistentialID() const {
    assert(getOpenedExistentialType() && "Not an opened existential archetype");
    // The UUID is tail-allocated at the end of opened existential archetypes.
    return *getTrailingObjects<UUID>();
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Archetype;
  }

private:
  ArchetypeType(
          const ASTContext &Ctx,
          llvm::PointerUnion<ArchetypeType *, GenericEnvironment *>
            ParentOrGenericEnv,
          llvm::PointerUnion<AssociatedTypeDecl *, Identifier> AssocTypeOrName,
          ArrayRef<ProtocolDecl *> ConformsTo,
          Type Superclass, LayoutConstraint Layout);

  ArchetypeType(const ASTContext &Ctx, Type Existential,
                ArrayRef<ProtocolDecl *> ConformsTo, Type Superclass,
                LayoutConstraint Layout, UUID uuid);
};
BEGIN_CAN_TYPE_WRAPPER(ArchetypeType, SubstitutableType)
CanArchetypeType getParent() const {
  return CanArchetypeType(getPointer()->getParent());
}
END_CAN_TYPE_WRAPPER(ArchetypeType, SubstitutableType)

/// Describes the type of a generic parameter.
///
/// \sa GenericTypeParamDecl
class GenericTypeParamType : public SubstitutableType {
  using DepthIndexTy = llvm::PointerEmbeddedInt<unsigned, 31>;

  /// The generic type parameter or depth/index.
  llvm::PointerUnion<GenericTypeParamDecl *, DepthIndexTy> ParamOrDepthIndex;

public:
  /// Retrieve a generic type parameter at the given depth and index.
  static GenericTypeParamType *get(unsigned depth, unsigned index,
                                   const ASTContext &ctx);

  /// Retrieve the declaration of the generic type parameter, or null if
  /// there is no such declaration.
  GenericTypeParamDecl *getDecl() const {
    return ParamOrDepthIndex.dyn_cast<GenericTypeParamDecl *>();
  }

  /// Get the name of the generic type parameter.
  Identifier getName() const;
  
  /// The depth of this generic type parameter, i.e., the number of outer
  /// levels of generic parameter lists that enclose this type parameter.
  ///
  /// \code
  /// struct X<T> {
  ///   func f<U>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' has depth 0 and 'U' has depth 1. Both have index 0.
  unsigned getDepth() const;

  /// The index of this generic type parameter within its generic parameter
  /// list.
  ///
  /// \code
  /// struct X<T, U> {
  ///   func f<V>() { }
  /// }
  /// \endcode
  ///
  /// Here 'T' and 'U' have indexes 0 and 1, respectively. 'V' has index 0.
  unsigned getIndex() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::GenericTypeParam;
  }

private:
  friend class GenericTypeParamDecl;

  explicit GenericTypeParamType(GenericTypeParamDecl *param)
    : SubstitutableType(TypeKind::GenericTypeParam, nullptr,
                        RecursiveTypeProperties::HasTypeParameter),
      ParamOrDepthIndex(param) { }

  explicit GenericTypeParamType(unsigned depth,
                                unsigned index,
                                const ASTContext &ctx)
    : SubstitutableType(TypeKind::GenericTypeParam, &ctx,
                        RecursiveTypeProperties::HasTypeParameter),
      ParamOrDepthIndex(depth << 16 | index) { }
};
BEGIN_CAN_TYPE_WRAPPER(GenericTypeParamType, SubstitutableType)
  static CanGenericTypeParamType get(unsigned depth, unsigned index,
                                     const ASTContext &C) {
    return CanGenericTypeParamType(GenericTypeParamType::get(depth, index, C));
  }
END_CAN_TYPE_WRAPPER(GenericTypeParamType, SubstitutableType)

/// A type that refers to a member type of some type that is dependent on a
/// generic parameter.
class DependentMemberType : public TypeBase {
  Type Base;
  llvm::PointerUnion<Identifier, AssociatedTypeDecl *> NameOrAssocType;

  DependentMemberType(Type base, Identifier name, const ASTContext *ctx,
                      RecursiveTypeProperties properties)
    : TypeBase(TypeKind::DependentMember, ctx, properties),
      Base(base), NameOrAssocType(name) { }

  DependentMemberType(Type base, AssociatedTypeDecl *assocType,
                      const ASTContext *ctx,
                      RecursiveTypeProperties properties)
    : TypeBase(TypeKind::DependentMember, ctx, properties),
      Base(base), NameOrAssocType(assocType) { }

public:
  static DependentMemberType *get(Type base, Identifier name);
  static DependentMemberType *get(Type base, AssociatedTypeDecl *assocType);

  /// Retrieve the base type.
  Type getBase() const { return Base; }

  /// Retrieve the name of the member type.
  Identifier getName() const;

  /// Retrieve the associated type referenced as a member.
  ///
  /// The associated type will only be available after successful type checking.
  AssociatedTypeDecl *getAssocType() const {
    return NameOrAssocType.dyn_cast<AssociatedTypeDecl *>();
  }
  
  /// Substitute the base type, looking up our associated type in it if it is
  /// non-dependent. Returns null if the member could not be found in the new
  /// base.
  Type substBaseType(ModuleDecl *M,
                     Type base,
                     LazyResolver *resolver = nullptr);

  /// Substitute the base type, looking up our associated type in it if it is
  /// non-dependent. Returns null if the member could not be found in the new
  /// base.
  Type substBaseType(Type base, LookupConformanceFn lookupConformance);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::DependentMember;
  }
};
BEGIN_CAN_TYPE_WRAPPER(DependentMemberType, Type)
  static CanDependentMemberType get(CanType base, AssociatedTypeDecl *assocType,
                                    const ASTContext &C) {
    return CanDependentMemberType(DependentMemberType::get(base, assocType));
  }

  PROXY_CAN_TYPE_SIMPLE_GETTER(getBase)
END_CAN_TYPE_WRAPPER(DependentMemberType, Type)


/// \brief The storage type of a variable with non-strong reference
/// ownership semantics.
///
/// The referent type always satisfies allowsOwnership().
///
/// These types may appear in the AST only as the type of a variable;
/// getTypeOfReference strips this layer from the formal type of a
/// reference to the variable.  However, it is extremely useful to
/// represent this as a distinct type in SIL and IR-generation.
class ReferenceStorageType : public TypeBase {
protected:
  ReferenceStorageType(TypeKind kind, Type referent, const ASTContext *C,
                       RecursiveTypeProperties properties)
    : TypeBase(kind, C, properties), Referent(referent) {}

private:
  Type Referent;
public:
  static ReferenceStorageType *get(Type referent, Ownership ownership,
                                   const ASTContext &C);

  Type getReferentType() const { return Referent; }
  Ownership getOwnership() const {
    return (getKind() == TypeKind::WeakStorage    ? Ownership::Weak :
            getKind() == TypeKind::UnownedStorage ? Ownership::Unowned
                                                  : Ownership::Unmanaged);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_ReferenceStorageType &&
           T->getKind() <= TypeKind::Last_ReferenceStorageType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(ReferenceStorageType, Type)
  static CanReferenceStorageType get(CanType referent, Ownership ownership) {
    return CanReferenceStorageType(ReferenceStorageType::get(referent,
                                                   ownership,
                                                   referent->getASTContext()));
  }
  PROXY_CAN_TYPE_SIMPLE_GETTER(getReferentType)
END_CAN_TYPE_WRAPPER(ReferenceStorageType, Type)

/// \brief The storage type of a variable with @unowned ownership semantics.
class UnownedStorageType : public ReferenceStorageType {
  friend class ReferenceStorageType;
  UnownedStorageType(Type referent, const ASTContext *C,
                     RecursiveTypeProperties properties)
    : ReferenceStorageType(TypeKind::UnownedStorage, referent, C, properties) {}

public:
  static UnownedStorageType *get(Type referent, const ASTContext &C) {
    return static_cast<UnownedStorageType*>(
                 ReferenceStorageType::get(referent, Ownership::Unowned, C));
  }

  /// Is this unowned storage type known to be loadable within the given
  /// resilience scope?
  bool isLoadable(ResilienceExpansion resilience) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnownedStorage;
  }
};
BEGIN_CAN_TYPE_WRAPPER(UnownedStorageType, ReferenceStorageType)
  static CanUnownedStorageType get(CanType referent) {
    return cast<UnownedStorageType>(
        CanType(UnownedStorageType::get(referent, referent->getASTContext())));
  }
END_CAN_TYPE_WRAPPER(UnownedStorageType, ReferenceStorageType)

/// \brief The storage type of a variable with @unowned(unsafe)
/// ownership semantics, akin to the library Unmanaged<> type.
class UnmanagedStorageType : public ReferenceStorageType {
  friend class ReferenceStorageType;
  UnmanagedStorageType(Type referent, const ASTContext *C,
                       RecursiveTypeProperties properties)
    : ReferenceStorageType(TypeKind::UnmanagedStorage, referent, C,
                           properties) {}

public:
  static UnmanagedStorageType *get(Type referent, const ASTContext &C) {
    return static_cast<UnmanagedStorageType*>(
                 ReferenceStorageType::get(referent, Ownership::Unmanaged, C));
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::UnmanagedStorage;
  }
};
BEGIN_CAN_TYPE_WRAPPER(UnmanagedStorageType, ReferenceStorageType)
  static CanUnmanagedStorageType get(CanType referent) {
    return cast<UnmanagedStorageType>(
       CanType(UnmanagedStorageType::get(referent, referent->getASTContext())));
  }
END_CAN_TYPE_WRAPPER(UnmanagedStorageType, ReferenceStorageType)

/// \brief The storage type of a variable with [weak] ownership semantics.
class WeakStorageType : public ReferenceStorageType {
  friend class ReferenceStorageType;
  WeakStorageType(Type referent, const ASTContext *C,
                  RecursiveTypeProperties properties)
    : ReferenceStorageType(TypeKind::WeakStorage, referent, C, properties) {}

public:
  static WeakStorageType *get(Type referent, const ASTContext &C) {
    return static_cast<WeakStorageType*>(
                    ReferenceStorageType::get(referent, Ownership::Weak, C));
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::WeakStorage;
  }
};
BEGIN_CAN_TYPE_WRAPPER(WeakStorageType, ReferenceStorageType)
  static CanWeakStorageType get(CanType referent) {
    return cast<WeakStorageType>(
       CanType(WeakStorageType::get(referent, referent->getASTContext())));
  }
END_CAN_TYPE_WRAPPER(WeakStorageType, ReferenceStorageType)

/// \brief A type variable used during type checking.
class TypeVariableType : public TypeBase {
  // Note: We can't use llvm::TrailingObjects here because the trailing object
  // type is opaque.

  TypeVariableType(const ASTContext &C, unsigned ID)
    : TypeBase(TypeKind::TypeVariable, &C,
               RecursiveTypeProperties::HasTypeVariable) {
    TypeVariableTypeBits.ID = ID;
  }

  class Implementation;
  
public:
 
  /// \brief Create a new type variable whose implementation is constructed
  /// with the given arguments.
  template<typename ...Args>
  static TypeVariableType *getNew(const ASTContext &C, unsigned ID,
                                  Args &&...args);
  
  /// \brief Retrieve the implementation data corresponding to this type
  /// variable.
  ///
  /// The contents of the implementation data for this type are hidden in the
  /// details of the constraint solver used for type checking.
  Implementation &getImpl() {
    return *reinterpret_cast<Implementation *>(this + 1);
  }

  /// \brief Retrieve the implementation data corresponding to this type
  /// variable.
  ///
  /// The contents of the implementation data for this type are hidden in the
  /// details of the constraint solver used for type checking.
  const Implementation &getImpl() const {
    return *reinterpret_cast<const Implementation *>(this + 1);
  }

  /// \brief Access the implementation object for this type variable.
  Implementation *operator->() {
    return reinterpret_cast<Implementation *>(this + 1);
  }

  unsigned getID() const { return TypeVariableTypeBits.ID; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::TypeVariable;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(TypeVariableType, Type)

inline bool TypeBase::isTypeVariableOrMember() {
  if (is<TypeVariableType>())
    return true;

  if (auto depMemTy = getAs<DependentMemberType>())
    return depMemTy->getBase()->isTypeVariableOrMember();

  return false;
}

inline bool TypeBase::isTypeParameter() {
  if (is<GenericTypeParamType>())
    return true;

  if (auto depMemTy = getAs<DependentMemberType>())
    return depMemTy->getBase()->isTypeParameter();

  return false;
}

inline bool TypeBase::isExistentialType() {
  return getCanonicalType().isExistentialType();
}

inline bool TypeBase::isAnyExistentialType() {
  return getCanonicalType().isAnyExistentialType();
}

inline bool CanType::isExistentialTypeImpl(CanType type) {
  return isa<ProtocolType>(type) || isa<ProtocolCompositionType>(type);
}

inline bool CanType::isAnyExistentialTypeImpl(CanType type) {
  return isExistentialTypeImpl(type) || isa<ExistentialMetatypeType>(type);
}

inline bool TypeBase::isClassExistentialType() {
  CanType T = getCanonicalType();
  if (auto pt = dyn_cast<ProtocolType>(T))
    return pt->requiresClass();
  if (auto pct = dyn_cast<ProtocolCompositionType>(T))
    return pct->requiresClass();
  return false;
}

inline bool TypeBase::isOpenedExistential() {
  if (!hasOpenedExistential())
    return false;

  CanType T = getCanonicalType();
  if (auto archetype = dyn_cast<ArchetypeType>(T))
    return !archetype->getOpenedExistentialType().isNull();
  return false;
}

inline ClassDecl *TypeBase::getClassOrBoundGenericClass() {
  return getCanonicalType().getClassOrBoundGenericClass();
}

inline ClassDecl *CanType::getClassOrBoundGenericClass() const {
  if (auto classTy = dyn_cast<ClassType>(*this))
    return classTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericClassType>(*this))
    return boundTy->getDecl();

  return nullptr;
}

inline StructDecl *TypeBase::getStructOrBoundGenericStruct() {
  return getCanonicalType().getStructOrBoundGenericStruct();
}

inline StructDecl *CanType::getStructOrBoundGenericStruct() const {
  if (auto structTy = dyn_cast<StructType>(*this))
    return structTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericStructType>(*this))
    return boundTy->getDecl();
  
  return nullptr;
}

inline EnumDecl *TypeBase::getEnumOrBoundGenericEnum() {
  return getCanonicalType().getEnumOrBoundGenericEnum();
}

inline EnumDecl *CanType::getEnumOrBoundGenericEnum() const {
  if (auto enumTy = dyn_cast<EnumType>(*this))
    return enumTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericEnumType>(*this))
    return boundTy->getDecl();
  
  return nullptr;
}

inline NominalTypeDecl *TypeBase::getNominalOrBoundGenericNominal() {
  return getCanonicalType().getNominalOrBoundGenericNominal();
}

inline NominalTypeDecl *CanType::getNominalOrBoundGenericNominal() const {
  if (auto nomTy = dyn_cast<NominalType>(*this))
    return nomTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericType>(*this))
    return boundTy->getDecl();
  
  return nullptr;
}

inline NominalTypeDecl *TypeBase::getAnyNominal() {
  return getCanonicalType().getAnyNominal();
}

inline Type TypeBase::getNominalParent() {
  if (auto classType = getAs<NominalType>()) {
    return classType->getParent();
  } else {
    return castTo<BoundGenericType>()->getParent();
  }
}

inline GenericTypeDecl *TypeBase::getAnyGeneric() {
  return getCanonicalType().getAnyGeneric();
}

  
  
inline bool TypeBase::isBuiltinIntegerType(unsigned n) {
  if (auto intTy = dyn_cast<BuiltinIntegerType>(getCanonicalType()))
    return intTy->getWidth().isFixedWidth()
      && intTy->getWidth().getFixedWidth() == n;
  return false;
}

/// getInOutObjectType - For an inout type, retrieves the underlying object
/// type.  Otherwise, returns the type itself.
inline Type TypeBase::getInOutObjectType() {
  if (auto iot = getAs<InOutType>())
    return iot->getObjectType();
  return this;
}

inline Type TypeBase::getRValueObjectType() {
  Type type = this;

  // Look through lvalue type.
  if (auto lv = type->getAs<LValueType>())
    type = lv->getObjectType();

  // Look through argument list tuples.
  return type->getWithoutImmediateLabel();
}

/// getLValueOrInOutObjectType - For an @lvalue or inout type, retrieves the
/// underlying object type. Otherwise, returns the type itself.
inline Type TypeBase::getLValueOrInOutObjectType() {
  if (auto iot = getAs<InOutType>())
    return iot->getObjectType();
  if (auto lv = getAs<LValueType>())
    return lv->getObjectType();
  return this;
}

/// For a ReferenceStorageType like @unowned, this returns the referent.
/// Otherwise, it returns the type itself.
inline Type TypeBase::getReferenceStorageReferent() {
  if (auto rst = getAs<ReferenceStorageType>())
    return rst->getReferentType();
  return this;
}

inline CanType CanType::getReferenceStorageReferentImpl(CanType type) {
  if (auto refType = dyn_cast<ReferenceStorageType>(type))
    return refType.getReferentType();
  return type;
}

inline CanType CanType::getLValueOrInOutObjectTypeImpl(CanType type) {
  if (auto refType = dyn_cast<InOutType>(type))
    return refType.getObjectType();
  if (auto refType = dyn_cast<LValueType>(type))
    return refType.getObjectType();
  return type;
}

inline CanType CanType::getNominalParent() const {
  if (auto classType = dyn_cast<NominalType>(*this)) {
    return classType.getParent();
  } else {
    return cast<BoundGenericType>(*this).getParent();
  }
}

inline bool TypeBase::mayHaveSuperclass() {
  if (getClassOrBoundGenericClass())
    return true;

  if (auto archetype = getAs<ArchetypeType>())
    return (bool)archetype->requiresClass();

  return is<DynamicSelfType>();
}

inline TupleTypeElt::TupleTypeElt(Type ty, Identifier name, bool isVariadic,
                                  bool isAutoClosure, bool isEscaping)
    : Name(name), ElementType(ty),
      Flags(isVariadic, isAutoClosure, isEscaping) {
  assert(!isVariadic ||
         ty->hasError() ||
         isa<ArraySliceType>(ty.getPointer()) ||
         (isa<BoundGenericType>(ty.getPointer()) &&
          ty->castTo<BoundGenericType>()->getGenericArgs().size() == 1));
  assert(!isAutoClosure || (ty->is<AnyFunctionType>() &&
                            ty->castTo<AnyFunctionType>()->isAutoClosure()));
  assert(!isEscaping || (ty->is<AnyFunctionType>() &&
                         !ty->castTo<AnyFunctionType>()->isNoEscape()));
}

inline Type TupleTypeElt::getVarargBaseTy(Type VarArgT) {
  TypeBase *T = VarArgT.getPointer();
  if (ArraySliceType *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  if (BoundGenericType *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
  assert(T->hasError());
  return T;
}

/// Create one from what's present in the parameter decl and type
inline ParameterTypeFlags
ParameterTypeFlags::fromParameterType(Type paramTy, bool isVariadic) {
  bool autoclosure = paramTy->is<AnyFunctionType>() &&
                     paramTy->castTo<AnyFunctionType>()->isAutoClosure();
  bool escaping = paramTy->is<AnyFunctionType>() &&
                  !paramTy->castTo<AnyFunctionType>()->isNoEscape();
  return {isVariadic, autoclosure, escaping};
}

#define TYPE(id, parent)
#define SUGARED_TYPE(id, parent) \
template <> \
constexpr bool TypeBase::isSugaredType<id##Type>() { \
  return true; \
}
#include "swift/AST/TypeNodes.def"

inline GenericParamKey::GenericParamKey(const GenericTypeParamType *p)
  : Depth(p->getDepth()), Index(p->getIndex()) { }

} // end namespace swift

namespace llvm {

// DenseMapInfo for BuiltinIntegerWidth.
template<>
struct DenseMapInfo<swift::BuiltinIntegerWidth> {
  using BuiltinIntegerWidth = swift::BuiltinIntegerWidth;
  
  static inline BuiltinIntegerWidth getEmptyKey() {
    return BuiltinIntegerWidth(BuiltinIntegerWidth::DenseMapEmpty);
  }
  
  static inline BuiltinIntegerWidth getTombstoneKey() {
    return BuiltinIntegerWidth(BuiltinIntegerWidth::DenseMapTombstone);
  }
  
  static unsigned getHashValue(BuiltinIntegerWidth w) {
    return DenseMapInfo<unsigned>::getHashValue(w.RawValue);
  }
  
  static bool isEqual(BuiltinIntegerWidth a, BuiltinIntegerWidth b) {
    return a == b;
  }
};


}
  
#endif
