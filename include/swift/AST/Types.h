//===--- Types.h - Swift Language Type ASTs ---------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2018 Apple Inc. and the Swift project authors
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

// SWIFT_ENABLE_TENSORFLOW
#include "swift/AST/AutoDiff.h"
#include "swift/AST/DeclContext.h"
#include "swift/AST/GenericParamKey.h"
#include "swift/AST/Identifier.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/ProtocolConformanceRef.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/SILLayout.h"
#include "swift/AST/SubstitutionMap.h"
#include "swift/AST/Type.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/InlineBitfield.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerEmbeddedInt.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/SmallBitVector.h"
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
  class DependentMemberType;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class GenericParamList;
  class GenericSignature;
  class Identifier;
  class InOutType;
  enum class ReferenceCounting : uint8_t;
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
  enum PointerTypeKind : unsigned;
  struct ValueOwnershipKind;
  // SWIFT_ENABLE_TENSORFLOW
  struct SILAutoDiffConfig;

  enum class TypeKind : uint8_t {
#define TYPE(id, parent) id,
#define LAST_TYPE(id) Last_Type = id,
#define TYPE_RANGE(Id, FirstId, LastId) \
  First_##Id##Type = FirstId, Last_##Id##Type = LastId,
#include "swift/AST/TypeNodes.def"
  };
  enum : unsigned { NumTypeKindBits =
    countBitsUsed(static_cast<unsigned>(TypeKind::Last_Type)) };
  
/// Various properties of types that are primarily defined recursively
/// on structural types.
class RecursiveTypeProperties {
public:
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
    
    /// Whether this type expression contains an unbound generic type.
    HasUnboundGeneric    = 0x10,

    /// This type expression contains an LValueType other than as a
    /// function input, and can be loaded to convert to an rvalue.
    IsLValue             = 0x20,

    /// This type expression contains an opened existential ArchetypeType.
    HasOpenedExistential = 0x40,

    /// This type expression contains a DynamicSelf type.
    HasDynamicSelf       = 0x80,

    /// This type contains an Error type.
    HasError             = 0x100,

    /// This type contains a DependentMemberType.
    HasDependentMember   = 0x200,

    Last_Property = HasDependentMember
  };
  enum { BitWidth = countBitsUsed(Property::Last_Property) };

private:
  unsigned Bits;

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
  
  /// Is a type with these properties an lvalue?
  bool isLValue() const { return Bits & IsLValue; }

  /// Does this type contain an error?
  bool hasError() const { return Bits & HasError; }

  /// Does this type contain a dependent member type, possibly with a
  /// non-type parameter base, such as a type variable or concrete type?
  bool hasDependentMember() const { return Bits & HasDependentMember; }

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

  /// Remove the HasDependentMember property from this set.
  void removeHasDependentMember() {
    Bits &= ~HasDependentMember;
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
enum class TypeMatchFlags {
  /// Allow properly-covariant overrides.
  AllowOverride = 1 << 0,
  /// Allow a parameter with IUO type to be overridden by a parameter with non-
  /// optional type.
  AllowNonOptionalForIUOParam = 1 << 1,
  /// Allow any mismatches of Optional or ImplicitlyUnwrappedOptional at the
  /// top level of a type.
  ///
  /// This includes function parameters and result types as well as tuple
  /// elements, but excludes generic parameters.
  AllowTopLevelOptionalMismatch = 1 << 2,
  /// Allow any ABI-compatible types to be considered matching.
  AllowABICompatible = 1 << 3,
  /// Allow escaping function parameters to override optional non-escaping ones.
  ///
  /// This is necessary because Objective-C allows optional function paramaters
  /// to be non-escaping, but Swift currently does not.
  IgnoreNonEscapingForOptionalFunctionParam = 1 << 4
};
using TypeMatchOptions = OptionSet<TypeMatchFlags>;

/// TypeBase - Base class for all types in Swift.
class alignas(1 << TypeAlignInBits) TypeBase {
  
  friend class ASTContext;
  TypeBase(const TypeBase&) = delete;
  void operator=(const TypeBase&) = delete;
  
  /// This union contains to the ASTContext for canonical types, and is
  /// otherwise lazily populated by ASTContext when the canonical form of a
  /// non-canonical type is requested. The disposition of the union is stored
  /// outside of the union for performance. See Bits.TypeBase.IsCanonical.
  union {
    CanType CanonicalType;
    const ASTContext *Context;
  };

  /// Returns true if the given type is a sugared type.
  ///
  /// Only intended for use in compile-time assertions.
  // Specializations of this are at the end of the file.
  template <typename T>
  static constexpr bool isSugaredType() {
    return false;
  }

protected:
  // SWIFT_ENABLE_TENSORFLOW
  enum { NumAFTExtInfoBits = 10 };
  enum { NumSILExtInfoBits = 9 };
  union { uint64_t OpaqueBits;

  SWIFT_INLINE_BITFIELD_BASE(TypeBase, bitmax(NumTypeKindBits,8) +
                             RecursiveTypeProperties::BitWidth + 1,
    /// Kind - The discriminator that indicates what subclass of type this is.
    Kind : bitmax(NumTypeKindBits,8),

    Properties : RecursiveTypeProperties::BitWidth,

    /// Whether this type is canonical or not.
    IsCanonical : 1
  );

  SWIFT_INLINE_BITFIELD(ErrorType, TypeBase, 1,
    /// Whether there is an original type.
    HasOriginalType : 1
  );

  SWIFT_INLINE_BITFIELD(SugarType, TypeBase, 1,
    HasCachedType : 1
  );

  enum { NumFlagBits = 8 };
  SWIFT_INLINE_BITFIELD(ParenType, SugarType, NumFlagBits,
    /// Whether there is an original type.
    Flags : NumFlagBits
  );

  SWIFT_INLINE_BITFIELD_FULL(AnyFunctionType, TypeBase, NumAFTExtInfoBits+16,
    /// Extra information which affects how the function is called, like
    /// regparm and the calling convention.
    ExtInfo : NumAFTExtInfoBits,

    : NumPadBits,
    NumParams : 16
  );

  SWIFT_INLINE_BITFIELD_FULL(ArchetypeType, TypeBase, 1+1+1+16,
    ExpandedNestedTypes : 1,
    HasSuperclass : 1,
    HasLayoutConstraint : 1,
    : NumPadBits,
    NumProtocols : 16
  );

  SWIFT_INLINE_BITFIELD_FULL(TypeVariableType, TypeBase, 64-NumTypeBaseBits,
    /// \brief The unique number assigned to this type variable.
    ID : 32 - NumTypeBaseBits,

    /// Type variable options.
    Options : 3,

    ///  Index into the list of type variables, as used by the
    ///  constraint graph.
    GraphIndex : 29
  );

  SWIFT_INLINE_BITFIELD(SILFunctionType, TypeBase, NumSILExtInfoBits+3+1+2,
    ExtInfo : NumSILExtInfoBits,
    CalleeConvention : 3,
    HasErrorResult : 1,
    CoroutineKind : 2
  );

  SWIFT_INLINE_BITFIELD(AnyMetatypeType, TypeBase, 2,
    /// The representation of the metatype.
    ///
    /// Zero indicates that no representation has been set; otherwise,
    /// the value is the representation + 1
    Representation : 2
  );

  SWIFT_INLINE_BITFIELD_FULL(ProtocolCompositionType, TypeBase, 1+32,
    /// Whether we have an explicitly-stated class constraint not
    /// implied by any of our members.
    HasExplicitAnyObject : 1,

    : NumPadBits,

    /// The number of protocols being composed.
    Count : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(TupleType, TypeBase, 1+32,
    /// Whether an element of the tuple is inout, __shared or __owned.
    /// Values cannot have such tuple types in the language.
    HasElementWithOwnership : 1,

    : NumPadBits,

    /// The number of elements of the tuple.
    Count : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(BoundGenericType, TypeBase, 32,
    : NumPadBits,

    /// The number of generic arguments.
    GenericArgCount : 32
  );

  SWIFT_INLINE_BITFIELD_FULL(NameAliasType, SugarType, 1+1,
    : NumPadBits,

    /// Whether we have a parent type.
    HasParent : 1,

    /// Whether we have a substitution map.
    HasSubstitutionMap : 1
  );

  } Bits;

protected:
  TypeBase(TypeKind kind, const ASTContext *CanTypeCtx,
           RecursiveTypeProperties properties)
    : Context(nullptr) {
    Bits.OpaqueBits = 0;
    Bits.TypeBase.Kind = static_cast<unsigned>(kind);
    Bits.TypeBase.IsCanonical = false;
    // If this type is canonical, switch the CanonicalType union to ASTContext.
    if (CanTypeCtx) {
      Bits.TypeBase.IsCanonical = true;
      Context = CanTypeCtx;
    }
    setRecursiveProperties(properties);
  }

  void setRecursiveProperties(RecursiveTypeProperties properties) {
    Bits.TypeBase.Properties = properties.getBits();
    assert(Bits.TypeBase.Properties == properties.getBits() && "Bits dropped!");
  }

public:
  /// getKind - Return what kind of type this is.
  TypeKind getKind() const { return static_cast<TypeKind>(Bits.TypeBase.Kind); }

  /// isCanonical - Return true if this is a canonical type.
  bool isCanonical() const { return Bits.TypeBase.IsCanonical; }
  
  /// hasCanonicalTypeComputed - Return true if we've already computed a
  /// canonical version of this type.
  bool hasCanonicalTypeComputed() const { return !CanonicalType.isNull(); }

private:
  CanType computeCanonicalType();

public:
  /// getCanonicalType - Return the canonical version of this type, which has
  /// sugar from all levels stripped off.
  CanType getCanonicalType() const {
    if (isCanonical())
      return CanType(const_cast<TypeBase*>(this));
    if (hasCanonicalTypeComputed())
      return CanonicalType;
    return const_cast<TypeBase*>(this)->computeCanonicalType();
  }

  /// getCanonicalType - Stronger canonicalization which folds away equivalent
  /// associated types, or type parameters that have been made concrete.
  CanType getCanonicalType(GenericSignature *sig);

  /// Reconstitute type sugar, e.g., for array types, dictionary
  /// types, optionals, etc.
  TypeBase *reconstituteSugar(bool Recursive);

  /// getASTContext - Return the ASTContext that this type belongs to.
  ASTContext &getASTContext() {
    // If this type is canonical, it has the ASTContext in it.
    if (isCanonical())
      return *const_cast<ASTContext*>(Context);
    // If not, canonicalize it to get the Context.
    return *const_cast<ASTContext*>(getCanonicalType()->Context);
  }
  
  /// isEqual - Return true if these two types are equal, ignoring sugar.
  ///
  /// To compare sugar, check for pointer equality of the underlying TypeBase *
  /// values, obtained by calling getPointer().
  bool isEqual(Type Other);
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();
  
  /// If this type is a (potentially sugared) type of the specified kind, remove
  /// the minimal amount of sugar required to get a pointer to the type.
  template <typename T>
  T *getAs() {
    static_assert(!isSugaredType<T>(), "getAs desugars types");
    auto Ty = getDesugaredType();
    SWIFT_ASSUME(Ty != nullptr);
    return dyn_cast<T>(Ty);
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
    return RecursiveTypeProperties(Bits.TypeBase.Properties);
  }

  /// hasReferenceSemantics() - Do objects of this type have reference
  /// semantics?
  bool hasReferenceSemantics();

  /// Is this a nominally uninhabited type, such as 'Never'?
  bool isUninhabited();

  /// Is this an uninhabited type, such as 'Never' or '(Never, Int)'?
  bool isStructurallyUninhabited();
  
  /// Is this the 'Any' type?
  bool isAny();

  /// Does the type have outer parenthesis?
  bool hasParenSugar() const { return getKind() == TypeKind::Paren; }

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
  bool isOpenedExistential() const;

  /// Determine whether the type is an opened existential type with Error inside
  bool isOpenedExistentialWithError();

  /// Retrieve the set of opened existential archetypes that occur
  /// within this type.
  void getOpenedExistentials(SmallVectorImpl<ArchetypeType *> &opened);

  /// Erase the given opened existential type by replacing it with its
  /// existential type throughout the given type.
  Type eraseOpenedExistential(ArchetypeType *opened);

  /// Erase DynamicSelfType from the given type by replacing it with its
  /// underlying type.
  Type eraseDynamicSelfType();

  /// Given a declaration context, returns a function type with the 'self'
  /// type curried as the input if the declaration context describes a type.
  /// Otherwise, returns the type itself.
  Type addCurriedSelfType(const DeclContext *dc);

  /// Map a contextual type to an interface type.
  Type mapTypeOutOfContext();

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

  /// \brief Determine whether this type can dynamically be an optional type.
  ///
  /// \param includeExistential Whether an existential type should be considered
  /// such a type.
  bool canDynamicallyBeOptionalType(bool includeExistential);

  /// Determine whether this type contains a type parameter somewhere in it.
  bool hasTypeParameter() {
    return getRecursiveProperties().hasTypeParameter();
  }

  /// Find any unresolved dependent member type within this type.
  ///
  /// "Unresolved" dependent member types have no known associated type,
  /// and are only used transiently in the type checker.
  const DependentMemberType *findUnresolvedDependentMemberType();

  /// Return the root generic parameter of this type parameter type.
  GenericTypeParamType *getRootGenericParam();

  /// Determines whether this type is an lvalue. This includes both straight
  /// lvalue types as well as tuples or optionals of lvalues.
  bool hasLValueType() {
    return getRecursiveProperties().isLValue();
  }
  
  /// Is a type with these properties materializable: that is, is it a
  /// first-class value type?
  bool isMaterializable();

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

  /// Does this type contain a dependent member type, possibly with a
  /// non-type parameter base, such as a type variable or concrete type?
  bool hasDependentMember() const {
    return getRecursiveProperties().hasDependentMember();
  }

  /// \brief Check if this type is a valid type for the LHS of an assignment.
  /// This mainly means hasLValueType(), but empty tuples and tuples of empty
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

  /// Opens an existential instance or meta-type and returns the opened type.
  Type openAnyExistentialType(ArchetypeType *&opened);

  /// Break an existential down into a set of constraints.
  ExistentialLayout getExistentialLayout();

  /// Determines the element type of a known
  /// [Autoreleasing]Unsafe[Mutable][Raw]Pointer variant, or returns null if the
  /// type is not a pointer.
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

  /// \brief Determine whether this type is a legal, lowered SIL type.
  ///
  /// A type is SIL-illegal if it is:
  ///   - an l-value type,
  ///   - a metatype without a representation,
  ///   - an AST function type (i.e. subclasses of AnyFunctionType),
  ///   - an optional whose object type is SIL-illegal, or
  ///   - a tuple type with a SIL-illegal element type.
  bool isLegalSILType();

  /// \brief Determine whether this type is a legal formal type.
  ///
  /// A type is illegal as a formal type if it is:
  ///   - an l-value type,
  ///   - a reference storage type,
  ///   - a metatype with a representation,
  ///   - a lowered function type (i.e. SILFunctionType),
  ///   - an optional whose object type is not a formal type, or
  ///   - a tuple type with an element that is not a formal type.
  ///
  /// These are the types of the Swift type system.
  bool isLegalFormalType();

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

  /// Determine whether this type satisfies a class layout constraint, written
  /// `T: AnyObject` in the source.
  ///
  /// A class layout constraint is satisfied when we have a single retainable
  /// pointer as the representation, which includes:
  /// - @objc existentials
  /// - class constrained archetypes
  /// - classes
  bool satisfiesClassConstraint();

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
  /// \param useArchetypes Whether to use context archetypes for outer generic
  /// parameters if the class is nested inside a generic function.
  ///
  /// \returns The superclass of this type, or a null type if it has no
  ///          superclass.
  Type getSuperclass(bool useArchetypes = true);
  
  /// \brief True if this type is the exact superclass of another type.
  ///
  /// \param ty       The potential subclass.
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
  bool isExactSuperclassOf(Type ty);

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
  ///
  /// \param useArchetypes Whether to use context archetypes for outer generic
  /// parameters if the class is nested inside a generic function.
  Type getSuperclassForDecl(const ClassDecl *classDecl,
                            bool useArchetypes = true);

  /// \brief True if this type is the superclass of another type, or a generic
  /// type that could be bound to the superclass.
  ///
  /// \param ty       The potential subclass.
  ///
  /// \returns True if this type is \c ty, a superclass of \c ty, or an
  ///          archetype-parameterized type that can be bound to a superclass
  ///          of \c ty.
  bool isBindableToSuperclassOf(Type ty);

  /// True if this type contains archetypes that could be substituted with
  /// concrete types to form the argument type.
  bool isBindableTo(Type ty);

  /// \brief Determines whether this type is similar to \p other as defined by
  /// \p matchOptions.
  bool matches(Type other, TypeMatchOptions matchOptions);

  bool matchesParameter(Type other, TypeMatchOptions matchMode);

  /// \brief Determines whether this function type is similar to \p
  /// other as defined by \p matchOptions and the callback \p
  /// paramsAndResultMatch which determines in a client-specific way
  /// whether the parameters and result of the types match.
  bool matchesFunctionType(Type other, TypeMatchOptions matchOptions,
                           llvm::function_ref<bool()> paramsAndResultMatch);

  /// \brief Determines whether this type has a retainable pointer
  /// representation, i.e. whether it is representable as a single,
  /// possibly nil pointer that can be unknown-retained and
  /// unknown-released.
  bool hasRetainablePointerRepresentation();

  /// Given that this type is a reference type, which kind of reference
  /// counting does it use?
  ReferenceCounting getReferenceCounting();

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

  /// removeArgumentLabels -  Retrieve a version of this type with all
  /// argument labels removed.
  Type removeArgumentLabels(unsigned numArgumentLabels);

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

  /// getInOutObjectType - For an inout type, retrieves the underlying object
  /// type.  Otherwise, returns the type itself.
  Type getInOutObjectType();

  /// getWithoutSpecifierType - For a non-materializable type
  /// e.g. @lvalue or inout, retrieves the underlying object type.
  /// Otherwise, returns the type itself.
  Type getWithoutSpecifierType();

  /// getMetatypeInstanceType - Looks through metatypes.
  Type getMetatypeInstanceType();

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
  ///
  /// \param genericEnv If non-null and the type is nested inside of a
  /// generic function, generic parameters of the outer context are
  /// mapped to context archetypes of this generic environment.
  SubstitutionMap getContextSubstitutionMap(ModuleDecl *module,
                                            const DeclContext *dc,
                                            GenericEnvironment *genericEnv=nullptr);

  /// Deprecated version of the above.
  TypeSubstitutionMap getContextSubstitutions(const DeclContext *dc,
                                              GenericEnvironment *genericEnv=nullptr);

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
                                      Type memberType);

  /// Return T if this type is Optional<T>; otherwise, return the null type.
  Type getOptionalObjectType();

  // Return type underlying type of a swift_newtype annotated imported struct;
  // otherwise, return the null type.
  Type getSwiftNewtypeUnderlyingType();

  /// Return the type T after looking through all of the optional
  /// types.
  Type lookThroughAllOptionalTypes();

  /// Return the type T after looking through all of the optional
  /// types.
  Type lookThroughAllOptionalTypes(SmallVectorImpl<Type> &optionals);

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

  /// Does this type have grammatically simple syntax?
  bool hasSimpleTypeRepr() const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;

  /// Return the name of the type, adding parens in cases where
  /// appending or prepending text to the result would cause that text
  /// to be appended to only a portion of the returned type. For
  /// example for a function type "Int -> Float", adding text after
  /// the type would make it appear that it's appended to "Float" as
  /// opposed to the entire type.
  std::string
  getStringAsComponent(const PrintOptions &PO = PrintOptions()) const;

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

/// AnyGenericType - This abstract class helps types ensure that fields
/// exist at the same offset in memory to improve code generation of the
/// compiler itself.
class AnyGenericType : public TypeBase {
  friend class NominalOrBoundGenericNominalType;

  /// TheDecl - This is the TypeDecl which declares the given type. It
  /// specifies the name and other useful information about this type.
  union {
    GenericTypeDecl *GenDecl;
    NominalTypeDecl *NomDecl;
  };

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

  template <typename... Args>
  AnyGenericType(NominalTypeDecl *TheDecl, Type Parent, Args &&...args)
    : TypeBase(std::forward<Args>(args)...), NomDecl(TheDecl), Parent(Parent) {}

protected:
  template <typename... Args>
  AnyGenericType(GenericTypeDecl *TheDecl, Type Parent, Args &&...args)
    : TypeBase(std::forward<Args>(args)...), GenDecl(TheDecl), Parent(Parent) {}

public:

  /// \brief Returns the declaration that declares this type.
  GenericTypeDecl *getDecl() const { return GenDecl; }

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
    return T->getKind() >= TypeKind::First_AnyGenericType &&
           T->getKind() <= TypeKind::Last_AnyGenericType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(AnyGenericType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getParent)
END_CAN_TYPE_WRAPPER(AnyGenericType, Type)

/// NominalOrBoundGenericNominal - This abstract class helps types ensure that
/// fields exist at the same offset in memory to improve code generation of the
/// compiler itself.
class NominalOrBoundGenericNominalType : public AnyGenericType {
public:
  template <typename... Args>
  NominalOrBoundGenericNominalType(Args &&...args)
    : AnyGenericType(std::forward<Args>(args)...) {}

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return NomDecl; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_NominalOrBoundGenericNominalType &&
           T->getKind() <= TypeKind::Last_NominalOrBoundGenericNominalType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(NominalOrBoundGenericNominalType, AnyGenericType)

/// ErrorType - This represents a type that was erroneously constructed.  This
/// is produced when parsing types and when name binding type aliases, and is
/// installed in declaration that use these erroneous types.  All uses of a
/// declaration of invalid type should be ignored and not re-diagnosed.
class ErrorType final : public TypeBase {
  friend class ASTContext;
  // The Error type is always canonical.
  ErrorType(ASTContext &C, Type originalType,
            RecursiveTypeProperties properties)
      : TypeBase(TypeKind::Error, &C, properties) {
    assert(properties.hasError());
    if (originalType) {
      Bits.ErrorType.HasOriginalType = true;
      *reinterpret_cast<Type *>(this + 1) = originalType;
    } else {
      Bits.ErrorType.HasOriginalType = false;
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
    if (Bits.ErrorType.HasOriginalType)
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
  
/// An abstract type for all sugared types to make getDesugaredType() fast by
/// sharing field offsets and logic for the fast path.
class SugarType : public TypeBase {
  // The state of this union is known via Bits.SugarType.HasCachedType so that
  // we can avoid masking the pointer on the fast path.
  union {
    TypeBase *UnderlyingType;
    const ASTContext *Context;
  };

protected:
  // Sugar types are never canonical.
  SugarType(TypeKind K, const ASTContext *ctx,
            RecursiveTypeProperties properties)
      : TypeBase(K, nullptr, properties), Context(ctx) {
    assert(ctx != nullptr &&
           "Context for SugarType should not be null");
    Bits.SugarType.HasCachedType = false;
  }

  // Sugar types are never canonical.
  SugarType(TypeKind K, Type type, RecursiveTypeProperties properties)
      : TypeBase(K, nullptr, properties), UnderlyingType(type.getPointer()) {
    Bits.SugarType.HasCachedType = true;
  }

  void setUnderlyingType(Type type) {
    assert(!Bits.SugarType.HasCachedType && "Cached type already set");
    Bits.SugarType.HasCachedType = true;
    UnderlyingType = type.getPointer();
  }

public:
  /// Remove one level of top-level sugar from this type.
  Type getSinglyDesugaredTypeSlow();
  TypeBase *getSinglyDesugaredType() const {
    if (LLVM_LIKELY(Bits.SugarType.HasCachedType))
      return UnderlyingType;
    auto Ty = const_cast<SugarType*>(this);
    return Ty->getSinglyDesugaredTypeSlow().getPointer();
  }

  static bool classof(const TypeBase *T) {
    // Workaround: http://llvm.org/PR35906
    if (TypeKind::Last_Type == TypeKind::Last_SugarType)
      return T->getKind() >= TypeKind::First_SugarType;
    return T->getKind() >= TypeKind::First_SugarType &&
           T->getKind() <= TypeKind::Last_SugarType;
  }
};

/// A reference to a type alias that is somehow generic, along with the
/// set of substitutions to apply to make the type concrete.
class NameAliasType final
  : public SugarType, public llvm::FoldingSetNode,
    llvm::TrailingObjects<NameAliasType, Type, SubstitutionMap>
{
  TypeAliasDecl *typealias;

  friend class ASTContext;
  friend TrailingObjects;

  NameAliasType(TypeAliasDecl *typealias, Type parent,
                SubstitutionMap substitutions, Type underlying,
                RecursiveTypeProperties properties);

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return Bits.NameAliasType.HasParent ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<SubstitutionMap>) const {
    return Bits.NameAliasType.HasSubstitutionMap ? 1 : 0;
  }

public:
  /// Retrieve the generic signature used for substitutions.
  GenericSignature *getGenericSignature() const {
    return getSubstitutionMap().getGenericSignature();
  }

  static NameAliasType *get(TypeAliasDecl *typealias, Type parent,
                            SubstitutionMap substitutions, Type underlying);

  /// \brief Returns the declaration that declares this type.
  TypeAliasDecl *getDecl() const {
    // Avoid requiring the definition of TypeAliasDecl.
    return typealias;
  }

  /// Retrieve the parent of this type as written, e.g., the part that was
  /// written before ".", if provided.
  Type getParent() const {
    return Bits.NameAliasType.HasParent ? *getTrailingObjects<Type>()
                                        : Type();
  }

  /// Retrieve the substitution map applied to the declaration's underlying
  /// to produce the described type.
  SubstitutionMap getSubstitutionMap() const {
    if (!Bits.NameAliasType.HasSubstitutionMap)
      return SubstitutionMap();

    return *getTrailingObjects<SubstitutionMap>();
  }

  /// Get the innermost generic arguments, which correspond to the generic
  /// arguments that are directly applied to the typealias declaration in
  /// produced by \c getDecl().
  ///
  /// The result can be empty, if the declaration itself is non-generic but
  /// the parent is generic.
  SmallVector<Type, 2> getInnermostGenericArgs() const;

  // Support for FoldingSet.
  void Profile(llvm::FoldingSetNodeID &id) const;

  static void Profile(llvm::FoldingSetNodeID &id, TypeAliasDecl *typealias,
                      Type parent, SubstitutionMap substitutions,
                      Type underlying);

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
    None        = 0,
    Variadic    = 1 << 0,
    AutoClosure = 1 << 1,
    Escaping    = 1 << 2,
    OwnershipShift = 3,
    Ownership   = 7 << OwnershipShift,
    // SWIFT_ENABLE_TENSORFLOW
    NonDifferentiable = 1 << 6,
    NumBits = 7
  };
  OptionSet<ParameterFlags> value;
  static_assert(NumBits < 8*sizeof(OptionSet<ParameterFlags>), "overflowed");

  ParameterTypeFlags(OptionSet<ParameterFlags, uint8_t> val) : value(val) {}

public:
  ParameterTypeFlags() = default;
  static ParameterTypeFlags fromRaw(uint8_t raw) {
    return ParameterTypeFlags(OptionSet<ParameterFlags>(raw));
  }

  ParameterTypeFlags(bool variadic, bool autoclosure, bool escaping,
                     // SWIFT_ENABLE_TENSORFLOW
                     ValueOwnership ownership, bool nonDifferentiable)
      : value((variadic ? Variadic : 0) | (autoclosure ? AutoClosure : 0) |
              (escaping ? Escaping : 0) |
              // SWIFT_ENABLE_TENSORFLOW
              (uint8_t(ownership) << OwnershipShift) |
              (nonDifferentiable ? NonDifferentiable : 0)) {}

  /// Create one from what's present in the parameter type
  inline static ParameterTypeFlags
  // SWIFT_ENABLE_TENSORFLOW
  fromParameterType(Type paramTy, bool isVariadic, ValueOwnership ownership,
                    bool isNonDifferentiable);

  bool isNone() const { return !value; }
  bool isVariadic() const { return value.contains(Variadic); }
  bool isAutoClosure() const { return value.contains(AutoClosure); }
  bool isEscaping() const { return value.contains(Escaping); }
  bool isInOut() const { return getValueOwnership() == ValueOwnership::InOut; }
  bool isShared() const { return getValueOwnership() == ValueOwnership::Shared;}
  bool isOwned() const { return getValueOwnership() == ValueOwnership::Owned; }
  // SWIFT_ENABLE_TENSORFLOW
  bool isNonDifferentiable() const { return value.contains(NonDifferentiable); }

  ValueOwnership getValueOwnership() const {
    return ValueOwnership((value.toRaw() & Ownership) >> OwnershipShift);
  }

  ParameterTypeFlags withVariadic(bool variadic) const {
    return ParameterTypeFlags(variadic ? value | ParameterTypeFlags::Variadic
                                       : value - ParameterTypeFlags::Variadic);
  }

  ParameterTypeFlags withEscaping(bool escaping) const {
    return ParameterTypeFlags(escaping ? value | ParameterTypeFlags::Escaping
                                       : value - ParameterTypeFlags::Escaping);
  }

  ParameterTypeFlags withInOut(bool isInout) const {
    return withValueOwnership(isInout ? ValueOwnership::InOut
                                      : ValueOwnership::Default);
  }
  
  ParameterTypeFlags withShared(bool isShared) const {
    return withValueOwnership(isShared ? ValueOwnership::Shared
                                       : ValueOwnership::Default);
  }

  ParameterTypeFlags withOwned(bool isOwned) const {
    return withValueOwnership(isOwned ? ValueOwnership::Owned
                                      : ValueOwnership::Default);
  }

  ParameterTypeFlags withValueOwnership(ValueOwnership ownership) const {
    return (value - ParameterTypeFlags::Ownership)
            | ParameterFlags(uint8_t(ownership) << OwnershipShift);
  }

  // SWIFT_ENABLE_TENSORFLOW
  ParameterTypeFlags withNonDifferentiable(bool nonDifferentiable) const {
    return ParameterTypeFlags(nonDifferentiable
                              ? value | ParameterTypeFlags::NonDifferentiable
                              : value - ParameterTypeFlags::NonDifferentiable);
  }

  bool operator ==(const ParameterTypeFlags &other) const {
    return value.toRaw() == other.value.toRaw();
  }

  bool operator!=(const ParameterTypeFlags &other) const {
    return value.toRaw() != other.value.toRaw();
  }

  uint8_t toRaw() const { return value.toRaw(); }
};

class YieldTypeFlags {
  enum YieldFlags : uint8_t {
    None        = 0,
    Ownership   = 7,
    OwnershipShift = 0,

    NumBits = 3
  };
  OptionSet<YieldFlags> value;

  static_assert(NumBits < 8 * sizeof(OptionSet<YieldFlags>), "overflowed");

  YieldTypeFlags(OptionSet<YieldFlags, uint8_t> val) : value(val) {}

public:
  YieldTypeFlags() = default;
  static YieldTypeFlags fromRaw(uint8_t raw) {
    return YieldTypeFlags(OptionSet<YieldFlags>(raw));
  }

  YieldTypeFlags(ValueOwnership ownership)
      : value(uint8_t(ownership) << OwnershipShift) {}

  bool isInOut() const { return getValueOwnership() == ValueOwnership::InOut; }
  bool isShared() const { return getValueOwnership() == ValueOwnership::Shared;}
  bool isOwned() const { return getValueOwnership() == ValueOwnership::Owned; }

  ValueOwnership getValueOwnership() const {
    return ValueOwnership((value.toRaw() & Ownership) >> OwnershipShift);
  }

  YieldTypeFlags withInOut(bool isInout) const {
    return withValueOwnership(isInout ? ValueOwnership::InOut
                                      : ValueOwnership::Default);
  }
  
  YieldTypeFlags withShared(bool isShared) const {
    return withValueOwnership(isShared ? ValueOwnership::Shared
                                       : ValueOwnership::Default);
  }

  YieldTypeFlags withOwned(bool isOwned) const {
    return withValueOwnership(isOwned ? ValueOwnership::Owned
                                      : ValueOwnership::Default);
  }

  YieldTypeFlags withValueOwnership(ValueOwnership ownership) const {
    return (value - YieldTypeFlags::Ownership)
            | YieldFlags(uint8_t(ownership) << OwnershipShift);
  }

  /// Return these flags interpreted as parameter flags.
  ParameterTypeFlags asParamFlags() const {
    return ParameterTypeFlags(/*variadic*/ false,
                              /*autoclosure*/ false,
                              /*escaping*/ false,
                              // SWIFT_ENABLE_TENSORFLOW
                              getValueOwnership(),
                              /*nondifferentiable*/ false);
  }

  bool operator ==(const YieldTypeFlags &other) const {
    return value.toRaw() == other.value.toRaw();
  }

  bool operator!=(const YieldTypeFlags &other) const {
    return value.toRaw() != other.value.toRaw();
  }

  uint8_t toRaw() const { return value.toRaw(); }
};

/// ParenType - A paren type is a type that's been written in parentheses.
class ParenType : public SugarType {
  friend class ASTContext;
  
  ParenType(Type UnderlyingType, RecursiveTypeProperties properties,
            ParameterTypeFlags flags);

public:
  Type getUnderlyingType() const { return getSinglyDesugaredType(); }

  static ParenType *get(const ASTContext &C, Type underlying,
                        ParameterTypeFlags flags = {});

  /// Get the parameter flags
  ParameterTypeFlags getParameterFlags() const {
    return ParameterTypeFlags::fromRaw(Bits.ParenType.Flags);
  }

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
  TupleTypeElt(Type ty, Identifier name = Identifier(),
               ParameterTypeFlags fl = {});
  
  bool hasName() const { return !Name.empty(); }
  Identifier getName() const { return Name; }
  
  Type getRawType() const { return ElementType; }
  Type getType() const;

  ParameterTypeFlags getParameterFlags() const { return Flags; }

  /// Determine whether this field is variadic.
  bool isVararg() const { return Flags.isVariadic(); }

  /// Determine whether this field is an autoclosure parameter closure.
  bool isAutoClosure() const { return Flags.isAutoClosure(); }

  /// Determine whether this field is an escaping parameter closure.
  bool isEscaping() const { return Flags.isEscaping(); }
  
  /// Determine whether this field is marked 'inout'.
  bool isInOut() const { return Flags.isInOut(); }
  
  /// Remove the type of this varargs element designator, without the array
  /// type wrapping it.
  Type getVarargBaseTy() const;
  
  /// Retrieve a copy of this tuple type element with the type replaced.
  TupleTypeElt getWithType(Type T) const;

  /// Retrieve a copy of this tuple type element with the name replaced.
  TupleTypeElt getWithName(Identifier name) const;

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
class TupleType final : public TypeBase, public llvm::FoldingSetNode,
    private llvm::TrailingObjects<TupleType, TupleTypeElt> {
  friend TrailingObjects;
  
public:
  /// get - Return the uniqued tuple type with the specified elements.
  /// Returns a ParenType instead if there is exactly one element which
  /// is unlabeled and not varargs, so it doesn't accidentally construct
  /// a tuple which is impossible to write.
  static Type get(ArrayRef<TupleTypeElt> Elements, const ASTContext &C);

  /// getEmpty - Return the empty tuple type '()'.
  static CanTypeWrapper<TupleType> getEmpty(const ASTContext &C);

  unsigned getNumElements() const { return Bits.TupleType.Count; }

  /// getElements - Return the elements of this tuple.
  ArrayRef<TupleTypeElt> getElements() const {
    return {getTrailingObjects<TupleTypeElt>(), getNumElements()};
  }

  const TupleTypeElt &getElement(unsigned i) const {
    return getTrailingObjects<TupleTypeElt>()[i];
  }

  /// getElementType - Return the type of the specified element.
  Type getElementType(unsigned ElementNo) const {
    return getTrailingObjects<TupleTypeElt>()[ElementNo].getType();
  }

  TupleEltTypeArrayRef getElementTypes() const {
    return TupleEltTypeArrayRef(getElements());
  }
  
  /// getNamedElementId - If this tuple has an element with the specified name,
  /// return the element index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// Returns true if this tuple has inout, __shared or __owned elements.
  bool hasElementWithOwnership() const {
    return static_cast<bool>(Bits.TupleType.HasElementWithOwnership);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Tuple;
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getElements());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      ArrayRef<TupleTypeElt> Elements);
  
private:
  TupleType(ArrayRef<TupleTypeElt> elements, const ASTContext *CanCtx,
            RecursiveTypeProperties properties,
            bool hasElementWithOwnership)
     : TypeBase(TypeKind::Tuple, CanCtx, properties) {
     Bits.TupleType.HasElementWithOwnership = hasElementWithOwnership;
     Bits.TupleType.Count = elements.size();
     std::uninitialized_copy(elements.begin(), elements.end(),
                             getTrailingObjects<TupleTypeElt>());
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
class UnboundGenericType : public AnyGenericType,
    public llvm::FoldingSetNode {
private:
  UnboundGenericType(GenericTypeDecl *TheDecl, Type Parent, const ASTContext &C,
                     RecursiveTypeProperties properties)
    : AnyGenericType(TheDecl, Parent, TypeKind::UnboundGeneric,
                     (!Parent || Parent->isCanonical()) ? &C : nullptr,
                     properties | RecursiveTypeProperties::HasUnboundGeneric) {}

public:
  static UnboundGenericType* get(GenericTypeDecl *TheDecl, Type Parent,
                                 const ASTContext &C);

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
DEFINE_EMPTY_CAN_TYPE_WRAPPER(UnboundGenericType, AnyGenericType)

inline CanType getAsCanType(const Type &type) { return CanType(type); }
typedef ArrayRefView<Type,CanType,getAsCanType> CanTypeArrayRef;

/// BoundGenericType - An abstract class for applying a generic type to the
/// given type arguments.
class BoundGenericType : public NominalOrBoundGenericNominalType,
    public llvm::FoldingSetNode {
  
  /// Retrieve the intrusive pointer storage from the subtype
  const Type *getTrailingObjectsPointer() const;
  Type *getTrailingObjectsPointer() {
    const BoundGenericType *temp = this;
    return const_cast<Type *>(temp->getTrailingObjectsPointer());
  }

protected:
  BoundGenericType(TypeKind theKind, NominalTypeDecl *theDecl, Type parent,
                   ArrayRef<Type> genericArgs, const ASTContext *context,
                   RecursiveTypeProperties properties);

public:
  static BoundGenericType* get(NominalTypeDecl *TheDecl, Type Parent,
                               ArrayRef<Type> GenericArgs);

  /// Retrieve the set of generic arguments provided at this level.
  ArrayRef<Type> getGenericArgs() const {
    return {getTrailingObjectsPointer(), Bits.BoundGenericType.GenericArgCount};
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getDecl(), getParent(), getGenericArgs());
  }
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *TheDecl,
                      Type Parent, ArrayRef<Type> GenericArgs);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_BoundGenericType &&
           T->getKind() <= TypeKind::Last_BoundGenericType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(BoundGenericType, NominalOrBoundGenericNominalType)
  CanTypeArrayRef getGenericArgs() const {
    return CanTypeArrayRef(getPointer()->getGenericArgs());
  }
END_CAN_TYPE_WRAPPER(BoundGenericType, NominalOrBoundGenericNominalType)


/// BoundGenericClassType - A subclass of BoundGenericType for the case
/// when the nominal type is a generic class type.
class BoundGenericClassType final : public BoundGenericType,
    private llvm::TrailingObjects<BoundGenericClassType, Type> {
  friend TrailingObjects;

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
class BoundGenericEnumType final : public BoundGenericType,
    private llvm::TrailingObjects<BoundGenericEnumType, Type> {
  friend TrailingObjects;

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
class BoundGenericStructType final : public BoundGenericType,
    private llvm::TrailingObjects<BoundGenericStructType, Type> {
  friend TrailingObjects;

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
class NominalType : public NominalOrBoundGenericNominalType {

protected:
  NominalType(TypeKind K, const ASTContext *C, NominalTypeDecl *TheDecl,
              Type Parent, RecursiveTypeProperties properties)
    : NominalOrBoundGenericNominalType(TheDecl, Parent, K,
               (!Parent || Parent->isCanonical())? C : nullptr, properties) {}

public:
  static NominalType *get(NominalTypeDecl *D, Type Parent, const ASTContext &C);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_NominalType &&
           T->getKind() <= TypeKind::Last_NominalType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(NominalType, NominalOrBoundGenericNominalType)

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
    return Bits.AnyMetatypeType.Representation > 0;
  }
  
  /// Retrieve the metatype representation.
  ///
  /// The metatype representation is a SIL-only property. Thin
  /// metatypes can be lowered away to empty types in IR, unless a
  /// metatype value is required at an abstraction level.
  MetatypeRepresentation getRepresentation() const {
    assert(Bits.AnyMetatypeType.Representation &&
           "metatype has no representation");
    return static_cast<MetatypeRepresentation>(
             Bits.AnyMetatypeType.Representation - 1);
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
enum class SILFunctionLanguage : uint8_t {
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

  // SWIFT_ENABLE_TENSORFLOW
  /// A function that will be promoted to a TensorFlow Graph.
  TensorFlow,

  /// The value of the greatest AST function representation.
  Last = TensorFlow,
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

  // SWIFT_ENABLE_TENSORFLOW
  /// A TensorFlow function pointer.
  TensorFlow = uint8_t(FunctionTypeRepresentation::TensorFlow),

  /// The value of the greatest AST function representation.
  LastAST = TensorFlow,

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
  // SWIFT_ENABLE_TENSORFLOW
  case SILFunctionTypeRepresentation::TensorFlow:
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
  // SWIFT_ENABLE_TENSORFLOW
  case SILFunctionTypeRepresentation::TensorFlow:
    return SILFunctionLanguage::Swift;
  }

  llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
}

// SWIFT_ENABLE_TENSORFLOW
/// The differentiability of a function type.
enum class FunctionTypeDifferentiability : uint8_t {
  /// Non-differentiable.
  None = 0,
  /// Forward-mode differentiable.
  Forward,
  /// Reverse-mode differentiable.
  Reverse,
  /// Both forward-mode and reverse-mode differentiable.
  Bidirectional,
  /// Linear map.
  Linear,
  /// Constant function, whose derivatives are always zero.
  Constant,
};

/// AnyFunctionType - A function type has zero or more input parameters and a
/// single result. The result type may be a tuple. For example:
///   "(int) -> int" or "(a : int, b : int) -> (int, int)".
///
/// There are two kinds of function types:  monomorphic (FunctionType) and
/// polymorphic (GenericFunctionType). Both type families additionally can
/// be 'thin', indicating that a function value has no capture context and can be
/// represented at the binary level as a single function pointer.
class AnyFunctionType : public TypeBase {
  const Type Output;
  
public:
  using Representation = FunctionTypeRepresentation;
  // SWIFT_ENABLE_TENSORFLOW
  using Differentiability = FunctionTypeDifferentiability;

  class Param {
  public:
    explicit Param(Type t,
                   Identifier l = Identifier(),
                   ParameterTypeFlags f = ParameterTypeFlags())
        : Ty(t), Label(l), Flags(f) {
      assert(!t || !t->is<InOutType>() && "set flags instead");
    }

  private:
    /// The type of the parameter. For a variadic parameter, this is the
    /// element type.
    Type Ty;
    
    // The label associated with the parameter, if any.
    Identifier Label;
    
    /// Parameter specific flags.
    ParameterTypeFlags Flags = {};
    
  public:
    /// FIXME: Remove this. Return the formal type of the parameter in the
    /// function type, including the InOutType if there is one.
    ///
    /// For example, 'inout Int' => 'inout Int', 'Int...' => 'Int'.
    Type getOldType() const;

    /// Return the formal type of the parameter.
    ///
    /// For example, 'inout Int' => 'Int', 'Int...' => 'Int'.
    Type getPlainType() const { return Ty; }

    /// The type of the parameter when referenced inside the function body
    /// as an rvalue.
    ///
    /// For example, 'inout Int' => 'Int', 'Int...' => '[Int]'.
    Type getParameterType(bool forCanonical = false,
                          ASTContext *ctx = nullptr) const;

    bool hasLabel() const { return !Label.empty(); }
    Identifier getLabel() const { return Label; }
    
    ParameterTypeFlags getParameterFlags() const { return Flags; }

    /// Whether the parameter is varargs
    bool isVariadic() const { return Flags.isVariadic(); }
    
    /// Whether the parameter is marked '@autoclosure'
    bool isAutoClosure() const { return Flags.isAutoClosure(); }
    
    /// Whether the parameter is marked '@escaping'
    bool isEscaping() const { return Flags.isEscaping(); }
    
    /// Whether the parameter is marked 'inout'
    bool isInOut() const { return Flags.isInOut(); }
    
    /// Whether the parameter is marked 'shared'
    bool isShared() const { return Flags.isShared(); }

    /// Whether the parameter is marked 'owned'
    bool isOwned() const { return Flags.isOwned(); }

    ValueOwnership getValueOwnership() const {
      return Flags.getValueOwnership();
    }

    bool operator==(Param const &b) const {
      return (Label == b.Label &&
              getPlainType()->isEqual(b.getPlainType()) &&
              Flags == b.Flags);
    }
    bool operator!=(Param const &b) const { return !(*this == b); }

    Param getWithoutLabel() const { return Param(Ty, Identifier(), Flags); }
  };

  class CanParam : public Param {
    explicit CanParam(const Param &param) : Param(param) {}
  public:
    static CanParam getFromParam(const Param &param) { return CanParam(param); }

    CanType getOldType() const { return CanType(Param::getOldType()); }
    CanType getPlainType() const { return CanType(Param::getPlainType()); }
    CanType getParameterType() const {
      return CanType(Param::getParameterType(/*forCanonical*/ true));
    }
  };

  using CanParamArrayRef =
    ArrayRefView<Param,CanParam,CanParam::getFromParam,/*AccessOriginal*/true>;
  
  class CanYield;
  class Yield {
    Type Ty;
    YieldTypeFlags Flags;
  public:
    explicit Yield(Type type, YieldTypeFlags flags)
      : Ty(type), Flags(flags) {}

    Type getType() const { return Ty; }

    YieldTypeFlags getFlags() const { return Flags; }
    ValueOwnership getValueOwnership() const {
      return getFlags().getValueOwnership();
    }
    bool isInOut() const { return getFlags().isInOut(); }

    CanYield getCanonical() const;

    /// There are a number of places where it's convenient to re-use
    /// the call machinery, processing yields as if they were
    /// parameters of a call.  Return this reinterpreted as a parameter.
    Param asParam() const {
      return Param(getType(), Identifier(), getFlags().asParamFlags());
    }

    Yield subst(SubstitutionMap subs, SubstOptions options = None) const {
      return Yield(getType().subst(subs, options), getFlags());
    }

    bool operator==(const Yield &other) const {
      return getType()->isEqual(other.getType()) &&
             getFlags() == other.getFlags();
    }
    bool operator!=(const Yield &other) const {
      return !operator==(other);
    }
  };

  class CanYield : public Yield {
  public:
    explicit CanYield(CanType type, YieldTypeFlags flags)
      : Yield(type, flags) {}

    CanType getType() const { return CanType(Yield::getType()); }
    CanParam asParam() const { return CanParam::getFromParam(Yield::asParam());}

    CanYield subst(SubstitutionMap subs, SubstOptions options = None) const {
      return CanYield(getType().subst(subs, options)->getCanonicalType(),
                      getFlags());
    }
  };

  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // If bits are added or removed, then TypeBase::AnyFunctionTypeBits
    // and NumMaskBits must be updated, and they must match.
    //
    //   SWIFT_ENABLE_TENSORFLOW
    //   |representation|isAutoClosure|noEscape|throws|differentiability|
    //   |    0 .. 3    |      4      |    5   |   6  |      7 .. 9     |
    //
    enum : unsigned {
      RepresentationMask     = 0xF << 0,
      AutoClosureMask        = 1 << 4,
      NoEscapeMask           = 1 << 5,
      ThrowsMask             = 1 << 6,
      // SWIFT_ENABLE_TENSORFLOW
      DifferentiabilityOffset = 7,
      DifferentiabilityMask  = 0b111 << DifferentiabilityOffset,
      NumMaskBits            = 10
    };

    unsigned Bits; // Naturally sized for speed.

    ExtInfo(unsigned Bits) : Bits(Bits) {}

    friend class AnyFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) {
      assert(getRepresentation() == Representation::Swift);
      // SWIFT_ENABLE_TENSORFLOW
      assert(getDifferentiability() == Differentiability::None);
    }

    // Constructor for polymorphic type.
    ExtInfo(Representation Rep, bool Throws) {
      Bits = ((unsigned) Rep) | (Throws ? ThrowsMask : 0);
    }

    // Constructor with no defaults.
    ExtInfo(Representation Rep,
            bool IsAutoClosure, bool IsNoEscape,
            // SWIFT_ENABLE_TENSORFLOW
            bool Throws, Differentiability Diff)
      : ExtInfo(Rep, Throws) {
      Bits |= (IsAutoClosure ? AutoClosureMask : 0);
      Bits |= (IsNoEscape ? NoEscapeMask : 0);
      // SWIFT_ENABLE_TENSORFLOW
      Bits |=
          (((unsigned)Diff << DifferentiabilityOffset) & DifferentiabilityMask);
    }

    bool isAutoClosure() const { return Bits & AutoClosureMask; }
    bool isNoEscape() const { return Bits & NoEscapeMask; }
    bool throws() const { return Bits & ThrowsMask; }
    // SWIFT_ENABLE_TENSORFLOW
    bool isDifferentiable() const { return Bits & DifferentiabilityMask; }
    Representation getRepresentation() const {
      unsigned rawRep = Bits & RepresentationMask;
      assert(rawRep <= unsigned(Representation::Last)
             && "unexpected SIL representation");
      return Representation(rawRep);
    }
    // SWIFT_ENABLE_TENSORFLOW
    Differentiability getDifferentiability() const {
      return Differentiability(
          (Bits & DifferentiabilityMask) >> DifferentiabilityOffset);
    }

    bool hasSelfParam() const {
      switch (getSILRepresentation()) {
      case SILFunctionTypeRepresentation::Thick:
      case SILFunctionTypeRepresentation::Block:
      case SILFunctionTypeRepresentation::Thin:
      case SILFunctionTypeRepresentation::CFunctionPointer:
      case SILFunctionTypeRepresentation::Closure:
      // SWIFT_ENABLE_TENSORFLOW
      case SILFunctionTypeRepresentation::TensorFlow:
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
      // SWIFT_ENABLE_TENSORFLOW
      case SILFunctionTypeRepresentation::TensorFlow:
        return false;
      }

      llvm_unreachable("Unhandled SILFunctionTypeRepresentation in switch.");
    }
    
    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    LLVM_NODISCARD
    ExtInfo withRepresentation(Representation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    LLVM_NODISCARD
    ExtInfo withIsAutoClosure(bool IsAutoClosure = true) const {
      if (IsAutoClosure)
        return ExtInfo(Bits | AutoClosureMask);
      else
        return ExtInfo(Bits & ~AutoClosureMask);
    }
    LLVM_NODISCARD
    ExtInfo withNoEscape(bool NoEscape = true) const {
      if (NoEscape)
        return ExtInfo(Bits | NoEscapeMask);
      else
        return ExtInfo(Bits & ~NoEscapeMask);
    }
    LLVM_NODISCARD
    ExtInfo withThrows(bool Throws = true) const {
      if (Throws)
        return ExtInfo(Bits | ThrowsMask);
      else
        return ExtInfo(Bits & ~ThrowsMask);
    }
    // SWIFT_ENABLE_TENSORFLOW
    LLVM_NODISCARD
    ExtInfo withDifferentiability(Differentiability diff) const {
      return ExtInfo((Bits & ~DifferentiabilityMask) |
                     (unsigned)diff << DifferentiabilityOffset);
    }

    unsigned getFuncAttrKey() const {
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
                  Type Output, RecursiveTypeProperties properties,
                  unsigned NumParams, ExtInfo Info)
  : TypeBase(Kind, CanTypeContext, properties), Output(Output) {
    Bits.AnyFunctionType.ExtInfo = Info.Bits;
    Bits.AnyFunctionType.NumParams = NumParams;
    assert(Bits.AnyFunctionType.NumParams == NumParams && "Params dropped!");
    // The use of both assert() and static_assert() is intentional.
    assert(Bits.AnyFunctionType.ExtInfo == Info.Bits && "Bits were dropped!");
    static_assert(ExtInfo::NumMaskBits == NumAFTExtInfoBits,
                 "ExtInfo and AnyFunctionTypeBitfields must agree on bit size");
  }

public:
  /// \brief Break an input type into an array of \c AnyFunctionType::Params.
  static void decomposeInput(Type type,
                             SmallVectorImpl<Param> &result);

  /// \brief Take an array of parameters and turn it into an input type.
  ///
  /// The result type is only there as a way to extract the ASTContext when
  /// needed.
  static Type composeInput(ASTContext &ctx, ArrayRef<Param> params,
                           bool canonicalVararg);
  static Type composeInput(ASTContext &ctx, CanParamArrayRef params,
                           bool canonicalVararg) {
    return composeInput(ctx, params.getOriginalArray(), canonicalVararg);
  }

  /// \brief Given two arrays of parameters determine if they are equal.
  static bool equalParams(ArrayRef<Param> a, ArrayRef<Param> b);

  /// \brief Given two arrays of parameters determine if they are equal.
  static bool equalParams(CanParamArrayRef a, CanParamArrayRef b);

  /// \brief Given an array of parameters and an array of labels of the
  /// same length, update each parameter to have the corresponding label.
  static void relabelParams(MutableArrayRef<Param> params,
                            ArrayRef<Identifier> labels);

  Type getResult() const { return Output; }
  ArrayRef<Param> getParams() const;
  unsigned getNumParams() const { return Bits.AnyFunctionType.NumParams; }

  GenericSignature *getOptGenericSignature() const;
  
  ExtInfo getExtInfo() const {
    return ExtInfo(Bits.AnyFunctionType.ExtInfo);
  }

  /// \brief Get the representation of the function type.
  Representation getRepresentation() const {
    return getExtInfo().getRepresentation();
  }

  // SWIFT_ENABLE_TENSORFLOW
  /// \brief Get the differentiability of the function type.
  Differentiability getDifferentiability() const {
    return getExtInfo().getDifferentiability();
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
  
  // SWIFT_ENABLE_TENSORFLOW
  bool isDifferentiable() const {
    return getExtInfo().isDifferentiable();
  }

  /// Returns a new function type exactly like this one but with the ExtInfo
  /// replaced.
  AnyFunctionType *withExtInfo(ExtInfo info) const;

  void printParams(raw_ostream &OS,
                   const PrintOptions &PO = PrintOptions()) const;
  void printParams(ASTPrinter &Printer, const PrintOptions &PO) const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_AnyFunctionType &&
           T->getKind() <= TypeKind::Last_AnyFunctionType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(AnyFunctionType, Type)
  using ExtInfo = AnyFunctionType::ExtInfo;
  using CanParamArrayRef = AnyFunctionType::CanParamArrayRef;

  static CanAnyFunctionType get(CanGenericSignature signature,
                                CanParamArrayRef params,
                                CanType result,
                                ExtInfo info = ExtInfo());

  CanGenericSignature getOptGenericSignature() const;

  CanParamArrayRef getParams() const {
    return CanParamArrayRef(getPointer()->getParams());
  }

  PROXY_CAN_TYPE_SIMPLE_GETTER(getResult)
  
  CanAnyFunctionType withExtInfo(ExtInfo info) const {
    return CanAnyFunctionType(getPointer()->withExtInfo(info));
  }
END_CAN_TYPE_WRAPPER(AnyFunctionType, Type)

inline AnyFunctionType::CanYield AnyFunctionType::Yield::getCanonical() const {
  return CanYield(getType()->getCanonicalType(), getFlags());
}

/// FunctionType - A monomorphic function type, specified with an arrow.
///
/// For example:
///   let x : (Float, Int) -> Int
class FunctionType final : public AnyFunctionType,
    public llvm::FoldingSetNode,
    private llvm::TrailingObjects<FunctionType, AnyFunctionType::Param> {
  friend TrailingObjects;
      
public:
  /// 'Constructor' Factory Function
  static FunctionType *get(ArrayRef<Param> params, Type result,
                           ExtInfo info = ExtInfo());

  // Retrieve the input parameters of this function type.
  ArrayRef<Param> getParams() const {
    return {getTrailingObjects<Param>(), getNumParams()};
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getParams(), getResult(), getExtInfo());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      ArrayRef<Param> params,
                      Type result,
                      ExtInfo info);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Function;
  }
      
private:
  FunctionType(ArrayRef<Param> params, Type result, ExtInfo info,
               const ASTContext *ctx, RecursiveTypeProperties properties);
};
BEGIN_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)
  static CanFunctionType get(CanParamArrayRef params, CanType result,
                             ExtInfo info = ExtInfo()) {
    auto fnType = FunctionType::get(params.getOriginalArray(), result, info);
    return cast<FunctionType>(fnType->getCanonicalType());
  }

  CanFunctionType withExtInfo(ExtInfo info) const {
    return CanFunctionType(cast<FunctionType>(getPointer()->withExtInfo(info)));
  }
END_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)

/// Map the given parameter list onto a bitvector describing whether
/// the argument type at each index has a default argument associated with
/// it.
SmallBitVector
computeDefaultMap(ArrayRef<AnyFunctionType::Param> params,
                  const ValueDecl *paramOwner, bool skipCurriedSelf);

/// Turn a param list into a symbolic and printable representation that does not
/// include the types, something like (: , b:, c:)
std::string getParamListAsString(ArrayRef<AnyFunctionType::Param> parameters);

/// Describes a generic function type.
///
/// A generic function type describes a function that is polymorphic with
/// respect to some set of generic parameters and the requirements placed
/// on those parameters and dependent member types thereof. The input and
/// output types of the generic function can be expressed in terms of those
/// generic parameters.
class GenericFunctionType final : public AnyFunctionType,
    public llvm::FoldingSetNode,
    private llvm::TrailingObjects<GenericFunctionType, AnyFunctionType::Param> {
  friend TrailingObjects;
      
  GenericSignature *Signature;

  /// Construct a new generic function type.
  GenericFunctionType(GenericSignature *sig,
                      ArrayRef<Param> params,
                      Type result,
                      ExtInfo info,
                      const ASTContext *ctx,
                      RecursiveTypeProperties properties);
      
public:
  /// Create a new generic function type.
  static GenericFunctionType *get(GenericSignature *sig,
                                  ArrayRef<Param> params,
                                  Type result,
                                  ExtInfo info = ExtInfo());

  // Retrieve the input parameters of this function type.
  ArrayRef<Param> getParams() const {
    return {getTrailingObjects<Param>(), getNumParams()};
  }
      
  /// Retrieve the generic signature of this function type.
  GenericSignature *getGenericSignature() const {
    return Signature;
  }
  
  /// Retrieve the generic parameters of this polymorphic function type.
  TypeArrayView<GenericTypeParamType> getGenericParams() const;

  /// Retrieve the requirements of this polymorphic function type.
  ArrayRef<Requirement> getRequirements() const;
                              
  /// Substitute the given generic arguments into this generic
  /// function type and return the resulting non-generic type.
  FunctionType *substGenericArgs(SubstitutionMap subs);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericSignature(), getParams(), getResult(),
            getExtInfo());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericSignature *sig,
                      ArrayRef<Param> params,
                      Type result,
                      ExtInfo info);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::GenericFunction;
  }
};

BEGIN_CAN_TYPE_WRAPPER(GenericFunctionType, AnyFunctionType)
  /// Create a new generic function type.
  static CanGenericFunctionType get(CanGenericSignature sig,
                                    CanParamArrayRef params,
                                    CanType result,
                                    ExtInfo info = ExtInfo()) {
    // Knowing that the argument types are independently canonical is
    // not sufficient to guarantee that the function type will be canonical.
    auto fnType = GenericFunctionType::get(sig, params.getOriginalArray(),
                                           result, info);
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

inline CanAnyFunctionType
CanAnyFunctionType::get(CanGenericSignature signature, CanParamArrayRef params,
                        CanType result, ExtInfo extInfo) {
  if (signature) {
    return CanGenericFunctionType::get(signature, params, result, extInfo);
  } else {
    return CanFunctionType::get(params, result, extInfo);
  }
}

inline GenericSignature *AnyFunctionType::getOptGenericSignature() const {
  if (auto genericFn = dyn_cast<GenericFunctionType>(this)) {
    return genericFn->getGenericSignature();
  } else {
    return nullptr;
  }
}

inline CanGenericSignature CanAnyFunctionType::getOptGenericSignature() const {
  if (auto genericFn = dyn_cast<GenericFunctionType>(*this)) {
    return genericFn.getGenericSignature();
  } else {
    return CanGenericSignature();
  }
}

/// Conventions for passing arguments as parameters.
enum class ParameterConvention {
  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory.  The callee is responsible for destroying the
  /// object.  The callee may assume that the address does not alias any valid
  /// object.
  Indirect_In,

  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an object in memory.  The callee must treat the object as read-only
  /// The callee may assume that the address does not alias any valid object.
  Indirect_In_Constant,

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
// Check that the enum values fit inside Bits.SILFunctionType.
static_assert(unsigned(ParameterConvention::Direct_Guaranteed) < (1<<3),
              "fits in Bits.SILFunctionType and SILParameterInfo");

// Does this parameter convention require indirect storage? This reflects a
// SILFunctionType's formal (immutable) conventions, as opposed to the transient
// SIL conventions that dictate SILValue types.
inline bool isIndirectFormalParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_In_Constant:
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
  case ParameterConvention::Indirect_In_Constant:
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
  case ParameterConvention::Indirect_In_Constant:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Owned:
    return false;
  }
  llvm_unreachable("bad convention kind");
}

/// A parameter type and the rules for passing it.
class SILParameterInfo {
  llvm::PointerIntPair<CanType, 3, ParameterConvention> TypeAndConvention;
public:
  SILParameterInfo() = default;//: Ty(), Convention((ParameterConvention)0) {}
  SILParameterInfo(CanType type, ParameterConvention conv)
    : TypeAndConvention(type, conv) {
    assert(type->isLegalSILType() && "SILParameterInfo has illegal SIL type");
  }

  CanType getType() const {
    return TypeAndConvention.getPointer();
  }
  ParameterConvention getConvention() const {
    return TypeAndConvention.getInt();
  }
  // Does this parameter convention require indirect storage? This reflects a
  // SILFunctionType's formal (immutable) conventions, as opposed to the
  // transient SIL conventions that dictate SILValue types.
  bool isFormalIndirect() const {
    return isIndirectFormalParameter(getConvention());
  }

  bool isDirectGuaranteed() const {
    return getConvention() == ParameterConvention::Direct_Guaranteed;
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
    id.AddPointer(getType().getPointer());
    id.AddInteger((unsigned)getConvention());
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
    return getType() == rhs.getType() && getConvention() == rhs.getConvention();
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

using YieldConvention = ParameterConvention;

/// The type and convention of a value yielded from a yield-once or
/// yield-many coroutine.
class SILYieldInfo : public SILParameterInfo {
public:
  SILYieldInfo() {}
  SILYieldInfo(CanType type, YieldConvention conv)
    : SILParameterInfo(type, conv) {
  }

  SILYieldInfo getWithType(CanType type) const {
    return SILYieldInfo(type, getConvention());
  }

  template<typename F>
  SILYieldInfo map(const F &fn) const {
    return getWithType(fn(getType()));
  }
};

/// SILCoroutineKind - What kind of coroutine is this SILFunction?
enum class SILCoroutineKind : uint8_t {
  /// This function is not a coroutine.  It may have arbitrary normal
  /// results and may not have yield results.
  None,

  /// This function is a yield-once coroutine (used by e.g. accessors).
  /// It must not have normal results and may have arbitrary yield results.
  YieldOnce,

  /// This function is a yield-many coroutine (used by e.g. generators).
  /// It must not have normal results and may have arbitrary yield results.
  YieldMany,
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
  // SWIFT_ENABLE_TENSORFLOW
  using Differentiability = FunctionTypeDifferentiability;

  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // If bits are added or removed, then TypeBase::SILFunctionTypeBits
    // and NumMaskBits must be updated, and they must match.

    // SWIFT_ENABLE_TENSORFLOW
    //   |representation|pseudogeneric| noescape | differentiability |
    //   |    0 .. 3    |      4      |     5    |      6 .. 8       |
    //
    enum : unsigned {
      RepresentationMask = 0xF << 0,
      PseudogenericMask  = 1 << 4,
      NoEscapeMask       = 1 << 5,
      // SWIFT_ENABLE_TENSORFLOW
      DifferentiabilityOffset = 6,
      DifferentiabilityMask = 0b111 << DifferentiabilityOffset,
      NumMaskBits        = 9
    };

    unsigned Bits; // Naturally sized for speed.

    ExtInfo(unsigned Bits) : Bits(Bits) {}

    friend class SILFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) { }

    // Constructor for polymorphic type.
    // SWIFT_ENABLE_TENSORFLOW
    ExtInfo(Representation rep, bool isPseudogeneric, bool isNoEscape,
            Differentiability diff) {
      Bits = ((unsigned) rep) |
             (isPseudogeneric ? PseudogenericMask : 0) |
             // SWIFT_ENABLE_TENSORFLOW
             (isNoEscape ? NoEscapeMask : 0) |
             ((unsigned)diff << DifferentiabilityOffset);
    }

    /// Is this function pseudo-generic?  A pseudo-generic function
    /// is not permitted to dynamically depend on its type arguments.
    bool isPseudogeneric() const { return Bits & PseudogenericMask; }

    // Is this function guaranteed to be no-escape by the type system?
    bool isNoEscape() const { return Bits & NoEscapeMask; }
    
    // SWIFT_ENABLE_TENSORFLOW
    bool isDifferentiable() const { return Bits & DifferentiabilityMask; }

    Differentiability getDifferentiability() const {
      return Differentiability(
          (Bits & DifferentiabilityMask) >> DifferentiabilityOffset);
    }

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
      // SWIFT_ENABLE_TENSORFLOW
      case Representation::TensorFlow:
        return false;
      case Representation::ObjCMethod:
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
      // SWIFT_ENABLE_TENSORFLOW
      case Representation::TensorFlow:
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
    ExtInfo withNoEscape(bool NoEscape = true) const {
      if (NoEscape)
        return ExtInfo(Bits | NoEscapeMask);
      else
        return ExtInfo(Bits & ~NoEscapeMask);
    }
    // SWIFT_ENABLE_TENSORFLOW
    ExtInfo withDifferentiability(Differentiability diff) const {
      return ExtInfo((Bits & ~DifferentiabilityMask) |
                     (unsigned)diff << DifferentiabilityOffset);
    }

    unsigned getFuncAttrKey() const {
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

  // These are *normal* results if this is not a coroutine and *yield* results
  // otherwise.
  unsigned NumAnyResults : 16;         // Not including the ErrorResult.
  unsigned NumAnyIndirectFormalResults : 16; // Subset of NumAnyResults.

  // The layout of a SILFunctionType in memory is:
  //   SILFunctionType
  //   SILParameterInfo[NumParameters]
  //   SILResultInfo[isCoroutine() ? 0 : NumAnyResults]
  //   SILYieldInfo[isCoroutine() ? NumAnyResults : 0]
  //   SILResultInfo?    // if hasErrorResult()
  //   CanType?          // if !isCoro && NumAnyResults > 1, formal result cache
  //   CanType?          // if !isCoro && NumAnyResults > 1, all result cache

  CanGenericSignature GenericSig;
  Optional<ProtocolConformanceRef> WitnessMethodConformance;

  MutableArrayRef<SILParameterInfo> getMutableParameters() {
    return {getTrailingObjects<SILParameterInfo>(), NumParameters};
  }

  MutableArrayRef<SILResultInfo> getMutableResults() {
    auto *ptr = reinterpret_cast<SILResultInfo *>(getMutableParameters().end());
    return {ptr, getNumResults()};
  }

  MutableArrayRef<SILYieldInfo> getMutableYields() {
    auto *ptr = reinterpret_cast<SILYieldInfo *>(getMutableParameters().end());
    return {ptr, getNumYields()};
  }

  /// Return a pointer past the end of the formal results, whether they
  /// are yield-results or normal results.
  void *getEndOfFormalResults() {
    return isCoroutine() ? static_cast<void*>(getMutableYields().end())
                         : static_cast<void*>(getMutableResults().end());
  }

  SILResultInfo &getMutableErrorResult() {
    assert(hasErrorResult());
    return *reinterpret_cast<SILResultInfo*>(getEndOfFormalResults());
  }

  /// Return a pointer past the end of all of the results, including the
  /// error result if one is present.
  void *getEndOfAllResults() {
    void *end = getEndOfFormalResults();
    if (hasErrorResult())
      end = reinterpret_cast<char*>(end) + sizeof(SILResultInfo);
    return end;
  }

  /// Do we have slots for caches of the normal-result tuple type?
  bool hasResultCache() const {
    return NumAnyResults > 1 && !isCoroutine();
  }

  CanType &getMutableFormalResultsCache() const {
    assert(hasResultCache());
    auto *ptr = const_cast<SILFunctionType *>(this)->getEndOfAllResults();
    return *reinterpret_cast<CanType*>(ptr);
  }

  CanType &getMutableAllResultsCache() const {
    assert(hasResultCache());
    auto *ptr = const_cast<SILFunctionType *>(this)->getEndOfAllResults();
    return *(reinterpret_cast<CanType *>(ptr) + 1);
  }

  SILFunctionType(GenericSignature *genericSig, ExtInfo ext,
                  SILCoroutineKind coroutineKind,
                  ParameterConvention calleeConvention,
                  ArrayRef<SILParameterInfo> params,
                  ArrayRef<SILYieldInfo> yieldResults,
                  ArrayRef<SILResultInfo> normalResults,
                  Optional<SILResultInfo> errorResult,
                  const ASTContext &ctx,
                  RecursiveTypeProperties properties,
                  Optional<ProtocolConformanceRef> witnessMethodConformance);

public:
  static CanSILFunctionType get(GenericSignature *genericSig,
                                ExtInfo ext,
                                SILCoroutineKind coroutineKind,
                                ParameterConvention calleeConvention,
                                ArrayRef<SILParameterInfo> interfaceParams,
                                ArrayRef<SILYieldInfo> interfaceYields,
                                ArrayRef<SILResultInfo> interfaceResults,
                                Optional<SILResultInfo> interfaceErrorResult,
                                const ASTContext &ctx,
              Optional<ProtocolConformanceRef> witnessMethodConformance = None);

  /// Return a structurally-identical function type with a slightly tweaked
  /// ExtInfo.
  CanSILFunctionType getWithExtInfo(ExtInfo ext);

  /// Return a structurally-identical function type with a slightly tweaked
  /// representation.
  CanSILFunctionType getWithRepresentation(Representation repr);

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
    return ParameterConvention(Bits.SILFunctionType.CalleeConvention);
  }
  bool isCalleeConsumed() const {
    return getCalleeConvention() == ParameterConvention::Direct_Owned;
  }
  bool isCalleeUnowned() const {
    return getCalleeConvention() == ParameterConvention::Direct_Unowned;
  }
  bool isCalleeGuaranteed() const {
    return getCalleeConvention() == ParameterConvention::Direct_Guaranteed;
  }

  /// Is this some kind of coroutine?
  bool isCoroutine() const {
    return getCoroutineKind() != SILCoroutineKind::None;
  }
  SILCoroutineKind getCoroutineKind() const {
    return SILCoroutineKind(Bits.SILFunctionType.CoroutineKind);
  }

  /// Return the array of all the yields.
  ArrayRef<SILYieldInfo> getYields() const {
    return const_cast<SILFunctionType *>(this)->getMutableYields();
  }
  unsigned getNumYields() const { return isCoroutine() ? NumAnyResults : 0; }

  /// Return the array of all result information. This may contain inter-mingled
  /// direct and indirect results.
  ArrayRef<SILResultInfo> getResults() const {
    return const_cast<SILFunctionType *>(this)->getMutableResults();
  }
  unsigned getNumResults() const { return isCoroutine() ? 0 : NumAnyResults; }

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
    return isCoroutine() ? 0 : NumAnyIndirectFormalResults;
  }
  /// Does this function have any formally indirect results?
  bool hasIndirectFormalResults() const {
    return getNumIndirectFormalResults() != 0;
  }
  unsigned getNumDirectFormalResults() const {
    return isCoroutine() ? 0 : NumAnyResults - NumAnyIndirectFormalResults;
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
    return Bits.SILFunctionType.HasErrorResult;
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

  /// SWIFT_ENABLE_TENSORFLOW
  CanSILFunctionType getGradientType(
      const SILAutoDiffConfig &config, SILModule &M);

  CanSILFunctionType getWithDifferentiability(
      unsigned differentiationOrder, const SmallBitVector &parameterIndices);

  /// Returns the type of a differentiation fucntion that is associated with
  /// a function of this type.
  CanSILFunctionType getAutoDiffAssociatedFunctionType(
      const SmallBitVector &parameterIndices, unsigned differentiationOrder,
      AutoDiffAssociatedFunctionKind kind, SILModule &module);

  /// If this is a @convention(witness_method) function with a protocol
  /// constrained self parameter, return the protocol constraint for
  /// the Self type.
  ProtocolDecl *getDefaultWitnessMethodProtocol() const;

  /// If this is a @convention(witness_method) function with a class
  /// constrained self parameter, return the class constraint for the
  /// Self type.
  ClassDecl *getWitnessMethodClass(ModuleDecl &M) const;

  /// If this is a @convention(witness_method) function, return the conformance
  /// for which the method is a witness.
  ProtocolConformanceRef getWitnessMethodConformance() const {
    assert(getRepresentation() == Representation::WitnessMethod);
    return *WitnessMethodConformance;
  }

  /// If this is a @convention(witness_method) function, return the conformance
  /// for which the method is a witness, if it isn't that convention, return
  /// None.
  Optional<ProtocolConformanceRef> getWitnessMethodConformanceOrNone() const {
    return WitnessMethodConformance;
  }

  ExtInfo getExtInfo() const { return ExtInfo(Bits.SILFunctionType.ExtInfo); }

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

  // SWIFT_ENABLE_TENSORFLOW
  /// \brief Get the differentiability of the function type.
  Differentiability getDifferentiability() const {
    return getExtInfo().getDifferentiability();
  }

  bool isPseudogeneric() const {
    return getExtInfo().isPseudogeneric();
  }

  bool isNoEscape() const {
    return getExtInfo().isNoEscape();
  }

  /// Thick swift noescape function types are trivial.
  bool isTrivialNoEscape() const {
    return isNoEscape() &&
           getRepresentation() == SILFunctionTypeRepresentation::Thick;
  }

  // SWIFT_ENABLE_TENSORFLOW
  bool isDifferentiable() const {
    return getExtInfo().isDifferentiable();
  }

  bool isNoReturnFunction() const; // Defined in SILType.cpp

  class ABICompatibilityCheckResult {
    friend class SILFunctionType;

    enum innerty {
      None,
      DifferentFunctionRepresentations,
      ABIEscapeToNoEscapeConversion,
      DifferentNumberOfResults,
      DifferentReturnValueConventions,
      ABIIncompatibleReturnValues,
      DifferentErrorResultConventions,
      ABIIncompatibleErrorResults,
      DifferentNumberOfParameters,
      DifferingParameterConvention,
      ABIIncompatibleParameterType,
    } kind;
    Optional<uintptr_t> payload;

    ABICompatibilityCheckResult(innerty kind) : kind(kind) {}
    ABICompatibilityCheckResult(innerty kind, uintptr_t payload)
        : kind(kind), payload(payload) {}

  public:
    ABICompatibilityCheckResult() = delete;

    bool isCompatible() const { return kind == innerty::None; }
    bool isCompatibleUpToNoEscapeConversion() {
      return kind == innerty::None ||
             kind == innerty::ABIEscapeToNoEscapeConversion;
    }

    bool hasPayload() const { return payload.hasValue(); }
    uintptr_t getPayload() const { return payload.getValue(); }
    StringRef getMessage() const;
  };

  /// Returns no-error if this SILFunctionType is ABI compatible with \p
  /// other. Otherwise, it returns a true error with a message in
  /// std::error_code. This is only meant to be used in assertions. When
  /// assertions are disabled, this just returns true.
  ABICompatibilityCheckResult
  isABICompatibleWith(CanSILFunctionType other) const;

  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                      SubstitutionMap subs);
  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                      TypeSubstitutionFn subs,
                                      LookupConformanceFn conformances);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericSignature(), getExtInfo(), getCoroutineKind(),
            getCalleeConvention(), getParameters(), getYields(),
            getResults(), getOptionalErrorResult(),
            getWitnessMethodConformanceOrNone());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericSignature *genericSig,
                      ExtInfo info,
                      SILCoroutineKind coroutineKind,
                      ParameterConvention calleeConvention,
                      ArrayRef<SILParameterInfo> params,
                      ArrayRef<SILYieldInfo> yields,
                      ArrayRef<SILResultInfo> results,
                      Optional<SILResultInfo> errorResult,
                      Optional<ProtocolConformanceRef> conformance);

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
class SILBoxType final : public TypeBase, public llvm::FoldingSetNode
{
  SILLayout *Layout;
  SubstitutionMap Substitutions;

  SILBoxType(ASTContext &C,
             SILLayout *Layout, SubstitutionMap Substitutions);

public:
  static CanSILBoxType get(ASTContext &C,
                           SILLayout *Layout,
                           SubstitutionMap Substitutions);

  SILLayout *getLayout() const { return Layout; }
  SubstitutionMap getSubstitutions() const { return Substitutions; }

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
                      SubstitutionMap Args);
  
  /// \brief Produce a profile of this box, for use in a folding set.
  void Profile(llvm::FoldingSetNodeID &id) {
    Profile(id, getLayout(), getSubstitutions());
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

/// A singleton 'token' type, which establishes a formal dependency
/// between two SIL nodes.  A token 'value' cannot be abstracted in
/// SIL: it cannot be returned, yielded, or passed as a function or
/// block argument.
class SILTokenType final : public TypeBase {
  friend class ASTContext;
  SILTokenType(const ASTContext &C)
    : TypeBase(TypeKind::SILToken, &C, RecursiveTypeProperties()) {}
public:
  // The singleton instance of this type is ASTContext::TheSILTokenType.

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILToken;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILTokenType, Type)

/// A type with a special syntax that is always sugar for a library type. The
/// library type may have multiple base types. For unary syntax sugar, see
/// UnarySyntaxSugarType.
///
/// The prime examples are:
/// Arrays: [T] -> Array<T>
/// Optionals: T? -> Optional<T>
/// Dictionaries: [K : V]  -> Dictionary<K, V>
class SyntaxSugarType : public SugarType {
protected:
  // Syntax sugar types are never canonical.
  SyntaxSugarType(TypeKind K, const ASTContext &ctx,
                  RecursiveTypeProperties properties)
    : SugarType(K, &ctx, properties) {}

public:
  Type getImplementationType() const { return getSinglyDesugaredType(); }

  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SyntaxSugarType &&
           T->getKind() <= TypeKind::Last_SyntaxSugarType;
  }
};

/// A type with a special syntax that is always sugar for a library type that
/// wraps a single other type.
///
/// The prime examples are arrays ([T] -> Array<T>) and
/// optionals (T? -> Optional<T>).
class UnarySyntaxSugarType : public SyntaxSugarType {
  Type Base;

protected:
  UnarySyntaxSugarType(TypeKind K, const ASTContext &ctx, Type base,
                       RecursiveTypeProperties properties)
    : SyntaxSugarType(K, ctx, properties), Base(base) {}

public:
  Type getBaseType() const {
    return Base;
  }

  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_UnarySyntaxSugarType &&
           T->getKind() <= TypeKind::Last_UnarySyntaxSugarType;
  }
};
  
/// The type [T], which is always sugar for a library type.
class ArraySliceType : public UnarySyntaxSugarType {
  ArraySliceType(const ASTContext &ctx, Type base,
                 RecursiveTypeProperties properties)
    : UnarySyntaxSugarType(TypeKind::ArraySlice, ctx, base, properties) {}

public:
  /// Return a uniqued array slice type with the specified base type.
  static ArraySliceType *get(Type baseTy);

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ArraySlice;
  }
};

/// The type T?, which is always sugar for a library type.
class OptionalType : public UnarySyntaxSugarType {
  OptionalType(const ASTContext &ctx,Type base,
               RecursiveTypeProperties properties)
    : UnarySyntaxSugarType(TypeKind::Optional, ctx, base, properties) {}

public:
  /// Return a uniqued optional type with the specified base type.
  static OptionalType *get(Type baseTy);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Optional;
  }
};

/// The dictionary type [K : V], which is syntactic sugar for Dictionary<K, V>.
///
/// Example:
/// \code
/// var dict: [String : Int] = ["hello" : 0, "world" : 1]
/// \endcode
class DictionaryType : public SyntaxSugarType {
  Type Key;
  Type Value;

protected:
  // Syntax sugar types are never canonical.
  DictionaryType(const ASTContext &ctx, Type key, Type value,
                 RecursiveTypeProperties properties)
    : SyntaxSugarType(TypeKind::Dictionary, ctx, properties), 
      Key(key), Value(value) {}

public:
  /// Return a uniqued dictionary type with the specified key and value types.
  static DictionaryType *get(Type keyTy, Type valueTy);

  Type getKeyType() const { return Key; }
  Type getValueType() const { return Value; }

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
  bool requiresClass();

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
class ProtocolCompositionType final : public TypeBase,
    public llvm::FoldingSetNode,
    private llvm::TrailingObjects<ProtocolCompositionType, Type> {
  friend TrailingObjects;
  
public:
  /// \brief Retrieve an instance of a protocol composition type with the
  /// given set of members.
  static Type get(const ASTContext &C, ArrayRef<Type> Members,
                  bool HasExplicitAnyObject);
  
  /// \brief Retrieve the set of members composed to create this type.
  ///
  /// For non-canonical types, this can contain classes, protocols and
  /// protocol compositions in any order. There can be at most one unique
  /// class constraint, either stated directly or as recursive member.
  ///
  /// In canonical types, this list will contain the superclass first if
  /// any, followed by zero or more protocols in a canonical sorted order,
  /// minimized to remove duplicates or protocols implied by inheritance.
  ///
  /// Note that the list of members is not sufficient to uniquely identify
  /// a protocol composition type; you also have to look at
  /// hasExplicitAnyObject().
  ArrayRef<Type> getMembers() const {
    return {getTrailingObjects<Type>(), Bits.ProtocolCompositionType.Count};
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getMembers(), hasExplicitAnyObject());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      ArrayRef<Type> Members,
                      bool HasExplicitAnyObject);

  /// True if the composition requires the concrete conforming type to
  /// be a class, either via a directly-stated superclass constraint or
  /// one of its member protocols being class-constrained.
  bool requiresClass();

  /// True if the class requirement is stated directly via '& AnyObject'.
  bool hasExplicitAnyObject() const {
    return Bits.ProtocolCompositionType.HasExplicitAnyObject;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ProtocolComposition;
  }
  
private:
  static ProtocolCompositionType *build(const ASTContext &C,
                                        ArrayRef<Type> Members,
                                        bool HasExplicitAnyObject);

  ProtocolCompositionType(const ASTContext *ctx, ArrayRef<Type> members,
                          bool hasExplicitAnyObject,
                          RecursiveTypeProperties properties)
    : TypeBase(TypeKind::ProtocolComposition, /*Context=*/ctx, properties) {
    Bits.ProtocolCompositionType.HasExplicitAnyObject = hasExplicitAnyObject;
    Bits.ProtocolCompositionType.Count = members.size();
    std::uninitialized_copy(members.begin(), members.end(),
                            getTrailingObjects<Type>());
  }
};
BEGIN_CAN_TYPE_WRAPPER(ProtocolCompositionType, Type)
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
    return Bits.ArchetypeType.NumProtocols;
  }

  size_t numTrailingObjects(OverloadToken<Type>) const {
    return Bits.ArchetypeType.HasSuperclass ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<LayoutConstraint>) const {
    return Bits.ArchetypeType.HasLayoutConstraint ? 1 : 0;
  }

  size_t numTrailingObjects(OverloadToken<UUID>) const {
    return getOpenedExistentialType() ? 1 : 0;
  }

  llvm::PointerUnion3<ArchetypeType *, TypeBase *,
                      GenericEnvironment *> ParentOrOpenedOrEnvironment;
  Type InterfaceType;
  MutableArrayRef<std::pair<Identifier, Type>> NestedTypes;

  void populateNestedTypes() const;
  void resolveNestedType(std::pair<Identifier, Type> &nested) const;

public:
  /// getNew - Create a new nested archetype with the given associated type.
  ///
  /// The ConformsTo array will be copied into the ASTContext by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               DependentMemberType *InterfaceType,
                               SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                               Type Superclass, LayoutConstraint Layout);

  /// getNew - Create a new primary archetype with the given name.
  ///
  /// The ConformsTo array will be minimized then copied into the ASTContext
  /// by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx,
                               GenericEnvironment *GenericEnv,
                               GenericTypeParamType *InterfaceType,
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

  /// Retrieve the interface type of this associated type, which will either
  /// be a GenericTypeParamType or a DependentMemberType.
  Type getInterfaceType() const { return InterfaceType; }

  /// Retrieve the associated type to which this archetype (if it is a nested
  /// archetype) corresponds.
  ///
  /// This associated type will have the same name as the archetype and will
  /// be a member of one of the protocols to which the parent archetype
  /// conforms.
  AssociatedTypeDecl *getAssocType() const;

  /// getConformsTo - Retrieve the set of protocols to which this substitutable
  /// type shall conform.
  ArrayRef<ProtocolDecl *> getConformsTo() const {
    return { getTrailingObjects<ProtocolDecl *>(),
             static_cast<size_t>(Bits.ArchetypeType.NumProtocols) };
  }
  
  /// requiresClass - True if the type can only be substituted with class types.
  /// This is true if the type conforms to one or more class protocols or has
  /// a superclass constraint.
  bool requiresClass() const;

  /// \brief Retrieve the superclass of this type, if such a requirement exists.
  Type getSuperclass() const {
    if (!Bits.ArchetypeType.HasSuperclass) return Type();

    return *getTrailingObjects<Type>();
  }

  /// \brief Retrieve the layout constraint of this type, if such a requirement exists.
  LayoutConstraint getLayoutConstraint() const {
    if (!Bits.ArchetypeType.HasLayoutConstraint) return LayoutConstraint();

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
          Type InterfaceType,
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
  static ReferenceStorageType *get(Type referent, ReferenceOwnership ownership,
                                   const ASTContext &C);

  Type getReferentType() const { return Referent; }
  ReferenceOwnership getOwnership() const {
    switch (getKind()) {
#define REF_STORAGE(Name, ...) \
    case TypeKind::Name##Storage: \
      return ReferenceOwnership::Name;
#include "swift/AST/ReferenceStorage.def"
    default:
      llvm_unreachable("Unhandled reference storage type");
    }
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_ReferenceStorageType &&
           T->getKind() <= TypeKind::Last_ReferenceStorageType;
  }
};
BEGIN_CAN_TYPE_WRAPPER(ReferenceStorageType, Type)
static CanReferenceStorageType get(CanType referent,
                                   ReferenceOwnership ownership) {
  return CanReferenceStorageType(ReferenceStorageType::get(
      referent, ownership, referent->getASTContext()));
  }
  PROXY_CAN_TYPE_SIMPLE_GETTER(getReferentType)
END_CAN_TYPE_WRAPPER(ReferenceStorageType, Type)

#define REF_STORAGE_HELPER(Name, isLoadable) \
class Name##StorageType : public ReferenceStorageType { \
  friend class ReferenceStorageType; \
  Name##StorageType(Type referent, const ASTContext *C, \
                    RecursiveTypeProperties properties) \
    : ReferenceStorageType(TypeKind::Name##Storage, referent, C, properties){} \
public: \
  static Name##StorageType *get(Type referent, const ASTContext &C) { \
    return static_cast<Name##StorageType *>( \
        ReferenceStorageType::get(referent, ReferenceOwnership::Name, C)); \
  } \
  isLoadable \
  static bool classof(const TypeBase *T) { \
    return T->getKind() == TypeKind::Name##Storage; \
  } \
}; \
BEGIN_CAN_TYPE_WRAPPER(Name##StorageType, ReferenceStorageType) \
  static Can##Name##StorageType get(CanType referent) { \
    return cast<Name##StorageType>( \
        CanType(Name##StorageType::get(referent, referent->getASTContext()))); \
  } \
END_CAN_TYPE_WRAPPER(Name##StorageType, ReferenceStorageType)
#define UNCHECKED_REF_STORAGE(Name, ...) \
  REF_STORAGE_HELPER(Name, )
#define ALWAYS_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  REF_STORAGE_HELPER(Name, )
#define NEVER_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  REF_STORAGE_HELPER(Name, )
#define SOMETIMES_LOADABLE_CHECKED_REF_STORAGE(Name, ...) \
  REF_STORAGE_HELPER(Name, bool isLoadable(ResilienceExpansion resilience) const;)
#include "swift/AST/ReferenceStorage.def"
#undef REF_STORAGE_HELPER

/// \brief A type variable used during type checking.
class TypeVariableType : public TypeBase {
  // Note: We can't use llvm::TrailingObjects here because the trailing object
  // type is opaque.

  TypeVariableType(const ASTContext &C, unsigned ID)
    : TypeBase(TypeKind::TypeVariable, &C,
               RecursiveTypeProperties::HasTypeVariable) {
    Bits.TypeVariableType.ID = ID;
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

  unsigned getID() const { return Bits.TypeVariableType.ID; }

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

inline bool TypeBase::isMaterializable() {
  if (hasLValueType())
    return false;

  if (is<InOutType>())
    return false;

  if (auto *TTy = getAs<TupleType>())
    return !TTy->hasElementWithOwnership();

  return true;
}

inline GenericTypeParamType *TypeBase::getRootGenericParam() {
  Type t(this);

  while (auto *memberTy = t->getAs<DependentMemberType>())
    t = memberTy->getBase();

  return t->castTo<GenericTypeParamType>();
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

inline bool TypeBase::isOpenedExistential() const {
  if (!hasOpenedExistential())
    return false;

  CanType T = getCanonicalType();
  if (auto archetype = dyn_cast<ArchetypeType>(T))
    return !archetype->getOpenedExistentialType().isNull();
  return false;
}

inline bool TypeBase::isOpenedExistentialWithError() {
  if (!hasOpenedExistential())
    return false;

  CanType T = getCanonicalType();
  if (auto archetype = dyn_cast<ArchetypeType>(T)) {
    auto openedExistentialType = archetype->getOpenedExistentialType();
    return (!openedExistentialType.isNull() &&
            openedExistentialType->isExistentialWithError());
  }
  return false;
}

inline bool TypeBase::canDynamicallyBeOptionalType(bool includeExistential) {
  CanType T = getCanonicalType();
  auto isArchetypeOrExistential = isa<ArchetypeType>(T) ||
    (includeExistential && T.isExistentialType());

  return isArchetypeOrExistential && !T.isAnyClassReferenceType();
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
  if (auto Ty = dyn_cast<NominalOrBoundGenericNominalType>(*this))
    return Ty->getDecl();
  return nullptr;
}

inline NominalTypeDecl *TypeBase::getAnyNominal() {
  return getCanonicalType().getAnyNominal();
}

inline Type TypeBase::getNominalParent() {
  return castTo<AnyGenericType>()->getParent();
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

/// getWithoutSpecifierType - For a non-materializable type
/// e.g. @lvalue or inout, retrieves the underlying object type.
/// Otherwise, returns the type itself.
inline Type TypeBase::getWithoutSpecifierType() {
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

inline CanType CanType::getWithoutSpecifierTypeImpl(CanType type) {
  if (auto refType = dyn_cast<InOutType>(type))
    return refType.getObjectType();
  if (auto refType = dyn_cast<LValueType>(type))
    return refType.getObjectType();
  return type;
}

inline CanType CanType::getNominalParent() const {
  return cast<NominalOrBoundGenericNominalType>(*this).getParent();
}

inline bool CanType::isActuallyCanonicalOrNull() const {
  return getPointer() == nullptr ||
         getPointer() == llvm::DenseMapInfo<TypeBase *>::getTombstoneKey() ||
         getPointer()->isCanonical();
}

inline Type TupleTypeElt::getVarargBaseTy() const {
  TypeBase *T = getType().getPointer();
  if (auto *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  if (auto *BGT = dyn_cast<BoundGenericType>(T)) {
    // It's the stdlib Array<T>.
    return BGT->getGenericArgs()[0];
  }
  assert(T->hasError());
  return T;
}

inline TupleTypeElt TupleTypeElt::getWithName(Identifier name) const {
  assert(getParameterFlags().isInOut() == getType()->is<InOutType>());
  return TupleTypeElt(getRawType(), name, getParameterFlags());
}

inline TupleTypeElt TupleTypeElt::getWithType(Type T) const {
  auto flags = getParameterFlags().withInOut(T->is<InOutType>());
  return TupleTypeElt(T->getInOutObjectType(), getName(), flags);
}

/// Create one from what's present in the parameter decl and type
inline ParameterTypeFlags
ParameterTypeFlags::fromParameterType(Type paramTy, bool isVariadic,
                                      // SWIFT_ENABLE_TENSORFLOW
                                      ValueOwnership ownership,
                                      bool isNonDifferentiable) {
  bool autoclosure = paramTy->is<AnyFunctionType>() &&
                     paramTy->castTo<AnyFunctionType>()->isAutoClosure();
  bool escaping = paramTy->is<AnyFunctionType>() &&
                  !paramTy->castTo<AnyFunctionType>()->isNoEscape();
  // FIXME(Remove InOut): The last caller that needs this is argument
  // decomposition.  Start by enabling the assertion there and fixing up those
  // callers, then remove this, then remove
  // ParameterTypeFlags::fromParameterType entirely.
  if (paramTy->is<InOutType>()) {
    assert(ownership == ValueOwnership::Default ||
           ownership == ValueOwnership::InOut);
    ownership = ValueOwnership::InOut;
  }
  // SWIFT_ENABLE_TENSORFLOW
  return {isVariadic, autoclosure, escaping, ownership, isNonDifferentiable};
}

inline const Type *BoundGenericType::getTrailingObjectsPointer() const {
  if (auto ty = dyn_cast<BoundGenericStructType>(this))
    return ty->getTrailingObjects<Type>();
  if (auto ty = dyn_cast<BoundGenericEnumType>(this))
    return ty->getTrailingObjects<Type>();
  if (auto ty = dyn_cast<BoundGenericClassType>(this))
    return ty->getTrailingObjects<Type>();
  llvm_unreachable("Unhandled BoundGenericType!");
}

inline ArrayRef<AnyFunctionType::Param> AnyFunctionType::getParams() const {
  switch (getKind()) {
  case TypeKind::Function:
    return cast<FunctionType>(this)->getParams();
  case TypeKind::GenericFunction:
    return cast<GenericFunctionType>(this)->getParams();
  default:
    llvm_unreachable("Undefined function type");
  }
}
  
/// \brief If this is a method in a type or extension thereof, compute
/// and return a parameter to be used for the 'self' argument.  The type of
/// the parameter is the empty Type() if no 'self' argument should exist. This
/// can only be used after name binding has resolved types.
///
/// \param isInitializingCtor Specifies whether we're computing the 'self'
/// type of an initializing constructor, which accepts an instance 'self'
/// rather than a metatype 'self'.
///
/// \param wantDynamicSelf Specifies whether the 'self' type should be
/// wrapped in a DynamicSelfType, which is the case for the 'self' parameter
/// type inside a class method returning 'Self'.
AnyFunctionType::Param computeSelfParam(AbstractFunctionDecl *AFD,
                                        bool isInitializingCtor=false,
                                        bool wantDynamicSelf=false);

#define TYPE(id, parent)
#define SUGARED_TYPE(id, parent) \
template <> \
constexpr bool TypeBase::isSugaredType<id##Type>() { \
  return true; \
}
#include "swift/AST/TypeNodes.def"

inline GenericParamKey::GenericParamKey(const GenericTypeParamType *p)
  : Depth(p->getDepth()), Index(p->getIndex()) { }

inline TypeBase *TypeBase::getDesugaredType() {
  if (!isa<SugarType>(this))
    return this;
  return cast<SugarType>(this)->getSinglyDesugaredType()->getDesugaredType();
}

inline bool TypeBase::hasSimpleTypeRepr() const {
  // NOTE: Please keep this logic in sync with TypeRepr::isSimple().
  switch (getKind()) {
  case TypeKind::Function:
  case TypeKind::GenericFunction:
    return false;

  case TypeKind::Metatype:
  case TypeKind::ExistentialMetatype:
    return !cast<const AnyMetatypeType>(this)->hasRepresentation();

  case TypeKind::Archetype:
    return !cast<const ArchetypeType>(this)->isOpenedExistential();

  case TypeKind::ProtocolComposition: {
    // 'Any', 'AnyObject' and single protocol compositions are simple
    auto composition = cast<const ProtocolCompositionType>(this);
    auto memberCount = composition->getMembers().size();
    if (composition->hasExplicitAnyObject())
      return memberCount == 0;
    return memberCount <= 1;
  }

  default:
    return true;
  }
}

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
