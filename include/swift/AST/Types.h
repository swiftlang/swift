//===--- Types.h - Swift Language Type ASTs ---------------------*- C++ -*-===//
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
// This file defines the TypeBase class and subclasses.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPES_H
#define SWIFT_TYPES_H

#include "swift/AST/DeclContext.h"
#include "swift/AST/DefaultArgumentKind.h"
#include "swift/AST/Ownership.h"
#include "swift/AST/Requirement.h"
#include "swift/AST/Type.h"
#include "swift/AST/Identifier.h"
#include "swift/Basic/ArrayRefView.h"
#include "swift/Basic/UUID.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Fixnum.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/Support/ErrorHandling.h"

namespace llvm {
  struct fltSemantics;
}
namespace swift {
  enum class AllocationArena;
  class ArchetypeType;
  class AssociatedTypeDecl;
  class ASTContext;
  class ClassDecl;
  class ExprHandle;
  class GenericTypeParamDecl;
  class GenericTypeParamType;
  class GenericParamList;
  class GenericSignature;
  class Identifier;
  class SILModule;
  class SILType;
  class TypeAliasDecl;
  class TypeDecl;
  class NominalTypeDecl;
  class EnumDecl;
  class EnumElementDecl;
  class StructDecl;
  class ProtocolDecl;
  class TypeVariableType;
  class ValueDecl;
  class ModuleDecl;
  class ProtocolConformance;
  class Substitution;
  enum OptionalTypeKind : unsigned;
  enum PointerTypeKind : unsigned;

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
  enum { BitWidth = 8 };

  /// A single property.
  ///
  /// Note that the property polarities should be chosen so that 0 is
  /// the correct default value and bitwise-or correctly merges things.
  enum Property : unsigned {
    /// This type expression contains a TypeVariableType.
    HasTypeVariable     = 0x01,

    /// This type expression contains an ArchetypeType.
    HasArchetype        = 0x02,

    /// This type expression contains a GenericTypeParamType.
    IsDependent         = 0x04,

    /// This type expression contains an LValueType or InOutType,
    /// other than as a function input.
    IsNotMaterializable = 0x08,
    
    /// This type expression contains an LValueType and can be loaded to convert
    /// to an rvalue.
    IsLValue = 0x10,

    /// This type expression contains an opened existential ArchetypeType.
    HasOpenedExistential = 0x20,

    /// This type expression contains a DynamicSelf type.
    HasDynamicSelf = 0x40,

    /// Whether this type expression contains an unbound generic type.
    HasUnboundGeneric = 0x80,
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

  /// Is a type with these properties dependent, in the sense of being
  /// expressed in terms of a generic type parameter or a dependent
  /// member thereof?
  bool isDependent() const { return Bits & IsDependent; }

  /// Is a type with these properties materializable: that is, is it a
  /// first-class value type?
  bool isMaterializable() const { return !(Bits & IsNotMaterializable); }

  /// Is a type with these properties an lvalue?
  bool isLValue() const { return Bits & IsLValue; }
  
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
  friend RecursiveTypeProperties operator+(Property lhs, Property rhs) {
    return RecursiveTypeProperties(lhs | rhs);
  }
  friend RecursiveTypeProperties operator+(RecursiveTypeProperties lhs,
                                           RecursiveTypeProperties rhs) {
    return RecursiveTypeProperties(lhs.Bits | rhs.Bits);
  }

  /// Add any properties in the right-hand set to this set.
  RecursiveTypeProperties &operator+=(RecursiveTypeProperties other) {
    Bits |= other.Bits;
    return *this;
  }

  /// Returns the set of properties present in the left-hand set but
  /// missing in the right-hand set.
  RecursiveTypeProperties operator-(RecursiveTypeProperties other) {
    return RecursiveTypeProperties(Bits & ~other.Bits);
  }

  /// Remove any properties in the right-hand set from this set.
  RecursiveTypeProperties &operator-=(RecursiveTypeProperties other) {
    Bits &= ~other.Bits;
    return *this;
  }

  /// Test for a particular property in this set.
  bool operator&(Property prop) const {
    return Bits & prop;
  }
};
  
/// The result of a type trait check.
enum class TypeTraitResult {
  /// The type cannot have the trait.
  IsNot,
  /// The generic type can be bound to a type that has the trait.
  CanBe,
  /// The type has the trait irrespective of generic substitutions.
  Is,
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
  struct AnyFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// Extra information which affects how the function is called, like
    /// regparm and the calling convention.
    unsigned ExtInfo : 16;
  };
  enum { NumAnyFunctionTypeBits = NumTypeBaseBits + 16 };
  static_assert(NumAnyFunctionTypeBits <= 32, "fits in an unsigned");

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
  enum { NumSILFunctionTypeBits = NumTypeBaseBits + 16+4 };
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
    AnyFunctionTypeBitfields AnyFunctionTypeBits;
    TypeVariableTypeBitfields TypeVariableTypeBits;
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

  /// Determine whether the type involves an archetype.
  bool hasArchetype() const {
    return getRecursiveProperties().hasArchetype();
  }

  /// Determine whether the type involves an opened existential archetype.
  bool hasOpenedExistential() const {
    return getRecursiveProperties().hasOpenedExistential();
  }

  /// Determine whether the type involves the given opend existential
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
  Type eraseOpenedExistential(ModuleDecl *module, ArchetypeType *opened);

  /// \brief Compute and return the set of type variables that occur within this
  /// type.
  ///
  /// \param typeVariables This vector is populated with the set of
  /// type variables referenced by this type.
  void getTypeVariables(SmallVectorImpl<TypeVariableType *> &typeVariables);

  /// Determine whether the type is directly dependent on a generic type
  /// parameter.
  ///
  /// Unlike the C++ notion of "dependent", for which nearly any occurrence of
  /// a generic parameter within the type causes the type to be dependent,
  /// the Swift definition of "dependent" is fairly shallow: we either have
  /// a generic parameter or a member of that generic parameter. Types such
  /// as X<T>, where T is a generic parameter, are not considered "dependent".
  bool isDependentType() {
    return getRecursiveProperties().isDependent();
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

  /// Retrieve the complete set of generic arguments for a specialized type.
  ///
  /// \param scratch Scratch space to use when the complete list needs to be
  /// stitched together from multiple lists of generic arguments.
  ArrayRef<Type> getAllGenericArgs(SmallVectorImpl<Type> &scratch);

  /// Gather all of the substitutions used to produce the given specialized type
  /// from its unspecialized type.
  ///
  /// \param scratchSpace The substitutions will be written into this scratch
  /// space if a single substitutions array cannot be returned.
  ArrayRef<Substitution> gatherAllSubstitutions(
                           ModuleDecl *module,
                           SmallVectorImpl<Substitution> &scratchSpace,
                           LazyResolver *resolver,
                           DeclContext *gpContext = nullptr);

  /// Gather all of the substitutions used to produce the given specialized type
  /// from its unspecialized type.
  ///
  /// \returns ASTContext-allocated substitutions.
  ArrayRef<Substitution> gatherAllSubstitutions(
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

  /// \brief Retrieve the superclass of this type.
  ///
  /// \param resolver The resolver for lazy type checking, or null if the
  ///                 AST is already type-checked.
  ///
  /// \returns The superclass of this type, or a null type if it has no
  ///          superclass.
  Type getSuperclass(LazyResolver *resolver);
  
  /// \brief True if this type is a superclass of another type.
  ///
  /// \param ty       The potential subclass.
  /// \param resolver The resolver for lazy type checking, or null if the
  ///                 AST is already type-checked.
  ///
  /// \returns True if this type is \c ty or a superclass of \c ty.
  bool isSuperclassOf(Type ty, LazyResolver *resolver);

  /// \brief Determines whether this type is a subtype of the \p other,
  /// guaranteed to have the same representation, and is permitted in an
  /// override.
  bool canOverride(Type other, bool allowUnsafeParameterOverride,
                   LazyResolver *resolver);

  /// \brief Determines whether this type has a retainable pointer
  /// representation, i.e. whether it is representable as a single,
  /// possibly nil pointer that can be unknown-retained and
  /// unknown-released.
  bool hasRetainablePointerRepresentation();

  /// Determines whether this type has a bridgable object
  /// representation, i.e., whether it is representable as a single
  /// (non-nil) pointer that can be unknown-retained and
  /// unknown-released.
  ///
  /// This predicate covers all types that could be placed into an
  /// AnyObject.
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

  /// getUnlabeledType - Retrieve a version of this type with all labels
  /// removed at every level. For example, given a tuple type 
  /// \code
  /// (p : (x : int, y : int))
  /// \endcode
  /// the result would be the (parenthesized) type ((int, int)).
  Type getUnlabeledType(ASTContext &Context);

  /// Relabel the elements of the given type with the given new
  /// (top-level) labels.
  Type getRelabeledType(ASTContext &Context, ArrayRef<Identifier> labels);

  /// \brief Retrieve the type without any default arguments.
  Type getWithoutDefaultArgs(const ASTContext &Context);

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
                                  unsigned uncurryLevel = 1);

  /// Returns a function type that is not 'noreturn', but is otherwis the same
  /// as this type.
  Type getWithoutNoReturn(unsigned UncurryLevel);

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
  /// tuples, lvalue types, and metatypes.
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
  ///   typealias SomeArray = [T];
  /// }
  /// \endcode
  ///
  /// Asking for the member substitutions of \c X<Int,String> within
  /// the context of the extension above will produce substitutions T
  /// -> Int and U -> String suitable for mapping the type of
  /// \c SomeArray.
  TypeSubstitutionMap getMemberSubstitutions(DeclContext *dc);

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
  /// \param resolver The resolver for lazy type checking, which may be null for
  /// a fully-type-checked AST.
  ///
  /// \param memberType The type of the member, in which archetypes will be
  /// replaced by the generic arguments provided by the base type. If null,
  /// the member's type will be used.
  ///
  /// \returns the resulting member type.
  Type getTypeOfMember(ModuleDecl *module, const ValueDecl *member,
                       LazyResolver *resolver, Type memberType = Type());

  /// Given the type of a member from a particular declaration context,
  /// substitute in the types from the given base type (this) to produce
  /// the resulting member type.
  Type getTypeOfMember(ModuleDecl *module, Type memberType,
                       DeclContext *memberDC);

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

  /// Return the type T after looking through all of the optional or
  /// implicitly-unwrapped optional types.
  Type lookThroughAllAnyOptionalTypes();

  /// Return the type T after looking through all of the optional or
  /// implicitly-unwrapped optional types.
  Type lookThroughAllAnyOptionalTypes(SmallVectorImpl<Type> &optionals);

  /// Whether this is the AnyObject type.
  bool isAnyObject();

  void dump() const LLVM_ATTRIBUTE_USED;
  void dump(raw_ostream &os, unsigned indent = 0) const;

  void dumpPrint() const LLVM_ATTRIBUTE_USED;
  void print(raw_ostream &OS,
             const PrintOptions &PO = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &PO) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;
  
  /// Return whether this type is or can be substituted for a class type.
  TypeTraitResult canBeClass();
  
  /// Returns true if the type conforms to protocols using witnesses from the
  /// environment or from within the value. Generic parameters and existentials
  /// meet this criteria. In these cases we represent the
  /// conformance as a null ProtocolConformance* pointer, because there is no
  /// static conformance associated with the conforming type.
  bool hasDependentProtocolConformances();
  
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
  ErrorType(ASTContext &C) 
    : TypeBase(TypeKind::Error, &C, RecursiveTypeProperties()) { }
public:
  static Type get(const ASTContext &C);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Error;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ErrorType, Type)

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

/// ParenType - A paren type is a type that's been written in parentheses.
class ParenType : public TypeBase {
  Type UnderlyingType;

  friend class ASTContext;
  ParenType(Type UnderlyingType, RecursiveTypeProperties properties)
    : TypeBase(TypeKind::Paren, nullptr, properties),
      UnderlyingType(UnderlyingType) {}
public:
  Type getUnderlyingType() const { return UnderlyingType; }

  static ParenType *get(const ASTContext &C, Type underlying);
   
  /// Remove one level of top-level sugar from this type.
  TypeBase *getSinglyDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Paren;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
  /// An optional name for the field, along with a bit indicating whether it
  /// is variadic.
  llvm::PointerIntPair<Identifier, 1, bool> NameAndVariadic;

  /// \brief This is the type of the field, which is mandatory, along with the
  /// kind of default argument.
  llvm::PointerIntPair<Type, 3, DefaultArgumentKind> TyAndDefaultArg;

  friend class TupleType;

public:
  TupleTypeElt() = default;
  inline /*implicit*/ TupleTypeElt(Type ty,
                                   Identifier name = Identifier(),
                                   DefaultArgumentKind defaultArg =
                                     DefaultArgumentKind::None,
                                   bool isVariadic = false);

  /*implicit*/ TupleTypeElt(TypeBase *Ty)
    : NameAndVariadic(Identifier(), false),
      TyAndDefaultArg(Ty, DefaultArgumentKind::None) { }

  bool hasName() const { return !NameAndVariadic.getPointer().empty(); }
  Identifier getName() const { return NameAndVariadic.getPointer(); }

  Type getType() const { return TyAndDefaultArg.getPointer(); }

  /// Determine whether this field is variadic.
  bool isVararg() const {
    return NameAndVariadic.getInt();
  }

  /// Retrieve the kind of default argument available on this field.
  DefaultArgumentKind getDefaultArgKind() const {
    return TyAndDefaultArg.getInt();
  }

  static inline Type getVarargBaseTy(Type VarArgT);

  Type getVarargBaseTy() const {
    assert(isVararg());
    return getVarargBaseTy(getType());
  }

  /// Retrieve a copy of this tuple type element with the type replaced.
  TupleTypeElt getWithType(Type T) const {
    return TupleTypeElt(T, getName(), getDefaultArgKind(), isVararg());
  }

  /// Determine whether this tuple element has an initializer.
  bool hasInit() const {
    return getDefaultArgKind() != DefaultArgumentKind::None;
  }
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
  
  /// getNamedElementId - If this tuple has a element with the specified name,
  /// return the element index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// hasAnyDefaultValues - Return true if any of our elements has a default
  /// value.
  bool hasAnyDefaultValues() const;
  
  /// getElementForScalarInit - If a tuple of this type can be initialized with
  /// a scalar, return the element number that the scalar is assigned to.  If
  /// not, return -1.
  int getElementForScalarInit() const;

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

/// UnboundGenericType - Represents a generic nominal type where the
/// type arguments have not yet been resolved.
class UnboundGenericType : public TypeBase, public llvm::FoldingSetNode {
  NominalTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

private:
  UnboundGenericType(NominalTypeDecl *TheDecl, Type Parent, const ASTContext &C,
                     RecursiveTypeProperties properties)
    : TypeBase(TypeKind::UnboundGeneric,
               (!Parent || Parent->isCanonical())? &C : nullptr,
               properties + RecursiveTypeProperties::HasUnboundGeneric),
      TheDecl(TheDecl), Parent(Parent) { }

public:
  static UnboundGenericType* get(NominalTypeDecl *TheDecl, Type Parent,
                                 const ASTContext &C);

  /// \brief Returns the declaration that declares this type.
  NominalTypeDecl *getDecl() const { return TheDecl; }

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
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *D,
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

/// BoundGenericType - An abstract class for applying a generic
/// nominal type to the given type arguments.
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

  /// \brief Retrieve the set of substitutions used to produce this bound
  /// generic type from the underlying generic type.
  ///
  /// \param module The module in which we should compute the substitutions.
  /// FIXME: We currently don't account for this properly, so it can be null.
  ///
  /// \param resolver The resolver that handles lazy type checking, where
  /// required. This can be null for a fully-type-checked AST.
  ///
  /// \param gpContext The context from which the generic parameters will be
  /// extracted, which will be either the nominal type declaration or an
  /// extension thereof. If null, will be set to the nominal type declaration.
  ArrayRef<Substitution> getSubstitutions(ModuleDecl *module,
                                          LazyResolver *resolver,
                                          DeclContext *gpContext = nullptr);

  /// Retrieves the generic parameter context to use with substitutions for
  /// this bound generic type, using the given context if possible.
  DeclContext *getGenericParamContext(DeclContext *gpContext) const;

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
    return  T->getKind() == TypeKind::Module;
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
               properties + RecursiveTypeProperties(
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
};

/// Can this calling convention result in a function being called indirectly
/// through the runtime.
inline bool canBeCalledIndirectly(SILFunctionTypeRepresentation rep) {
  switch (rep) {
  case SILFunctionTypeRepresentation::Thick:
  case SILFunctionTypeRepresentation::Thin:
  case SILFunctionTypeRepresentation::CFunctionPointer:
  case SILFunctionTypeRepresentation::Block:
    return false;
  case SILFunctionTypeRepresentation::ObjCMethod:
  case SILFunctionTypeRepresentation::Method:
  case SILFunctionTypeRepresentation::WitnessMethod:
    return true;
  }
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
    return SILFunctionLanguage::Swift;
  }
}

/// AnyFunctionType - A function type has a single input and result, but
/// these types may be tuples, for example:
///   "(int) -> int" or "(a : int, b : int) -> (int, int)".
/// Note that the parser requires that the input to a function type be a Tuple
/// or ParenType, but ParenType desugars to its element, so the input to a
/// function may be an arbitrary type.
///
/// There are two kinds of function types:  monomorphic (FunctionType) and
/// polymorphic (PolymorphicFunctionType). Both type families additionally can
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

    //   |representation|isAutoClosure|noReturn|noEscape|throws|
    //   |    0 .. 3    |      4      |   5    |    6   |   7  |
    //
    enum : uint16_t { RepresentationMask = 0x00F };
    enum : uint16_t { AutoClosureMask    = 0x010 };
    enum : uint16_t { NoReturnMask       = 0x020 };
    enum : uint16_t { NoEscapeMask       = 0x040 };
    enum : uint16_t { ThrowsMask         = 0x080 };

    uint16_t Bits;

    ExtInfo(unsigned Bits) : Bits(static_cast<uint16_t>(Bits)) {}

    friend class AnyFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) {
      assert(getRepresentation() == Representation::Swift);
    }

    // Constructor for polymorphic type.
    ExtInfo(Representation Rep, bool IsNoReturn, bool Throws) {
      Bits = ((unsigned) Rep) |
             (IsNoReturn ? NoReturnMask : 0) |
             (Throws ? ThrowsMask : 0);
    }

    // Constructor with no defaults.
    ExtInfo(Representation Rep, bool IsNoReturn,
            bool IsAutoClosure, bool IsNoEscape, bool Throws)
      : ExtInfo(Rep, IsNoReturn, Throws) {
      Bits |= (IsAutoClosure ? AutoClosureMask : 0);
      Bits |= (IsNoEscape ? NoEscapeMask : 0);
    }

    bool isNoReturn() const { return Bits & NoReturnMask; }
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
        return false;
      case SILFunctionTypeRepresentation::ObjCMethod:
      case SILFunctionTypeRepresentation::Method:
      case SILFunctionTypeRepresentation::WitnessMethod:
        return true;
      }
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
        return false;
      }
    }
    
    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    ExtInfo withRepresentation(Representation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    ExtInfo withIsNoReturn(bool IsNoReturn = true) const {
      if (IsNoReturn)
        return ExtInfo(Bits | NoReturnMask);
      else
        return ExtInfo(Bits & ~NoReturnMask);
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
  
  bool isNoReturn() const {
    return getExtInfo().isNoReturn();
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
END_CAN_TYPE_WRAPPER(AnyFunctionType, Type)

/// FunctionType - A monomorphic function type.
///
/// If the AutoClosure bit is set to true, then the input type is known to be ()
/// and a value of this function type is only assignable (in source code) from
/// the destination type of the function. Sema inserts an ImplicitClosure to
/// close over the value.  For example:
///   @autoclosure var x : () -> Int = 4
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
END_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)
  
/// PolymorphicFunctionType - A polymorphic function type.
class PolymorphicFunctionType : public AnyFunctionType {
  // TODO: storing a GenericParamList* here is really the wrong solution;
  // we should be able to store something readily canonicalizable.
  GenericParamList *Params;
public:
  /// 'Constructor' Factory Function
  static PolymorphicFunctionType *get(Type input, Type output,
                                      GenericParamList *params,
                                      bool throws = false) {
    return get(input, output, params, ExtInfo().withThrows(throws));
  }

  static PolymorphicFunctionType *get(Type input, Type output,
                                      GenericParamList *params,
                                      const ExtInfo &Info);

  ArrayRef<GenericTypeParamDecl *> getGenericParameters() const;
  ArrayRef<ArchetypeType *> getAllArchetypes() const;

  GenericParamList &getGenericParams() const { return *Params; }

  /// Substitute the given generic arguments into this polymorphic
  /// function type and return the resulting non-polymorphic type.
  FunctionType *substGenericArgs(ModuleDecl *M, ArrayRef<Type> args);

  /// Substitute the given generic arguments into this polymorphic
  /// function type and return the resulting non-polymorphic type.
  ///
  /// The order of Substitutions must match the order of generic archetypes.
  FunctionType *substGenericArgs(ModuleDecl *M, ArrayRef<Substitution> subs);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::PolymorphicFunction;
  }
  
private:
  PolymorphicFunctionType(Type input, Type output,
                          GenericParamList *params,
                          const ExtInfo &Info,
                          const ASTContext &C,
                          RecursiveTypeProperties properties);
};  
BEGIN_CAN_TYPE_WRAPPER(PolymorphicFunctionType, AnyFunctionType)
  static CanPolymorphicFunctionType get(CanType input, CanType result,
                                        GenericParamList *params,
                                        const ExtInfo &info) {
    return CanPolymorphicFunctionType(
             PolymorphicFunctionType::get(input, result, params, info));
  }
END_CAN_TYPE_WRAPPER(PolymorphicFunctionType, AnyFunctionType)

/// Describes a generic function type.
///
/// A generic function type describes a function that is polymorphic with
/// respect to some set of generic parameters and the requirements placed
/// on those parameters and dependent member types thereof. The input and
/// output types of the generic function can be expressed in terms of those
/// generic parameters.
///
/// FIXME: \c GenericFunctionType is meant as a replacement for
/// \c PolymorphicFunctionType.
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
                              
  /// Substitute all of the given generic arguments into this generic
  /// function type and return the resulting non-generic type.
  FunctionType *substGenericArgs(ModuleDecl *M, ArrayRef<Type> args) const;

  /// Substitute the given generic arguments into this generic
  /// function type and return the resulting non-generic type.
  ///
  /// The order of Substitutions must match the order of generic parameters.
  FunctionType *substGenericArgs(ModuleDecl *M, ArrayRef<Substitution> subs);

  /// Substitute the given generic arguments into this generic
  /// function type, possibly leaving some of the generic parameters
  /// unsubstituted, and return the resulting function type.
  AnyFunctionType *partialSubstGenericArgs(ModuleDecl *M, ArrayRef<Type> args) const;
                              
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
    return CanGenericFunctionType(
              GenericFunctionType::get(sig, input, result, info));
  }
  
  CanGenericSignature getGenericSignature() const {
    return CanGenericSignature(getPointer()->getGenericSignature());
  }
  
  ArrayRef<CanTypeWrapper<GenericTypeParamType>> getGenericParams() const {
    return getGenericSignature().getGenericParams();
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
  /// of an object in memory.  The object is instantaneously valid on entry, and
  /// it must be instantaneously valid on exit.  The callee may assume that the
  /// address does not alias any valid object.
  Indirect_Inout,

  /// This argument is passed indirectly, i.e. by directly passing the address
  /// of an uninitialized object in memory.  The callee is responsible for
  /// leaving an initialized object at this address.  The callee may assume that
  /// the address does not alias any valid object.
  Indirect_Out,

  /// This argument is passed directly.  Its type is non-trivial, and the callee
  /// is responsible for destroying it.
  Direct_Owned,

  /// This argument is passed directly.  Its type may be trivial, or it may
  /// simply be that the callee is not responsible for destroying it.  Its
  /// validity is guaranteed only at the instant the call begins.
  Direct_Unowned,

  /// This argument is passed directly. Its type is non-trivial, and the callee
  /// guarantees that the caller can treat the argument as being instantaneously
  /// deallocated when the callee returns.
  Direct_Deallocating,

  /// This argument is passed directly.  Its type is non-trivial, and the caller
  /// guarantees its validity for the entirety of the call.
  Direct_Guaranteed,
};
inline bool isIndirectParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_In_Guaranteed:
    return true;

  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Deallocating:
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
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Direct_Deallocating:
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
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_In:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Owned:
  case ParameterConvention::Direct_Deallocating:
    return false;
  }
  llvm_unreachable("bad convention kind");
}

inline bool isDeallocatingParameter(ParameterConvention conv) {
  switch (conv) {
  case ParameterConvention::Direct_Deallocating:
    return true;

  case ParameterConvention::Indirect_In:
  case ParameterConvention::Indirect_Inout:
  case ParameterConvention::Indirect_Out:
  case ParameterConvention::Indirect_In_Guaranteed:
  case ParameterConvention::Direct_Unowned:
  case ParameterConvention::Direct_Guaranteed:
  case ParameterConvention::Direct_Owned:
    return false;
  }
  llvm_unreachable("covered switch isn't covered?!");
}

/// A parameter type and the rules for passing it.
class SILParameterInfo {
  llvm::PointerIntPair<CanType, 3, ParameterConvention> TypeAndConvention;
public:
  SILParameterInfo() = default;
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
  bool isIndirect() const {
    return isIndirectParameter(getConvention());
  }

  bool isIndirectInGuaranteed() const {
    return getConvention() == ParameterConvention::Indirect_In_Guaranteed;
  }

  bool isIndirectInOut() const {
    return getConvention() == ParameterConvention::Indirect_Inout;
  }
  bool isIndirectResult() const {
    return getConvention() == ParameterConvention::Indirect_Out;
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

  /// Returns true if this parameter is deallocating. This means that the
  /// deallocating bit has been set on the parameter. This means that retains,
  /// releases are inert for the duration of the lifetime of the function.
  bool isDeallocating() const {
    return isDeallocatingParameter(getConvention());
  }

  SILType getSILType() const; // in SILType.h

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

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// The API is comparable to Type::subst.
  SILParameterInfo subst(ModuleDecl *module, TypeSubstitutionMap &substitutions,
                         SubstOptions options) const {
    Type type = getType().subst(module, substitutions, options);
    return getWithType(type->getCanonicalType());
  }

  void profile(llvm::FoldingSetNodeID &id) {
    id.AddPointer(TypeAndConvention.getOpaqueValue());
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
    return TypeAndConvention == rhs.TypeAndConvention;
  }
  bool operator!=(SILParameterInfo rhs) const {
    return !(*this == rhs);
  }
};

/// Conventions for returning values.  All return values at this
/// level are direct.
enum class ResultConvention {
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

/// A direct result type and the rules for returning it.
///
/// Indirect results require an implicit address parameter and are
/// therefore represented with a kind of SILParameterInfo.  For now, a
/// function with an indirect result will always have a SILResultInfo
/// with the empty tuple type '()'.
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
  SILType getSILType() const; // in SILType.h

  /// Return a version of this result info with the type replaced.
  SILResultInfo getWithType(CanType type) const {
    return SILResultInfo(type, getConvention());
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

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// The API is comparable to Type::subst.
  SILResultInfo subst(ModuleDecl *module, TypeSubstitutionMap &substitutions,
                      SubstOptions options) const {
    Type type = getType().subst(module, substitutions, options);
    return getWithType(type->getCanonicalType());
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

  bool operator==(SILResultInfo rhs) const {
    return TypeAndConvention == rhs.TypeAndConvention;
  }
  bool operator!=(SILResultInfo rhs) const {
    return !(*this == rhs);
  }
};
  
class SILFunctionType;
typedef CanTypeWrapper<SILFunctionType> CanSILFunctionType;

// Some macros to aid the SILFunctionType transition.
#if 0
# define SIL_FUNCTION_TYPE_DEPRECATED __attribute__((deprecated))
# define SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_BEGIN \
    _Pragma("clang diagnostic push") \
    _Pragma("clang diagnostic ignored \"-Wdeprecated\"")
# define SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_END \
    _Pragma("clang diagnostic pop")
#else
# define SIL_FUNCTION_TYPE_DEPRECATED
# define SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_BEGIN
# define SIL_FUNCTION_TYPE_IGNORE_DEPRECATED_END
#endif
  
/// SILFunctionType - The detailed type of a function value, suitable
/// for use by SIL.
///
/// This type is defined by the AST library because it must be capable
/// of appearing in secondary positions, e.g. within tuple and
/// function parameter and result types.
class SILFunctionType : public TypeBase, public llvm::FoldingSetNode {
public:
  using Language = SILFunctionLanguage;
  using Representation = SILFunctionTypeRepresentation;

  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // Feel free to rearrange or add bits, but if you go over 15,
    // you'll need to adjust both the Bits field below and
    // BaseType::AnyFunctionTypeBits.

    //   |representation|noReturn|
    //   |    0 .. 3    |   7    |
    //
    enum : uint16_t { RepresentationMask = 0x00F };
    enum : uint16_t { NoReturnMask       = 0x010 };

    uint16_t Bits;

    ExtInfo(unsigned Bits) : Bits(static_cast<uint16_t>(Bits)) {}

    friend class SILFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) { }

    // Constructor for polymorphic type.
    ExtInfo(Representation Rep, bool IsNoReturn) {
      Bits = ((unsigned) Rep) |
             (IsNoReturn ? NoReturnMask : 0);
    }

    bool isNoReturn() const { return Bits & NoReturnMask; }
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
        return false;
      case Representation::ObjCMethod:
      case Representation::Method:
      case Representation::WitnessMethod:
        return true;
      }
    }

    bool hasGuaranteedSelfParam() const {
      switch (getRepresentation()) {
      case Representation::Thick:
      case Representation::Block:
      case Representation::Thin:
      case Representation::CFunctionPointer:
      case Representation::ObjCMethod:
        return false;
      case Representation::Method:
      case Representation::WitnessMethod:
        return true;
      }
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
        return false;
      }
    }
    
    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    ExtInfo withRepresentation(Representation Rep) const {
      return ExtInfo((Bits & ~RepresentationMask)
                     | (unsigned)Rep);
    }
    ExtInfo withIsNoReturn(bool IsNoReturn = true) const {
      if (IsNoReturn)
        return ExtInfo(Bits | NoReturnMask);
      else
        return ExtInfo(Bits & ~NoReturnMask);
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

  CanGenericSignature GenericSig;

  /// TODO: Permit an arbitrary number of results.
  SILResultInfo InterfaceResult;

  MutableArrayRef<SILParameterInfo> getMutableParameters() {
    auto ptr = reinterpret_cast<SILParameterInfo*>(this + 1);
    return MutableArrayRef<SILParameterInfo>(ptr, NumParameters);
  }

  SILResultInfo &getMutableErrorResult() {
    assert(hasErrorResult());
    return *reinterpret_cast<SILResultInfo*>(getMutableParameters().end());
  }

  SILFunctionType(GenericSignature *genericSig, ExtInfo ext,
                  ParameterConvention calleeConvention,
                  ArrayRef<SILParameterInfo> interfaceParams,
                  SILResultInfo interfaceResult,
                  Optional<SILResultInfo> interfaceErrorResult,
                  const ASTContext &ctx,
                  RecursiveTypeProperties properties);

  static SILType getParameterSILType(const SILParameterInfo &param);// SILType.h

public:
  static CanSILFunctionType get(GenericSignature *genericSig,
                                ExtInfo ext,
                                ParameterConvention calleeConvention,
                                ArrayRef<SILParameterInfo> interfaceParams,
                                SILResultInfo interfaceResult,
                                Optional<SILResultInfo> interfaceErrorResult,
                                const ASTContext &ctx);

  // in SILType.h
  SILType getSILResult() const;

  /// Return the convention under which the callee is passed, if this
  /// is a thick non-block callee.
  ParameterConvention getCalleeConvention() const {
    return ParameterConvention(SILFunctionTypeBits.CalleeConvention);
  }
  bool isCalleeConsumed() const {
    return getCalleeConvention() == ParameterConvention::Direct_Owned;
  }

  SILResultInfo getResult() const {
    return InterfaceResult;
  }

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
  
  ArrayRef<SILParameterInfo> getParameters() const {
    return const_cast<SILFunctionType*>(this)->getMutableParameters();
  }

  bool hasIndirectResult() const {
    return !getParameters().empty()
      && getParameters()[0].isIndirectResult();
  }
  SILParameterInfo getIndirectResult() const {
    assert(hasIndirectResult());
    return getParameters()[0];
  }

  /// Returns the 'self' parameter, assuming that this is the type of
  /// a method.
  SILParameterInfo getSelfParameter() const {
    return getParameters().back();
  }

  /// Returns the SILType of the semantic result of this function: the
  /// indirect result type, if there is one, otherwise the direct result.
  ///
  /// In SILType.h.
  SILType getSemanticResultSILType() const;

  /// Get the parameters, ignoring any indirect-result parameter.
  ArrayRef<SILParameterInfo>
  getParametersWithoutIndirectResult() const {
    auto params = getParameters();
    if (hasIndirectResult())
      params = params.slice(1);
    return params;
  }

  using ParameterSILTypeArrayRef
    = ArrayRefView<SILParameterInfo, SILType, getParameterSILType>;  
  ParameterSILTypeArrayRef getParameterSILTypes() const {
    return ParameterSILTypeArrayRef(getParameters());
  }
  
  ParameterSILTypeArrayRef
  getParameterSILTypesWithoutIndirectResult() const {
    return ParameterSILTypeArrayRef(getParametersWithoutIndirectResult());
  }

  bool isPolymorphic() const { return GenericSig != nullptr; }
  CanGenericSignature getGenericSignature() const { return GenericSig; }

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

  bool isNoReturn() const {
    return getExtInfo().isNoReturn();
  }
  
  CanSILFunctionType substGenericArgs(SILModule &silModule,
                                               ModuleDecl *astModule,
                                               ArrayRef<Substitution> subs);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericSignature(), getExtInfo(), getCalleeConvention(),
            getParameters(), getResult(), getOptionalErrorResult());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericSignature *genericSig,
                      ExtInfo info,
                      ParameterConvention calleeConvention,
                      ArrayRef<SILParameterInfo> params,
                      SILResultInfo result,
                      Optional<SILResultInfo> errorResult);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILFunction;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILFunctionType, Type)

class SILBoxType;
typedef CanTypeWrapper<SILBoxType> CanSILBoxType;

/// The SIL-only type @box T, which represents a reference to a (non-class)
/// refcounted heap allocation containing a value of type T.
class SILBoxType : public TypeBase {
  CanType BoxedType;

  SILBoxType(CanType BoxedType)
    : TypeBase(TypeKind::SILBox,
               &BoxedType->getASTContext(),
               BoxedType->getRecursiveProperties()),
      BoxedType(BoxedType) {}

public:
  static CanSILBoxType get(CanType BoxedType);

  CanType getBoxedType() const { return BoxedType; }
  // In SILType.h
  SILType getBoxedAddressType() const;

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILBox;
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
/// The prime examples are arrays (T[] -> Array<T>) and
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
  
/// The type T[], which is always sugar for a library type.
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
  /// Return a uniqued dicitonary type with the specified key and value types.
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
class ProtocolType : public NominalType {
public:
  /// \brief Retrieve the type when we're referencing the given protocol.
  /// declaration.
  static ProtocolType *get(ProtocolDecl *D, const ASTContext &C);

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

  /// Compare two protocols to provide them with a stable ordering for
  /// use in sorting.
  static int compareProtocols(ProtocolDecl * const* PP1,
                              ProtocolDecl * const* PP2);

private:
  friend class NominalTypeDecl;
  ProtocolType(ProtocolDecl *TheDecl, const ASTContext &Ctx);
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
/// var x : protocol<P, Q>
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
  /// \brief Retrieve the name of this type.
  Identifier getName() const;

  /// \brief Retrieve the parent of this type, or null if this is a
  /// primary type.
  SubstitutableType *getParent() const;

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
class ArchetypeType : public SubstitutableType {
public:
  typedef llvm::PointerUnion<AssociatedTypeDecl *, ProtocolDecl *>
    AssocTypeOrProtocolType;
  
  /// A nested type. Either a dependent associated archetype, or a concrete
  /// type (which may be a bound archetype from an outer context).
  class NestedType {
    llvm::PointerIntPair<TypeBase *, 1> TypeAndIsConcrete;
    NestedType(Type type, bool isConcrete)
      : TypeAndIsConcrete(type.getPointer(), isConcrete) {}
  public:
    NestedType() { assert(!*this && "empty nested type isn't false"); }
    static NestedType forArchetype(ArchetypeType *archetype) {
      return { archetype, false };
    }
    static NestedType forConcreteType(Type concrete) {
      return { concrete, true };
    }
    operator bool() const { return TypeAndIsConcrete.getOpaqueValue() != 0; }
    bool isConcreteType() const { return TypeAndIsConcrete.getInt(); }
    Type getValue() const { return TypeAndIsConcrete.getPointer(); }

    /// Check whether this nested type is a concrete type.
    Type getAsConcreteType() const {
      return (isConcreteType() ? getValue() : Type());
    }

    /// Assert that this nested type is a concrete type.
    Type castToConcreteType() const {
      assert(isConcreteType());
      return getValue();
    }

    /// Check whether this nested type is an archetype.
    ArchetypeType *getAsArchetype() const {
      return (isConcreteType() ? nullptr : castToArchetype());
    }

    /// Assert that this nested type is an archetype.
    ArchetypeType *castToArchetype() const {
      assert(!isConcreteType());
      return cast_or_null<ArchetypeType>(TypeAndIsConcrete.getPointer());
    }
  };
  
private:
  ArrayRef<ProtocolDecl *> ConformsTo;
  Type Superclass;

  llvm::PointerUnion<ArchetypeType *, TypeBase *> ParentOrOpened;
  AssocTypeOrProtocolType AssocTypeOrProto;
  Identifier Name;
  unsigned isRecursive: 1;
  ArrayRef<std::pair<Identifier, NestedType>> NestedTypes;

  /// Set the ID number of this opened existential.
  void setOpenedExistentialID(UUID value) {
    assert(getOpenedExistentialType() && "Not an opened existential archetype");
    // The UUID is tail-allocated at the end of opened existential archetypes.
    *reinterpret_cast<UUID *>(this + 1) = value;
  }

public:
  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be copied into the ASTContext by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               AssocTypeOrProtocolType AssocTypeOrProto,
                               Identifier Name, ArrayRef<Type> ConformsTo,
                               Type Superclass,
                               bool isRecursive = false);

  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be minimized then copied into the ASTContext
  /// by this routine.
  static CanTypeWrapper<ArchetypeType>
                        getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               AssocTypeOrProtocolType AssocTypeOrProto,
                               Identifier Name,
                               SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                               Type Superclass,
                               bool isRecursive = false);

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

  /// \brief Retrieve the name of this archetype.
  Identifier getName() const { return Name; }

  /// \brief Retrieve the fully-dotted name that should be used to display this
  /// archetype.
  std::string getFullName() const;

  /// \brief Retrieve the parent of this archetype, or null if this is a
  /// primary archetype.
  ArchetypeType *getParent() const { 
    return ParentOrOpened.dyn_cast<ArchetypeType *>(); 
  }

  /// Retrieve the opened existential type 
  Type getOpenedExistentialType() const {
    return ParentOrOpened.dyn_cast<TypeBase *>();
  }

  /// Retrieve the associated type to which this archetype (if it is a nested
  /// archetype) corresponds.
  ///
  /// This associated type will have the same name as the archetype and will
  /// be a member of one of the protocols to which the parent archetype
  /// conforms.
  AssociatedTypeDecl *getAssocType() const {
    return AssocTypeOrProto.dyn_cast<AssociatedTypeDecl *>();
  }

  /// Retrieve the protocol for which this archetype describes the 'Self'
  /// parameter.
  ProtocolDecl *getSelfProtocol() const {
    return AssocTypeOrProto.dyn_cast<ProtocolDecl *>();
  }
  
  /// True if this is the 'Self' parameter of a protocol or an associated type
  /// of 'Self'.
  bool isSelfDerived() {
    ArchetypeType *t = this;

    do {
      if (t->getSelfProtocol())
        return true;
    } while ((t = t->getParent()));

    return false;
  }

  /// getConformsTo - Retrieve the set of protocols to which this substitutable
  /// type shall conform.
  ArrayRef<ProtocolDecl *> getConformsTo() const { return ConformsTo; }
  
  /// requiresClass - True if the type can only be substituted with class types.
  /// This is true if the type conforms to one or more class protocols or has
  /// a superclass constraint.
  bool requiresClass() const;

  /// \brief Retrieve the superclass of this type, if such a requirement exists.
  Type getSuperclass() const { return Superclass; }

  /// \brief Return true if the archetype has any requirements at all.
  bool hasRequirements() const {
    return !getConformsTo().empty() || getSuperclass();
  }

  /// Retrieve either the associated type or the protocol to which this
  /// associated type corresponds.
  AssocTypeOrProtocolType getAssocTypeOrProtocol() const {
    return AssocTypeOrProto;
  }

  /// \brief Retrieve the nested type with the given name.
  NestedType getNestedType(Identifier Name) const;
  
  Type getNestedTypeValue(Identifier Name) const {
    return getNestedType(Name).getValue();
  }
  
  /// \brief Check if the archetype contains a nested type with the given name.
  bool hasNestedType(Identifier Name) const;

  /// \brief Retrieve the nested types of this archetype.
  ArrayRef<std::pair<Identifier, NestedType>> getNestedTypes() const {
    return NestedTypes;
  }

  /// \brief Set the nested types to a copy of the given array of
  /// archetypes, which will first be sorted in place.
  void setNestedTypes(ASTContext &Ctx,
                      MutableArrayRef<std::pair<Identifier, NestedType>> Nested);

  /// isPrimary - Determine whether this is the archetype for a 'primary'
  /// archetype, e.g., 
  bool isPrimary() const { 
    return ParentOrOpened.isNull();
  }

  /// Retrieve the ID number of this opened existential.
  UUID getOpenedExistentialID() const {
    assert(getOpenedExistentialType() && "Not an opened existential archetype");
    // The UUID is tail-allocated at the end of opened existential archetypes.
    return *reinterpret_cast<const UUID *>(this + 1);
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Archetype;
  }
  
  /// Convert an archetype to a dependent generic parameter type using the
  /// given mapping of primary archetypes to generic parameter types.
  Type getAsDependentType(
                    const llvm::DenseMap<ArchetypeType *, Type> &archetypeMap);
  
  /// getIsRecursive - The archetype type refers back to itself.
  bool getIsRecursive() { return this->isRecursive; }
  
private:
  ArchetypeType(const ASTContext &Ctx, ArchetypeType *Parent,
                AssocTypeOrProtocolType AssocTypeOrProto,
                Identifier Name, ArrayRef<ProtocolDecl *> ConformsTo,
                Type Superclass,
                bool isRecursive = false)
    : SubstitutableType(TypeKind::Archetype, &Ctx,
                        RecursiveTypeProperties::HasArchetype),
      ConformsTo(ConformsTo), Superclass(Superclass), ParentOrOpened(Parent),
      AssocTypeOrProto(AssocTypeOrProto), Name(Name),
      isRecursive(isRecursive) { }

  ArchetypeType(const ASTContext &Ctx, 
                Type Existential,
                ArrayRef<ProtocolDecl *> ConformsTo,
                Type Superclass, bool isRecursive = false)
    : SubstitutableType(TypeKind::Archetype, &Ctx,
                        RecursiveTypeProperties(
                          RecursiveTypeProperties::HasArchetype |
                          RecursiveTypeProperties::HasOpenedExistential)),
      ConformsTo(ConformsTo), Superclass(Superclass),
      ParentOrOpened(Existential.getPointer()),
      isRecursive(isRecursive) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ArchetypeType, SubstitutableType)

/// Abstract class used to describe the type of a generic type parameter
/// or associated type.
///
/// \sa AbstractTypeParamDecl
class AbstractTypeParamType : public SubstitutableType {
protected:
  AbstractTypeParamType(TypeKind kind, const ASTContext *ctx,
                        RecursiveTypeProperties properties)
    : SubstitutableType(kind, ctx, properties) { }

public:
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_AbstractTypeParamType &&
           T->getKind() <= TypeKind::Last_AbstractTypeParamType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(AbstractTypeParamType, SubstitutableType)

/// Describes the type of a generic parameter.
///
/// \sa GenericTypeParamDecl
class GenericTypeParamType : public AbstractTypeParamType {
  /// The generic type parameter or depth/index.
  llvm::PointerUnion<GenericTypeParamDecl *, llvm::Fixnum<31>>
    ParamOrDepthIndex;

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
    : AbstractTypeParamType(TypeKind::GenericTypeParam, nullptr,
                            RecursiveTypeProperties::IsDependent),
      ParamOrDepthIndex(param) { }

  explicit GenericTypeParamType(unsigned depth,
                                unsigned index,
                                const ASTContext &ctx)
    : AbstractTypeParamType(TypeKind::GenericTypeParam, &ctx,
                            RecursiveTypeProperties::IsDependent),
      ParamOrDepthIndex(depth << 16 | index) { }
};
BEGIN_CAN_TYPE_WRAPPER(GenericTypeParamType, AbstractTypeParamType)
  static CanGenericTypeParamType get(unsigned depth, unsigned index,
                                     const ASTContext &C) {
    return CanGenericTypeParamType(GenericTypeParamType::get(depth, index, C));
  }
END_CAN_TYPE_WRAPPER(GenericTypeParamType, AbstractTypeParamType)

/// Describes the type of an associated type.
///
/// \sa AssociatedTypeDecl
class AssociatedTypeType : public AbstractTypeParamType {
  /// The generic type parameter.
  AssociatedTypeDecl *AssocType;

public:
  /// Retrieve the declaration of the associated type.
  AssociatedTypeDecl *getDecl() const { return AssocType; }

  /// Remove one level of top-level sugar from this type.
  TypeBase *getSinglyDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::AssociatedType;
  }

private:
  friend class AssociatedTypeDecl;

  // These aren't classified as dependent for some reason.

  AssociatedTypeType(AssociatedTypeDecl *assocType)
    : AbstractTypeParamType(TypeKind::AssociatedType, nullptr,
                            RecursiveTypeProperties()),
      AssocType(assocType) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(AssociatedTypeType, AbstractTypeParamType)

/// SubstitutedType - A type that has been substituted for some other type,
/// which implies that the replacement type meets all of the requirements of
/// the original type.
class SubstitutedType : public TypeBase {
  // SubstitutedTypes are never canonical.
  explicit SubstitutedType(Type Original, Type Replacement,
                           RecursiveTypeProperties properties)
    : TypeBase(TypeKind::Substituted, nullptr, properties),
      Original(Original), Replacement(Replacement) {}
  
  Type Original;
  Type Replacement;
  
public:
  static SubstitutedType *get(Type Original, Type Replacement,
                              const ASTContext &C);
  
  /// \brief Retrieve the original type that is being replaced.
  Type getOriginal() const { return Original; }
  
  /// \brief Retrieve the replacement type.
  Type getReplacementType() const { return Replacement; }
  
  /// Remove one level of top-level sugar from this type.
  TypeBase *getSinglyDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Substituted;
  }
};

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
  static DependentMemberType *get(Type base, Identifier name,
                                  const ASTContext &ctx);
  static DependentMemberType *get(Type base, AssociatedTypeDecl *assocType,
                                  const ASTContext &ctx);

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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::DependentMember;
  }
};
BEGIN_CAN_TYPE_WRAPPER(DependentMemberType, Type)
  static CanDependentMemberType get(CanType base, AssociatedTypeDecl *assocType,
                                    const ASTContext &C) {
    return CanDependentMemberType(DependentMemberType::get(base, assocType, C));
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
  PROXY_CAN_TYPE_SIMPLE_GETTER(getReferentType)
END_CAN_TYPE_WRAPPER(ReferenceStorageType, Type)

/// \brief The storage type of a variable with [unowned] ownership semantics.
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
DEFINE_EMPTY_CAN_TYPE_WRAPPER(WeakStorageType, ReferenceStorageType)

/// \brief A type variable used during type checking.
class TypeVariableType : public TypeBase {
  TypeVariableType(const ASTContext &C, unsigned ID)
    : TypeBase(TypeKind::TypeVariable, &C,
               RecursiveTypeProperties::HasTypeVariable) {
    TypeVariableTypeBits.ID = ID;
  }

  class Implementation;
  
public:
  
  /// \brief Printing substitutions for type variables may result in recursive
  /// references to the type variable itself. This flag is used to short-circuit
  /// such operations.
  bool isPrinting = false;
  
  /// \brief Create a new type variable whose implementation is constructed
  /// with the given arguments.
  template<typename ...Args>
  static TypeVariableType *getNew(const ASTContext &C, unsigned ID,
                                  Args &&...args);
  
  /// \brief If possible, retrieve the TypeBase object that was opened to create
  /// this type variable.
  TypeBase *getBaseBeingSubstituted();

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

inline NominalTypeDecl *CanType::getAnyNominal() const {
  if (auto nominalTy = dyn_cast<NominalType>(*this))
    return nominalTy->getDecl();

  if (auto boundTy = dyn_cast<BoundGenericType>(*this))
    return boundTy->getDecl();

  if (auto unboundTy = dyn_cast<UnboundGenericType>(*this))
    return unboundTy->getDecl();

  return nullptr;
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
  if (auto tupleTy = type->getAs<TupleType>()) {
    if (tupleTy->getNumElements() == 1 && !tupleTy->getElement(0).isVararg())
      type = tupleTy->getElementType(0);
  }
  
  return type;
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

inline bool TypeBase::mayHaveSuperclass() {
  if (getClassOrBoundGenericClass())
    return true;

  if (auto archetype = getAs<ArchetypeType>())
    return (bool)archetype->requiresClass();

  return is<DynamicSelfType>();
}

inline TupleTypeElt::TupleTypeElt(Type ty,
                                  Identifier name,
                                  DefaultArgumentKind defArg,
                                  bool isVariadic)
  : NameAndVariadic(name, isVariadic),
    TyAndDefaultArg(ty.getPointer(), defArg)
{
  assert(!isVariadic || isa<ArraySliceType>(ty.getPointer()) ||
         (isa<BoundGenericType>(ty.getPointer()) &&
          ty->castTo<BoundGenericType>()->getGenericArgs().size() == 1));
}

inline Type TupleTypeElt::getVarargBaseTy(Type VarArgT) {
  TypeBase *T = VarArgT.getPointer();
  if (ArraySliceType *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  // It's the stdlib Array<T>.
  return cast<BoundGenericType>(T)->getGenericArgs()[0];
}

inline Identifier SubstitutableType::getName() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getName();
  if (auto GenericParam = dyn_cast<GenericTypeParamType>(this))
    return GenericParam->getName();
  if (auto DepMem = dyn_cast<DependentMemberType>(this))
    return DepMem->getName();

  llvm_unreachable("Not a substitutable type");
}

inline SubstitutableType *SubstitutableType::getParent() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getParent();

  return nullptr;
}

inline CanType Type::getCanonicalTypeOrNull() const {
  return isNull() ? CanType() : getPointer()->getCanonicalType();
}

inline bool TypeBase::hasDependentProtocolConformances() {
  return is<SubstitutableType>() || is<GenericTypeParamType>()
      || isAnyExistentialType();
}

#define TYPE(id, parent)
#define SUGARED_TYPE(id, parent) \
template <> \
constexpr bool TypeBase::isSugaredType<id##Type>() { \
  return true; \
}
#include "swift/AST/TypeNodes.def"

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
