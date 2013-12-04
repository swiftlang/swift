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
#include "swift/Basic/Fixnum.h"
#include "swift/Basic/Optional.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMapInfo.h"
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
  class GenericParam;
  class GenericParamList;
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
  class Module;
  class ProtocolConformance;
  class Substitution;

  enum class TypeKind {
#define TYPE(id, parent) id,
#define TYPE_RANGE(Id, FirstId, LastId) \
  First_##Id##Type = FirstId, Last_##Id##Type = LastId,
#include "swift/AST/TypeNodes.def"
  };
  

/// TypeBase - Base class for all types in Swift.
class alignas(8) TypeBase {
  // alignas(8) because we need three tag bits on Type.
  
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
    /// \brief Whether this type has a type variable somewhere in it.
    unsigned HasTypeVariable : 1;
  };

  enum { NumTypeBaseBits = 1 };
  static_assert(NumTypeBaseBits <= 32, "fits in an unsigned");

protected:
  struct AnyFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// Extra information which affects how the function is called, like
    /// regparm and the calling convention.
    unsigned ExtInfo : 8;
  };
  enum { NumAnyFunctionTypeBits = NumTypeBaseBits + 8 };
  static_assert(NumAnyFunctionTypeBits <= 32, "fits in an unsigned");

  struct TypeVariableTypeBitfields {
    unsigned : NumTypeBaseBits;

    /// \brief The unique number assigned to this type variable.
    unsigned ID : 31;
  };
  enum { NumTypeVariableTypeBits = NumTypeBaseBits + 31 };
  static_assert(NumTypeVariableTypeBits <= 32, "fits in an unsigned");

  struct SILFunctionTypeBitfields {
    unsigned : NumTypeBaseBits;
    unsigned ExtInfo : 8;
    unsigned CalleeConvention : 3;
    unsigned NumParameters : 32 - 11 - NumTypeBaseBits;
  };

  union {
    TypeBaseBitfields TypeBaseBits;
    AnyFunctionTypeBitfields AnyFunctionTypeBits;
    TypeVariableTypeBitfields TypeVariableTypeBits;
    SILFunctionTypeBitfields SILFunctionTypeBits;
  };

protected:
  TypeBase(TypeKind kind, const ASTContext *CanTypeCtx, bool HasTypeVariable)
    : CanonicalType((TypeBase*)nullptr), Kind(kind) {
    // If this type is canonical, switch the CanonicalType union to ASTContext.
    if (CanTypeCtx)
      CanonicalType = CanTypeCtx;
    
    setHasTypeVariable(HasTypeVariable);
  }

  /// \brief Mark this type as having a type variable.
  void setHasTypeVariable(bool TV = true) {
    TypeBaseBits.HasTypeVariable = TV;
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
  const ASTContext &getASTContext() {
    // If this type is canonical, it has the ASTContext in it.
    if (CanonicalType.is<const ASTContext *>())
      return *CanonicalType.get<const ASTContext *>();
    // If not, canonicalize it to get the Context.
    return *getCanonicalType()->CanonicalType.get<const ASTContext *>();
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
    return dyn_cast<T>(getDesugaredType());
  }

  template <typename T>
  bool is() {
    return isa<T>(getDesugaredType());
  }
  
  template <typename T>
  T *castTo() {
    return cast<T>(getDesugaredType());
  }

  /// isMaterializable - Is this type 'materializable' according to
  /// the rules of the language?  Basically, does it not contain any
  /// l-value types?
  bool isMaterializable();

  /// hasReferenceSemantics() - Do objects of this type have reference
  /// semantics?
  bool hasReferenceSemantics();
  
  /// allowsOwnership() - Are variables of this type permitted to have
  /// ownership attributes?
  bool allowsOwnership();  

  /// \brief Determine whether this type involves a type variable.
  bool hasTypeVariable() const;

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
  bool isDependentType();

  /// isExistentialType - Determines whether this type is an existential type,
  /// whose real (runtime) type is unknown but which is known to conform to
  /// some set of protocols. Protocol and protocol-conformance types are
  /// existential types.
  bool isExistentialType();
  
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

  /// \brief Determine whether the given type is "specialized", meaning that
  /// it involves generic types for which generic arguments have been provided.
  /// For example, the types Vector<Int> and Vector<Int>.Element are both
  /// specialized, but the type Vector is not.
  bool isSpecialized();

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

  /// \brief Retrieve the type without any default arguments.
  Type getWithoutDefaultArgs(const ASTContext &Context);

  /// getRValueType - For an lvalue type, retrieves the underlying object type.
  /// Otherwise, returns the type itself.
  Type getRValueType();

  /// Retrieves the rvalue instance type, looking through single-element
  /// tuples, lvalue types, and metatypes.
  Type getRValueInstanceType();

  /// isSettableLValue - Returns true if the type is a settable lvalue, or
  /// false if the type is an rvalue or non-settable lvalue.
  bool isSettableLValue();

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
  Type getTypeOfMember(Module *module, const ValueDecl *member,
                       LazyResolver *resolver, Type memberType = Type());


  /// Return T if this type is Optional<T>; otherwise, return the null type.
  Type getOptionalObjectType(const ASTContext &C);

  void dump() const;
  void print(raw_ostream &OS,
             const PrintOptions &PO = PrintOptions()) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;

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
    : TypeBase(TypeKind::Error, &C, /*HasTypeVariable=*/false) { }
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
  : TypeBase(kind, &canTypeCtx, /*HasTypeVariable=*/false) {}
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

/// BuiltinObjectPointerType - The builtin opaque object-pointer type.
/// Useful for keeping an object alive when it is otherwise being
/// manipulated via an unsafe pointer type.
class BuiltinObjectPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinObjectPointerType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinObjectPointer, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinObjectPointer;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinObjectPointerType, BuiltinType);

/// BuiltinObjCPointerType - The builtin opaque Objective-C pointer type.
/// Useful for pushing an Objective-C type through swift.
class BuiltinObjCPointerType : public BuiltinType {
  friend class ASTContext;
  BuiltinObjCPointerType(const ASTContext &C)
    : BuiltinType(TypeKind::BuiltinObjCPointer, C) {}
public:
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::BuiltinObjCPointer;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(BuiltinObjCPointerType, BuiltinType);

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
    : TypeBase(TypeKind::NameAlias, nullptr, /*HasTypeVariable=*/false),
      TheDecl(d) {}
  TypeAliasDecl *const TheDecl;

public:
  TypeAliasDecl *getDecl() const { return TheDecl; }
   
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::NameAlias;
  }
};

/// ParenType - A paren type is a type that's been written in parentheses.
class ParenType : public TypeBase {
  Type UnderlyingType;

  friend class ASTContext;
  ParenType(Type UnderlyingType, bool HasTypeVariable)
    : TypeBase(TypeKind::Paren, nullptr, HasTypeVariable),
      UnderlyingType(UnderlyingType) {}
public:
  Type getUnderlyingType() const { return UnderlyingType; }

  static ParenType *get(const ASTContext &C, Type underlying);
   
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Paren;
  }
};

/// TupleTypeElt - This represents a single element of a tuple.
class TupleTypeElt {
  /// An optional name for the field.
  Identifier Name;

  /// Describes whether this element is variadic or what kind of default
  /// argument it stores.
  enum class DefaultArgOrVarArg : uint8_t {
    /// Neither variadic nor a default argument.
    None,
    /// Variadic.
    VarArg,
    /// It has a normal default argument.
    DefaultArgument,
    /// It has a caller-provided __FILE__ default argument.
    FileArgument,
    /// It has a caller-provided __LINE__ default argument.
    LineArgument,
    /// It has a caller-provided __COLUMN__ default argument.
    ColumnArgument
  };

  /// \brief This is the type of the field, which is mandatory, along with a bit
  /// indicating whether this is a vararg.
  llvm::PointerIntPair<Type, 3, DefaultArgOrVarArg> TyAndDefaultOrVarArg;

  friend class TupleType;

public:
  TupleTypeElt() = default;
  inline /*implicit*/ TupleTypeElt(Type ty,
                                   Identifier name = Identifier(),
                                   DefaultArgumentKind defaultArg =
                                     DefaultArgumentKind::None,
                                   bool isVarArg = false);

  /*implicit*/ TupleTypeElt(TypeBase *Ty)
    : Name(Identifier()), TyAndDefaultOrVarArg(Ty, DefaultArgOrVarArg::None) { }

  bool hasName() const { return !Name.empty(); }
  Identifier getName() const { return Name; }

  Type getType() const { return TyAndDefaultOrVarArg.getPointer(); }

  /// Determine whether this field is variadic.
  bool isVararg() const {
    return TyAndDefaultOrVarArg.getInt() == DefaultArgOrVarArg::VarArg;
  }

  /// Retrieve the kind of default argument available on this field.
  DefaultArgumentKind getDefaultArgKind() const {
    switch (TyAndDefaultOrVarArg.getInt()) {
    case DefaultArgOrVarArg::None:
    case DefaultArgOrVarArg::VarArg:
      return DefaultArgumentKind::None;
    case DefaultArgOrVarArg::DefaultArgument:
      return DefaultArgumentKind::Normal;
    case DefaultArgOrVarArg::FileArgument:
      return DefaultArgumentKind::File;
    case DefaultArgOrVarArg::LineArgument:
      return DefaultArgumentKind::Line;
    case DefaultArgOrVarArg::ColumnArgument:
      return DefaultArgumentKind::Column;
    }
  }

  inline Type getVarargBaseTy() const;

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
  const ArrayRef<TupleTypeElt> Fields;
  
public:
  /// get - Return the uniqued tuple type with the specified elements.
  /// Returns a ParenType instead if there is exactly one element which
  /// is unlabeled and not varargs, so it doesn't accidentally construct
  /// a tuple which is impossible to write.
  static Type get(ArrayRef<TupleTypeElt> Fields, const ASTContext &C);

  /// getEmpty - Return the empty tuple type '()'.
  static CanTypeWrapper<TupleType> getEmpty(const ASTContext &C);

  /// getFields - Return the fields of this tuple.
  ArrayRef<TupleTypeElt> getFields() const { return Fields; }

  unsigned getNumElements() const { return Fields.size(); }
  
  /// getElementType - Return the type of the specified field.
  Type getElementType(unsigned FieldNo) const {
    return Fields[FieldNo].getType();
  }

  TupleEltTypeArrayRef getElementTypes() const {
    return TupleEltTypeArrayRef(getFields());
  };
  
  /// getNamedElementId - If this tuple has a field with the specified name,
  /// return the field index, otherwise return -1.
  int getNamedElementId(Identifier I) const;
  
  /// hasAnyDefaultValues - Return true if any of our elements has a default
  /// value.
  bool hasAnyDefaultValues() const;
  
  /// getFieldForScalarInit - If a tuple of this type can be initialized with a
  /// scalar, return the field number that the scalar is assigned to.  If not,
  /// return -1.
  int getFieldForScalarInit() const;

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Tuple;
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, Fields);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, 
                      ArrayRef<TupleTypeElt> Fields);
  
private:
  TupleType(ArrayRef<TupleTypeElt> fields, const ASTContext *CanCtx,
            bool hasTypeVariable);
};
BEGIN_CAN_TYPE_WRAPPER(TupleType, Type)
  CanType getElementType(unsigned fieldNo) const {
    return CanType(getPointer()->getElementType(fieldNo));
  }
  CanTupleEltTypeArrayRef getElementTypes() const {
    return CanTupleEltTypeArrayRef(getPointer()->getFields());
  };  
END_CAN_TYPE_WRAPPER(TupleType, Type)

/// UnboundGenericType - Represents a generic nominal type where the
/// type arguments have not yet been resolved.
class UnboundGenericType : public TypeBase, public llvm::FoldingSetNode {
  NominalTypeDecl *TheDecl;

  /// \brief The type of the parent, in which this type is nested.
  Type Parent;

private:
  UnboundGenericType(NominalTypeDecl *TheDecl, Type Parent, const ASTContext &C,
                     bool hasTypeVariable)
    : TypeBase(TypeKind::UnboundGeneric,
               (!Parent || Parent->isCanonical())? &C : nullptr,
               hasTypeVariable),
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
                   bool hasTypeVariable);

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

  ArrayRef<Type> getGenericArgs() const { return GenericArgs; }

  /// \brief Retrieve the set of substitutions used to produce this bound
  /// generic type from the underlying generic type.
  ///
  /// \param module The module in which we should compute the substitutions.
  /// FIXME: We currently don't account for this properly, so it can be null.
  ///
  /// \param resolver The resolver that handles lazy type checking, where
  /// required. This can be null for a fully-type-checked AST.
  ArrayRef<Substitution> getSubstitutions(Module *module,
                                          LazyResolver *resolver);

  void Profile(llvm::FoldingSetNodeID &ID) {
    bool hasTypeVariable = false;
    Profile(ID, TheDecl, Parent, GenericArgs, hasTypeVariable);
  }
  static void Profile(llvm::FoldingSetNodeID &ID, NominalTypeDecl *TheDecl,
                      Type Parent, ArrayRef<Type> GenericArgs,
                      bool &hasTypeVariable);

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
                        bool hasTypeVariable)
    : BoundGenericType(TypeKind::BoundGenericClass,
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, hasTypeVariable) {}
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
                        bool hasTypeVariable)
    : BoundGenericType(TypeKind::BoundGenericEnum,
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, hasTypeVariable) {}
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
                         bool hasTypeVariable)
    : BoundGenericType(TypeKind::BoundGenericStruct, 
                       reinterpret_cast<NominalTypeDecl*>(theDecl), parent,
                       genericArgs, context, hasTypeVariable) {}
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
              Type Parent, bool HasTypeVariable)
    : TypeBase(K, (!Parent || Parent->isCanonical())? C : nullptr,
               HasTypeVariable),
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
            bool HasTypeVariable);
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
             bool HasTypeVariable);
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
            bool HasTypeVariable);
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ClassType, NominalType)

/// MetaTypeType - This is the type given to a metatype value.  When a type is
/// declared, a 'metatype' value is injected into the value namespace to
/// resolve references to the type.  An example:
///
///  struct x { ... }  // declares type 'x' and metatype 'x'.
///  x.a()             // use of the metatype value since its a value context.
class MetaTypeType : public TypeBase {
  Type InstanceType;
  
public:
  /// get - Return the MetaTypeType for the specified type declaration.
  static MetaTypeType *get(Type T, const ASTContext &C);

  Type getInstanceType() const { return InstanceType; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::MetaType;
  }
  
private:
  MetaTypeType(Type T, const ASTContext *Ctx, bool HasTypeVariable);
  friend class TypeDecl;
};
BEGIN_CAN_TYPE_WRAPPER(MetaTypeType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getInstanceType)
END_CAN_TYPE_WRAPPER(MetaTypeType, Type)
  
/// ModuleType - This is the type given to a module value, e.g. the "Builtin" in
/// "Builtin.int".  This is typically given to a ModuleExpr, but can also exist
/// on ParenExpr, for example.
class ModuleType : public TypeBase {
  Module *const TheModule;
  
public:
  /// get - Return the ModuleType for the specified module.
  static ModuleType *get(Module *M);

  Module *getModule() const { return TheModule; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return  T->getKind() == TypeKind::Module;
  }
  
private:
  ModuleType(Module *M, const ASTContext &Ctx)
    : TypeBase(TypeKind::Module, &Ctx, // Always canonical
               /*HasTypeVariable=*/false),
      TheModule(M) {
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ModuleType, Type)
  
/// A high-level calling convention.
enum class AbstractCC : unsigned char {
  /// The calling convention used for calling a normal function.
  Freestanding = 0,

  /// The C freestanding calling convention.
  C,
  
  /// The ObjC method calling convention.
  ObjCMethod,

  /// The calling convention used for calling an instance method.
  Method,
  
  Last_AbstractCC = Method,
};
  
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
  /// \brief A class which abstracts out some details necessary for
  /// making a call.
  class ExtInfo {
    // Feel free to rearrange or add bits, but if you go over 7,
    // you'll need to adjust both the Bits field below and
    // BaseType::AnyFunctionTypeBits.

    //   |  CC  |isThin|isAutoClosure|isBlock|noReturn|
    //   |0 .. 3|   4  |      5      |   6   |   7    |
    //
    enum { CallConvMask = 0xF };
    enum { ThinMask = 0x10 };
    enum { AutoClosureMask = 0x20 };
    enum { BlockMask = 0x40 };
    enum { NoReturnMask = 0x80 };

    uint16_t Bits;

    ExtInfo(unsigned Bits) : Bits(static_cast<uint16_t>(Bits)) {}

    friend class AnyFunctionType;
    friend class SILFunctionType;
    
  public:
    // Constructor with all defaults.
    ExtInfo() : Bits(0) {
      assert(getCC() == AbstractCC::Freestanding);
    }

    // Constructor for polymorphic type.
    ExtInfo(AbstractCC CC, bool IsThin, bool IsNoReturn) {
      Bits = ((unsigned) CC) |
             (IsThin ? ThinMask : 0) |
             (IsNoReturn ? NoReturnMask : 0);
    }

    // Constructor with no defaults.
    ExtInfo(AbstractCC CC, bool IsThin, bool IsNoReturn,
            bool IsAutoClosure, bool IsBlock) : ExtInfo(CC, IsThin, IsNoReturn){
      Bits = Bits |
             (IsAutoClosure ? AutoClosureMask : 0) |
             (IsBlock ? BlockMask : 0);
    }

    explicit ExtInfo(AbstractCC CC) : Bits(0) {
      Bits = (Bits & ~CallConvMask) | (unsigned) CC;
    }

    AbstractCC getCC() const { return AbstractCC(Bits & CallConvMask); }
    bool isThin() const { return Bits & ThinMask; }
    bool isNoReturn() const { return Bits & NoReturnMask; }
    bool isAutoClosure() const { return Bits & AutoClosureMask; }
    bool isBlock() const { return Bits & BlockMask; }

    // Note that we don't have setters. That is by design, use
    // the following with methods instead of mutating these objects.
    ExtInfo withCallingConv(AbstractCC CC) const {
      return ExtInfo((Bits & ~CallConvMask) | (unsigned) CC);
    }
    ExtInfo withIsThin(bool IsThin) const {
      if (IsThin)
        return ExtInfo(Bits | ThinMask);
      else
        return ExtInfo(Bits & ~ThinMask);
    }
    ExtInfo withIsNoReturn(bool IsNoReturn) const {
      if (IsNoReturn)
        return ExtInfo(Bits | NoReturnMask);
      else
        return ExtInfo(Bits & ~NoReturnMask);
    }
    ExtInfo withIsAutoClosure(bool IsAutoClosure) const {
      if (IsAutoClosure)
        return ExtInfo(Bits | AutoClosureMask);
      else
        return ExtInfo(Bits & ~AutoClosureMask);
    }
    ExtInfo withIsBlock(bool IsBlock) const {
      if (IsBlock)
        return ExtInfo(Bits | BlockMask);
      else
        return ExtInfo(Bits & ~BlockMask);
    }

    char getFuncAttrKey() const {
      return Bits;
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
                  Type Input, Type Output, bool HasTypeVariable,
                  const ExtInfo &Info)
  : TypeBase(Kind, CanTypeContext, HasTypeVariable),
  Input(Input), Output(Output) {
    AnyFunctionTypeBits.ExtInfo = Info.Bits;
  }

public:

  Type getInput() const { return Input; }
  Type getResult() const { return Output; }

  ExtInfo getExtInfo() const {
    return ExtInfo(AnyFunctionTypeBits.ExtInfo);
  }

  /// \brief Returns the calling conventions of the function.
  AbstractCC getAbstractCC() const {
    return getExtInfo().getCC();
  }
  
  /// \brief True if the function type is "thin", meaning values of the type can
  /// be represented as simple function pointers without context.
  bool isThin() const {
    return getExtInfo().isThin();
  }

  bool isNoReturn() const {
    return getExtInfo().isNoReturn();
  }

  /// \brief True if this type allows an implicit conversion from a function
  /// argument expression of type T to a function of type () -> T.
  bool isAutoClosure() const {
    return getExtInfo().isAutoClosure();
  }
  
  /// \brief True if this type is an Objective-C-compatible block type.
  bool isBlock() const {
    return getExtInfo().isBlock();
  }
  
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
///   var x : @auto_closure () -> int = 4
class FunctionType : public AnyFunctionType {
public:
  /// 'Constructor' Factory Function
  static FunctionType *get(Type Input, Type Result, const ASTContext &C) {
    return get(Input, Result, ExtInfo(), C);
  }

  static FunctionType *get(Type Input, Type Result,
                           const ExtInfo &Info, const ASTContext &C);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Function;
  }
  
private:
  FunctionType(Type Input, Type Result,
               bool HasTypeVariable,
               const ExtInfo &Info);
};
BEGIN_CAN_TYPE_WRAPPER(FunctionType, AnyFunctionType)
  static CanFunctionType get(CanType input, CanType result,
                             const ASTContext &C) {
    return CanFunctionType(FunctionType::get(input, result, C));
  }
  static CanFunctionType get(CanType input, CanType result,
                             const ExtInfo &info,
                             const ASTContext &C) {
    return CanFunctionType(FunctionType::get(input, result, info, C));
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
                                      const ASTContext &C) {
    return get(input, output, params, ExtInfo(), C);
  }

  static PolymorphicFunctionType *get(Type input, Type output,
                                      GenericParamList *params,
                                      const ExtInfo &Info,
                                      const ASTContext &C);

  ArrayRef<GenericParam> getGenericParameters() const;
  ArrayRef<ArchetypeType *> getAllArchetypes() const;

  GenericParamList &getGenericParams() const { return *Params; }

  /// Substitute the given generic arguments into this polymorphic
  /// function type and return the resulting non-polymorphic type.
  FunctionType *substGenericArgs(Module *M, ArrayRef<Type> args);

  /// Substitute the given generic arguments into this polymorphic
  /// function type and return the resulting non-polymorphic type.
  ///
  /// The order of Substitutions must match the order of generic archetypes.
  FunctionType *substGenericArgs(Module *M, ArrayRef<Substitution> subs);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::PolymorphicFunction;
  }
  
private:
  PolymorphicFunctionType(Type input, Type output, GenericParamList *params,
                          const ExtInfo &Info,
                          const ASTContext &C);
};  
BEGIN_CAN_TYPE_WRAPPER(PolymorphicFunctionType, AnyFunctionType)
  static CanPolymorphicFunctionType get(CanType input, CanType result,
                                        GenericParamList *params,
                                        const ExtInfo &info,
                                        const ASTContext &C) {
    return CanPolymorphicFunctionType(
             PolymorphicFunctionType::get(input, result, params, info, C));
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
                            public llvm::FoldingSetNode {
  unsigned NumGenericParams;
  unsigned NumRequirements;

  /// Retrieve a mutable version of the generic parameters.
  MutableArrayRef<GenericTypeParamType *> getGenericParamsBuffer() {
    return { reinterpret_cast<GenericTypeParamType **>(this + 1),
             NumGenericParams };
  }

  /// Retrieve a mutable verison of the requirements.
  MutableArrayRef<Requirement> getRequirementsBuffer() {
    void *genericParams = getGenericParamsBuffer().end();
    return { reinterpret_cast<Requirement *>(genericParams),
             NumRequirements };
  }

  /// Construct a new generic function type.
  GenericFunctionType(ArrayRef<GenericTypeParamType *> genericParams,
                      ArrayRef<Requirement> requirements,
                      Type input,
                      Type result,
                      const ExtInfo &info,
                      const ASTContext *ctx);
public:
  /// Create a new generic function type.
  static GenericFunctionType *get(ArrayRef<GenericTypeParamType *> params,
                                  ArrayRef<Requirement> requirements,
                                  Type input,
                                  Type result,
                                  const ExtInfo &info,
                                  const ASTContext &ctx);

  /// Retrieve the generic parameters of this polymorphic function type.
  ArrayRef<GenericTypeParamType *> getGenericParams() const {
    return { reinterpret_cast<GenericTypeParamType * const *>(this + 1),
             NumGenericParams };
  }

  /// Retrieve the requirements of this polymorphic function type.
  ArrayRef<Requirement> getRequirements() const {
    const void *genericParams = getGenericParams().end();
    return { reinterpret_cast<const Requirement *>(genericParams),
             NumRequirements };
  }

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getRequirements(), getInput(), getResult(),
            getExtInfo());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      ArrayRef<GenericTypeParamType *> params,
                      ArrayRef<Requirement> requirements,
                      Type input,
                      Type result,
                      const ExtInfo &info);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::GenericFunction;
  }
};

DEFINE_EMPTY_CAN_TYPE_WRAPPER(GenericFunctionType, AnyFunctionType)

/// Conventions for passing arguments as parameters.
enum class ParameterConvention {
  /// This argument is passed indirectly, i.e. by directly passing
  /// the address of an object in memory.  The callee is responsible
  /// for destroying the object.  The callee may assume that the
  /// address does not alias any valid object.
  Indirect_In,

  /// This argument is passed indirectly, i.e. by directly passing
  /// the address of an object in memory.  The object is
  /// instantaneously valid on entry, and it must be instantaneously
  /// valid on exit.  The callee may assume that the address does
  /// not alias any valid object.
  Indirect_Inout,

  /// This argument is passed indirectly, i.e. by directly passing
  /// the address of an uninitialized object in memory.  The callee
  /// is responsible for leaving an initialized object at this
  /// address.  The callee may assume that the address does not
  /// alias any valid object.
  Indirect_Out,

  /// This argument is passed directly.  Its type is non-trivial,
  /// and the callee is responsible for destroying it.
  Direct_Owned,

  /// This argument is passed directly.  Its type may be trivial, or
  /// it may simply be that the callee is not responsible for
  /// destroying it.  Its validity is guaranteed only at the instant
  /// the call begins.
  Direct_Unowned,

  /// This argument is passed directly.  Its type is non-trivial, and
  /// the caller guarantees its validity for the entirety of the call.
  Direct_Guaranteed,
};
inline bool isIndirectParameter(ParameterConvention conv) {
  return conv <= ParameterConvention::Indirect_Out;
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
    return false;
  }
  llvm_unreachable("bad convention kind");
}

/// A parameter type and the rules for passing it.
class SILParameterInfo {
  llvm::PointerIntPair<CanType, 3, ParameterConvention> TypeAndConvention;
public:
  SILParameterInfo() = default;
  SILParameterInfo(CanType type, ParameterConvention conv)
    : TypeAndConvention(type, conv) {}

  CanType getType() const {
    return TypeAndConvention.getPointer();
  }
  ParameterConvention getConvention() const {
    return TypeAndConvention.getInt();
  }
  bool isIndirect() const {
    return isIndirectParameter(getConvention());
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

  SILType getSILType() const; // in SILType.h

  /// Transform this SILParameterInfo by applying the user-provided
  /// function to its type.
  template<typename F>
  SILParameterInfo transform(const F &fn) const {
    return SILParameterInfo(fn(getType())->getCanonicalType(), getConvention());
  }

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// The API is comparable to Type::subst.
  SILParameterInfo subst(Module *module, TypeSubstitutionMap &substitutions,
                         bool ignoreMissing, LazyResolver *resolver) const {
    Type type = getType().subst(module, substitutions, ignoreMissing, resolver);
    return SILParameterInfo(type->getCanonicalType(), getConvention());
  }

  void profile(llvm::FoldingSetNodeID &id) {
    id.AddPointer(TypeAndConvention.getOpaqueValue());
  }

  void dump() const;
  void print(llvm::raw_ostream &out,
             const PrintOptions &options = PrintOptions()) const;
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
    : TypeAndConvention(type, conv) {}

  CanType getType() const {
    return TypeAndConvention.getPointer();
  }
  ResultConvention getConvention() const {
    return TypeAndConvention.getInt();
  }
  SILType getSILType() const; // in SILType.h

  /// Transform this SILResultInfo by applying the user-provided
  /// function to its type.
  template<typename F>
  SILResultInfo transform(const F &fn) const {
    return SILResultInfo(fn(getType())->getCanonicalType(), getConvention());
  }

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// The API is comparable to Type::subst.
  SILResultInfo subst(Module *module, TypeSubstitutionMap &substitutions,
                      bool ignoreMissing, LazyResolver *resolver) const {
    Type type = getType().subst(module, substitutions, ignoreMissing, resolver);
    return SILResultInfo(type->getCanonicalType(), getConvention());
  }

  void profile(llvm::FoldingSetNodeID &id) {
    id.AddPointer(TypeAndConvention.getOpaqueValue());
  }

  void dump() const;
  void print(llvm::raw_ostream &out,
             const PrintOptions &options = PrintOptions()) const;
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

/// SILFunctionType - The detailed type of a function value, suitable
/// for use by SIL.
///
/// This type is defined by the AST library because it must be capable
/// of appearing in secondary positions, e.g. within tuple and
/// function parameter and result types.
class SILFunctionType : public TypeBase, public llvm::FoldingSetNode {
public:
  typedef AnyFunctionType::ExtInfo ExtInfo;

private:
  /// TODO: Use the representation from GenericFunctionType.
  GenericParamList *GenericParams;

  /// TODO: Permit an arbitrary number of results.
  const SILResultInfo Result;

  MutableArrayRef<SILParameterInfo> getMutableParameters() {
    auto ptr = reinterpret_cast<SILParameterInfo*>(this + 1);
    unsigned n = SILFunctionTypeBits.NumParameters;
    return MutableArrayRef<SILParameterInfo>(ptr, n);
  }

  SILFunctionType(GenericParamList *genericParams, ExtInfo ext,
                  ParameterConvention calleeConvention,
                  ArrayRef<SILParameterInfo> params, SILResultInfo result,
                  const ASTContext &ctx)
    : TypeBase(TypeKind::SILFunction, &ctx, /*HasTypeVariable*/ false),
      GenericParams(genericParams), Result(result) {
    SILFunctionTypeBits.ExtInfo = ext.Bits;
    SILFunctionTypeBits.NumParameters = params.size();
    assert(!isIndirectParameter(calleeConvention));
    SILFunctionTypeBits.CalleeConvention = unsigned(calleeConvention);
    memcpy(getMutableParameters().data(), params.data(),
           params.size() * sizeof(SILParameterInfo));
  }

  static SILType getParameterSILType(const SILParameterInfo &param);// SILType.h

public:
  static CanSILFunctionType get(GenericParamList *genericParams, ExtInfo ext,
                                ParameterConvention calleeConvention,
                                ArrayRef<SILParameterInfo> params,
                                SILResultInfo result, const ASTContext &ctx);

  SILType getSILResult() const; // in SILType.h
  SILType getSILParameter(unsigned i) const; // in SILType.h

  /// Return the convention under which the callee is passed, if this
  /// is a thick non-block callee.
  ParameterConvention getCalleeConvention() const {
    return ParameterConvention(SILFunctionTypeBits.CalleeConvention);
  }
  bool isCalleeConsumed() const {
    return getCalleeConvention() == ParameterConvention::Direct_Owned;
  }

  SILResultInfo getResult() const {
    return Result;
  }
  ArrayRef<SILParameterInfo> getParameters() const {
    return const_cast<SILFunctionType*>(this)->getMutableParameters();
  }

  bool hasIndirectResult() const {
    return !getParameters().empty() && getParameters()[0].isIndirectResult();
  }
  SILParameterInfo getIndirectResult() const {
    assert(hasIndirectResult());
    return getParameters()[0];
  }

  /// Returns the SILType of the semantic result of this function: the
  /// indirect result type, if there is one, otherwise the direct result.
  SILType getSemanticResultSILType() const; // in SILType.h

  /// Get the parameters, ignoring any indirect-result parameter.
  ArrayRef<SILParameterInfo> getParametersWithoutIndirectResult() const {
    auto params = getParameters();
    if (hasIndirectResult()) params = params.slice(1);
    return params;
  }

  using ParameterSILTypeArrayRef
    = ArrayRefView<SILParameterInfo, SILType, getParameterSILType>;  
  ParameterSILTypeArrayRef getParameterSILTypes() const {
    return ParameterSILTypeArrayRef(getParameters());
  }
  ParameterSILTypeArrayRef getParameterSILTypesWithoutIndirectResult() const {
    return ParameterSILTypeArrayRef(getParametersWithoutIndirectResult());
  }

  bool isPolymorphic() const { return GenericParams != nullptr; }
  GenericParamList *getGenericParams() const { return GenericParams; }

  ExtInfo getExtInfo() const { return ExtInfo(SILFunctionTypeBits.ExtInfo); }

  /// \brief Returns the calling conventions of the function.
  AbstractCC getAbstractCC() const {
    return getExtInfo().getCC();
  }

  /// \brief True if this type is an Objective-C-compatible block type.
  bool isBlock() const {
    return getExtInfo().isBlock();
  }
  
  /// \brief True if the function type is "thin", meaning values of the type can
  /// be represented as simple function pointers without context.
  bool isThin() const {
    return getExtInfo().isThin();
  }

  bool isNoReturn() const {
    return getExtInfo().isNoReturn();
  }

  CanSILFunctionType substGenericArgs(SILModule &silModule, Module *astModule,
                                      ArrayRef<Type> subs);
  CanSILFunctionType substGenericArgs(SILModule &silModule, Module *astModule,
                                      ArrayRef<Substitution> subs);

  void Profile(llvm::FoldingSetNodeID &ID) {
    Profile(ID, getGenericParams(), getExtInfo(), getCalleeConvention(),
            getParameters(), getResult());
  }
  static void Profile(llvm::FoldingSetNodeID &ID,
                      GenericParamList *genericParams,
                      ExtInfo info,
                      ParameterConvention calleeConvention,
                      ArrayRef<SILParameterInfo> params,
                      SILResultInfo result);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::SILFunction;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SILFunctionType, Type)

/// ArrayType - An array type has a base type and either an unspecified or a
/// constant size.  For example "int[]" and "int[4]".  Array types cannot have
/// size = 0.
class ArrayType : public TypeBase {
  const Type Base;
  
  /// Size - When this is zero it indicates an unsized array like "int[]".
  uint64_t Size;
  
public:
  /// 'Constructor' Factory Function.
  static ArrayType *get(Type BaseType, uint64_t Size, const ASTContext &C);

  Type getBaseType() const { return Base; }
  uint64_t getSize() const { return Size; }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Array;
  }
  
private:
  ArrayType(Type Base, uint64_t Size, bool HasTypeVariable);
};
BEGIN_CAN_TYPE_WRAPPER(ArrayType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getBaseType)
END_CAN_TYPE_WRAPPER(ArrayType, Type)

/// A type with a special syntax that is always sugar for a library type.
///
/// The prime examples are slices (T[] -> Slice<T>) and
/// optionals (T? -> Optional<T>).
class SyntaxSugarType : public TypeBase {
  Type Base;
  llvm::PointerUnion<Type, const ASTContext *> ImplOrContext;

protected:
  // Syntax sugar types are never canonical.
  SyntaxSugarType(TypeKind K, const ASTContext &ctx, Type base,
                  bool hasTypeVariable)
    : TypeBase(K, nullptr, hasTypeVariable), Base(base), ImplOrContext(&ctx) {}

public:
  Type getBaseType() const {
    return Base;
  }

  TypeBase *getDesugaredType();

  Type getImplementationType();

  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SyntaxSugarType &&
           T->getKind() <= TypeKind::Last_SyntaxSugarType;
  }
};
  
/// The type T[], which is always sugar for a library type.
class ArraySliceType : public SyntaxSugarType {
  ArraySliceType(const ASTContext &ctx, Type base, bool hasTypeVariable)
    : SyntaxSugarType(TypeKind::ArraySlice, ctx, base, hasTypeVariable) {}

public:
  /// Return a uniqued array slice type with the specified base type.
  static ArraySliceType *get(Type baseTy, const ASTContext &C);

  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::ArraySlice;
  }
};

/// The type T?, which is always sugar for a library type.
class OptionalType : public SyntaxSugarType {
  OptionalType(const ASTContext &ctx,Type base, bool hasTypeVariable)
    : SyntaxSugarType(TypeKind::Optional, ctx, base, hasTypeVariable) {}

public:
  /// Return a uniqued optional type with the specified base type.
  static OptionalType *get(Type baseTy, const ASTContext &C);

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Optional;
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
  
  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Protocol;
  }

  /// Canonicalizes the given set of protocols by eliminating any mentions
  /// of protocols that are already covered by inheritance due to other entries
  /// in the protocol list, then sorting them in some stable order.
  static void canonicalizeProtocols(SmallVectorImpl<ProtocolDecl *> &protocols);

private:
  friend class NominalTypeDecl;
  ProtocolType(ProtocolDecl *TheDecl, const ASTContext &Ctx);
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ProtocolType, NominalType)

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
               /*HasTypeVariable=*/false),
      Protocols(Protocols) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ProtocolCompositionType, Type)

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
/// The type of a function argument may carry an l-value.  This
/// is done by annotating the bound variable with the [inout]
/// attribute.
///
/// The type of a return value, local variable, or field may not
/// carry an l-value.
///
/// When inferring a value type from an expression whose type
/// carries an l-value, the carried l-value types are converted
/// to their object type.
class LValueType : public TypeBase {
public:
  class Qual {
  public:
    typedef unsigned opaque_type;

    enum QualBits : opaque_type {
      // The bits are chosen to make the subtype queries as efficient
      // as possible.  Basically, we want the subtypes to involve
      // fewer bits.

      /// An implicit lvalue is an lvalue that has not been explicitly written
      /// in the source as '&'.
      ///
      /// This qualifier is only used by the (constraint-based) type checker.
      Implicit = 0x1,
      
      /// A non-settable lvalue is an lvalue that cannot be assigned to because
      /// it is a variable with a `get` but no `set` method, a variable with a
      /// non-settable lvalue, or a variable of an rvalue. Non-settable
      /// lvalues cannot be used as the destination of an assignment or as
      /// [inout] arguments.
      NonSettable = 0x2,
      
      /// The default for a [inout] type.
      DefaultForType = 0,

      /// The default for a [inout] 'self' parameter.
      DefaultForInOutSelf = 0,

      /// The default for a variable reference.
      DefaultForVar = Implicit,
  
      /// The default for the base of a member access.
      DefaultForMemberAccess = Implicit,
    };

  private:
    opaque_type Bits;

  public:
    Qual() : Bits(0) {}
    explicit Qual(unsigned bits) : Bits(bits) {}
    Qual(QualBits qual) : Bits(qual) {}

    /// Return an opaque representation of this qualifier set.
    /// The result is hashable by DenseMap.
    opaque_type getOpaqueData() const { return Bits; }

    bool isSettable() const { return !(*this & NonSettable); }
    bool isImplicit() const { return (*this & Implicit); }
    
    friend Qual operator|(QualBits l, QualBits r) {
      return Qual(opaque_type(l) | opaque_type(r));
    }

    /// Union two qualifier sets, given that they are compatible.
    friend Qual operator|(Qual l, Qual r) { return Qual(l.Bits | r.Bits); }

    /// Union a qualifier set into this qualifier set, given that
    /// they are compatible.
    Qual &operator|=(Qual r) { Bits |= r.Bits; return *this; }

    /// Intersect two qualifier sets, given that they are compatible.
    friend QualBits operator&(Qual l, Qual r) {
      // Use QualBits to allow a wider range of conversions to bool.
      return QualBits(l.Bits & r.Bits);
    }

    /// Intersect a qualifier set into this qualifier set.
    Qual &operator&=(Qual r) { Bits &= r.Bits; return *this; }

    /// \brief Remove qualifiers from a qualifier set.
    friend Qual operator-(Qual l, Qual r) {
      return Qual(l.Bits & ~r.Bits);
    }

    /// Invert a qualifier set.  The state of the resulting
    /// non-boolean qualifiers is non-determined, except that they are
    /// is compatible with anything.
    friend Qual operator~(Qual qs) { return Qual(~qs.Bits); }
    friend Qual operator~(QualBits qs) { return Qual(~opaque_type(qs)); }

    /// Are these qualifier sets equivalent?
    friend bool operator==(Qual l, Qual r) { return l.Bits == r.Bits; }
    friend bool operator!=(Qual l, Qual r) { return l.Bits != r.Bits; }

    /// Is one qualifier set 'QL' "smaller than" another set 'QR'?
    /// This corresponds to the subtype relation on lvalue types
    /// for a fixed type T;  that is,
    ///   'QL <= QR' iff 'T [inout(QL)]' <= 'T [inout(QR)]'.
    /// Recall that this means that the first is implicitly convertible
    /// to the latter without "coercion", for some sense of that.
    ///
    /// This is not a total order.
    ///
    /// Right now, the subtyping rules are as follows:
    ///   An l-value type is a subtype of another l-value of the
    ///   same object type except:
    ///   - an implicit l-value is not a subtype of an explicit one.
    ///   - a non-settable lvalue is not a subtype of a settable one.
    friend bool operator<=(Qual l, Qual r) {
      // Right now, all our qualifiers are boolean and independent,
      // and we've set it up so that 1 bits correspond to supertypes.
      // Therefore this is just the set-algebraic 'is subset of'
      // operation and can be performed by intersecting the sets and
      // testing for identity with the left.
      return (l & r) == l;
    }
    friend bool operator<(Qual l, Qual r) { return l != r && l <= r; }
    friend bool operator>(Qual l, Qual r) { return r < l; }
    friend bool operator>=(Qual l, Qual r) { return r <= l; }
  };

private:
  Type ObjectTy;
  Qual Quals; // TODO: put these bits in TypeBase

  LValueType(Type objectTy, Qual quals, const ASTContext *canonicalContext,
             bool hasTypeVariable)
    : TypeBase(TypeKind::LValue, canonicalContext, hasTypeVariable),
      ObjectTy(objectTy), Quals(quals) {}

public:
  static LValueType *get(Type type, Qual quals, const ASTContext &C);

  Type getObjectType() const { return ObjectTy; }
  Qual getQualifiers() const { return Quals; }

  /// Is this lvalue settable?
  bool isSettable() const {
    return getQualifiers().isSettable();
  }

  /// For now, no l-values are ever materializable.  Maybe in the
  /// future we'll make heap l-values materializable.
  bool isMaterializable() const {
    return false;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *type) {
    return type->getKind() == TypeKind::LValue;
  }
};
BEGIN_CAN_TYPE_WRAPPER(LValueType, Type)
  PROXY_CAN_TYPE_SIMPLE_GETTER(getObjectType)
  static CanLValueType get(CanType type, LValueType::Qual quals,
                           const ASTContext &C) {
    return CanLValueType(LValueType::get(type, quals, C));
  }
END_CAN_TYPE_WRAPPER(LValueType, Type)

/// SubstitutableType - A reference to a type that can be substituted, i.e.,
/// an archetype or a generic parameter.
class SubstitutableType : public TypeBase {
  ArrayRef<ProtocolDecl *> ConformsTo;
  Type Superclass;

protected:
  SubstitutableType(TypeKind K, const ASTContext *C, 
                    ArrayRef<ProtocolDecl *> ConformsTo,
                    Type Superclass)
    : TypeBase(K, C, /*HasTypeVariable=*/false),
      ConformsTo(ConformsTo), Superclass(Superclass) { }

public:
  /// \brief Retrieve the name of this type.
  Identifier getName() const;

  /// \brief Retrieve the parent of this type, or null if this is a
  /// primary type.
  SubstitutableType *getParent() const;

  /// \brief Retrieve the archetype corresponding to this substitutable type.
  ArchetypeType *getArchetype();

  // FIXME: Temporary hack.
  bool isPrimary() const;
  unsigned getPrimaryIndex() const;

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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() >= TypeKind::First_SubstitutableType &&
           T->getKind() <= TypeKind::Last_SubstitutableType;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(SubstitutableType, Type)

/// ArchetypeType - An archetype is a type that is a stand-in used to describe
/// type parameters and associated types in generic definition and protocols.
/// Archetypes will be replaced with actual, concrete types at some later
/// point in time, whether it be at compile time due to a direct binding or
/// at run time due to the use of generic types.
class ArchetypeType : public SubstitutableType {
public:
  typedef llvm::PointerUnion<AssociatedTypeDecl *, ProtocolDecl *>
    AssocTypeOrProtocolType;

private:
  ArchetypeType *Parent;
  AssocTypeOrProtocolType AssocTypeOrProto;
  Identifier Name;
  unsigned IndexIfPrimary;
  ArrayRef<std::pair<Identifier, ArchetypeType *>> NestedTypes;
  
public:
  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be copied into the ASTContext by this routine.
  static ArchetypeType *getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               AssocTypeOrProtocolType AssocTypeOrProto,
                               Identifier Name, ArrayRef<Type> ConformsTo,
                               Type Superclass,
                               Optional<unsigned> Index = Optional<unsigned>());

  /// getNew - Create a new archetype with the given name.
  ///
  /// The ConformsTo array will be minimized then copied into the ASTContext
  /// by this routine.
  static ArchetypeType *getNew(const ASTContext &Ctx, ArchetypeType *Parent,
                               AssocTypeOrProtocolType AssocTypeOrProto,
                               Identifier Name,
                               SmallVectorImpl<ProtocolDecl *> &ConformsTo,
                               Type Superclass,
                               Optional<unsigned> Index = Optional<unsigned>());

  /// \brief Retrieve the name of this archetype.
  Identifier getName() const { return Name; }

  /// \brief Retrieve the fully-dotted name that should be used to display this
  /// archetype.
  std::string getFullName() const;

  /// \brief Retrieve the parent of this archetype, or null if this is a
  /// primary archetype.
  ArchetypeType *getParent() const { return Parent; }

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

  /// Retrieve either the associated type or the protocol to which this
  /// associated type corresponds.
  AssocTypeOrProtocolType getAssocTypeOrProtocol() const {
    return AssocTypeOrProto;
  }

  /// \brief Retrieve the nested type with the given name.
  ArchetypeType *getNestedType(Identifier Name) const;

  /// \brief Retrieve the nested types of this archetype.
  ArrayRef<std::pair<Identifier, ArchetypeType *>> getNestedTypes() const {
    return NestedTypes;
  }

  /// \brief Set the nested types to a copy of the given array of
  /// archetypes, which will first be sorted in place.
  void setNestedTypes(ASTContext &Ctx,
         MutableArrayRef<std::pair<Identifier, ArchetypeType *>> Nested);

  /// isPrimary - Determine whether this is the archetype for a 'primary'
  /// archetype, e.g., 
  bool isPrimary() const { return IndexIfPrimary > 0; }

  // getPrimaryIndex - For a primary archetype, return the zero-based index.
  unsigned getPrimaryIndex() const {
    assert(isPrimary() && "Non-primary archetype does not have index");
    return IndexIfPrimary - 1;
  }

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::Archetype;
  }
  
private:
  ArchetypeType(const ASTContext &Ctx, ArchetypeType *Parent,
                AssocTypeOrProtocolType AssocTypeOrProto,
                Identifier Name, ArrayRef<ProtocolDecl *> ConformsTo,
                Type Superclass, Optional<unsigned> Index)
    : SubstitutableType(TypeKind::Archetype, &Ctx, ConformsTo, Superclass),
      Parent(Parent), AssocTypeOrProto(AssocTypeOrProto), Name(Name),
      IndexIfPrimary(Index? *Index + 1 : 0) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(ArchetypeType, SubstitutableType)

/// Abstract class used to describe the type of a generic type parameter
/// or associated type.
///
/// \sa AbstractTypeParamDecl
class AbstractTypeParamType : public SubstitutableType {
protected:
  AbstractTypeParamType(TypeKind kind, const ASTContext *ctx)
    : SubstitutableType(kind, ctx, { }, Type()) { }

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
  llvm::PointerUnion<GenericTypeParamDecl *, Fixnum<31>> ParamOrDepthIndex;

public:
  /// Retrieve a generic type parameter at the given depth and index.
  static GenericTypeParamType *get(unsigned depth, unsigned index,
                                   const ASTContext &ctx);

  /// Retrieve the declaration of the generic type parameter, or null if
  /// there is no such declaration.
  GenericTypeParamDecl *getDecl() const {
    return ParamOrDepthIndex.dyn_cast<GenericTypeParamDecl *>();
  }

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
    : AbstractTypeParamType(TypeKind::GenericTypeParam, nullptr),
      ParamOrDepthIndex(param) { }

  explicit GenericTypeParamType(unsigned depth,
                                unsigned index,
                                const ASTContext &ctx)
    : AbstractTypeParamType(TypeKind::GenericTypeParam, &ctx),
      ParamOrDepthIndex(depth << 16 | index) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(GenericTypeParamType, AbstractTypeParamType)


/// Describes the type of an associated type.
///
/// \sa AssociatedTypeDecl
class AssociatedTypeType : public AbstractTypeParamType {
  /// The generic type parameter.
  AssociatedTypeDecl *AssocType;

public:
  /// Retrieve the declaration of the associated type.
  AssociatedTypeDecl *getDecl() const { return AssocType; }

  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::AssociatedType;
  }

private:
  friend class AssociatedTypeDecl;

  AssociatedTypeType(AssociatedTypeDecl *assocType)
    : AbstractTypeParamType(TypeKind::AssociatedType, nullptr),
      AssocType(assocType) { }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(AssociatedTypeType, AbstractTypeParamType)

/// SubstitutedType - A type that has been substituted for some other type,
/// which implies that the replacement type meets all of the requirements of
/// the original type.
class SubstitutedType : public TypeBase {
  // SubstitutedTypes are never canonical.
  explicit SubstitutedType(Type Original, Type Replacement,
                           bool HasTypeVariable)
    : TypeBase(TypeKind::Substituted, nullptr, HasTypeVariable),
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
  
  /// getDesugaredType - If this type is a sugared type, remove all levels of
  /// sugar until we get down to a non-sugar type.
  TypeBase *getDesugaredType();

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
                      bool hasTypeVariable)
    : TypeBase(TypeKind::DependentMember, ctx, hasTypeVariable),
      Base(base), NameOrAssocType(name) { }

  DependentMemberType(Type base, AssociatedTypeDecl *assocType,
                      const ASTContext *ctx, bool hasTypeVariable)
    : TypeBase(TypeKind::DependentMember, ctx, hasTypeVariable),
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

  // Implement isa/cast/dyncast/etc.
  static bool classof(const TypeBase *T) {
    return T->getKind() == TypeKind::DependentMember;
  }
};
DEFINE_EMPTY_CAN_TYPE_WRAPPER(DependentMemberType, Type)

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
  ReferenceStorageType(TypeKind kind, Type referent, const ASTContext *C)
    : TypeBase(kind, C, false), Referent(referent) {}

private:
  Type Referent;
public:
  static ReferenceStorageType *get(Type referent, Ownership ownership,
                                   const ASTContext &C);

  Type getReferentType() const { return Referent; }
  Ownership getOwnership() const {
    return (getKind() == TypeKind::WeakStorage ? Ownership::Weak
                                               : Ownership::Unowned);
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
  UnownedStorageType(Type referent, const ASTContext *C)
    : ReferenceStorageType(TypeKind::UnownedStorage, referent, C) {}

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
DEFINE_EMPTY_CAN_TYPE_WRAPPER(UnownedStorageType, ReferenceStorageType)

/// \brief The storage type of a variable with [weak] ownership semantics.
class WeakStorageType : public ReferenceStorageType {
  friend class ReferenceStorageType;
  WeakStorageType(Type referent, const ASTContext *C)
    : ReferenceStorageType(TypeKind::WeakStorage, referent, C) {}

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
    : TypeBase(TypeKind::TypeVariable, &C, true) {
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

namespace detail {
  /// An identity function whose sole purpose is to turn a non-dependent value
  /// into a type-dependent value, e.g., to delay type checking for incomplete
  /// types.
  ///
  /// \code
  ///
  /// \endcode
  template<typename Dependent, typename T>
  T &&dependentIdentity(T &&value) {
    return static_cast<T&&>(value);
  }
}

template<typename Pred>
bool Type::findIf(const Pred &pred) const {
  // Check this type node.
  if (pred(*this))
    return true;

  // Recursive into children of this type.
  TypeBase *base = getPointer();
  switch (base->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
    case TypeKind::Error:
    case TypeKind::TypeVariable:
    case TypeKind::AssociatedType:
    case TypeKind::GenericTypeParam:
      return false;

    case TypeKind::Enum:
    case TypeKind::Struct:
    case TypeKind::Class:
      if (auto parentTy = cast<NominalType>(base)->getParent()) {
        return parentTy.findIf(pred);
      }

      return false;

    case TypeKind::WeakStorage:
    case TypeKind::UnownedStorage:
      return cast<ReferenceStorageType>(base)->getReferentType().findIf(pred);

    case TypeKind::SILFunction: {
      auto fnTy = cast<SILFunctionType>(base);
      if (fnTy->getResult().getType().findIf(pred))
        return true;
      for (auto param : fnTy->getParameters())
        if (param.getType().findIf(pred))
          return true;
      return false;
    }

    case TypeKind::UnboundGeneric: {
      if (auto parentTy = cast<UnboundGenericType>(base)->getParent()) {
        return parentTy.findIf(pred);
      }

      return false;
    }

    case TypeKind::BoundGenericClass:
    case TypeKind::BoundGenericEnum:
    case TypeKind::BoundGenericStruct: {
      auto bound = cast<BoundGenericType>(base);

      // Check the parent.
      if (auto parentTy = bound->getParent()) {
        if (parentTy.findIf(pred))
          return true;
      }

      for (auto arg : bound->getGenericArgs()) {
        if (arg.findIf(pred))
          return true;
      }

      return false;
    }

    case TypeKind::MetaType:
      return cast<MetaTypeType>(base)->getInstanceType().findIf(pred);

    case TypeKind::NameAlias: {
      auto dependentDecl
        = detail::dependentIdentity<Pred>(cast<NameAliasType>(base)->getDecl());
      return dependentDecl->getUnderlyingType().findIf(pred);
    }

    case TypeKind::Paren:
      return cast<ParenType>(base)->getUnderlyingType().findIf(pred);

    case TypeKind::Tuple: {
      auto tuple = cast<TupleType>(base);
      for (const auto &elt : tuple->getFields()) {
        if (elt.getType().findIf(pred))
          return true;
      }

      return false;
    }

    case TypeKind::DependentMember:
      return cast<DependentMemberType>(base)->getBase().findIf(pred);

    case TypeKind::Substituted:
      return cast<SubstitutedType>(base)->getReplacementType().findIf(pred);

    case TypeKind::Function:
    case TypeKind::PolymorphicFunction: {
      auto function = cast<AnyFunctionType>(base);
      // FIXME: Polymorphic function's generic parameters and requirements.
      return function->getInput().findIf(pred) ||
             function->getResult().findIf(pred);
    }

    case TypeKind::GenericFunction: {
      auto function = cast<GenericFunctionType>(base);
      for (auto param : function->getGenericParams())
        if (Type(param).findIf(pred))
          return true;
      for (const auto &req : function->getRequirements()) {
        if (req.getFirstType().findIf(pred))
          return true;

        switch (req.getKind()) {
        case RequirementKind::SameType:
        case RequirementKind::Conformance:
          if (req.getSecondType().findIf(pred))
            return true;
          break;

        case RequirementKind::ValueWitnessMarker:
          break;
        }
      }
      return function->getInput().findIf(pred) ||
             function->getResult().findIf(pred);
    }

    case TypeKind::Array:
      return cast<ArrayType>(base)->getBaseType().findIf(pred);

    case TypeKind::ArraySlice:
      return cast<ArraySliceType>(base)->getBaseType().findIf(pred);

    case TypeKind::Optional:
      return cast<OptionalType>(base)->getBaseType().findIf(pred);

    case TypeKind::LValue:
      return cast<LValueType>(base)->getObjectType().findIf(pred);

    case TypeKind::ProtocolComposition: {
      auto pc = cast<ProtocolCompositionType>(base);
      for (auto proto : pc->getProtocols()) {
        if (proto.findIf(pred))
          return true;
      }

      return false;
    }
  }
  
  llvm_unreachable("Unhandled type in transformation");
}

template<typename F>
Type Type::transform(const ASTContext &ctx, const F &fn) const {
  // Transform this type node.
  Type transformed = fn(*this);

  // If the client changed the type, we're done.
  if (!transformed || transformed.getPointer() != getPointer())
    return transformed;

  // Recursive into children of this type.
  TypeBase *base = getPointer();
  switch (base->getKind()) {
#define ALWAYS_CANONICAL_TYPE(Id, Parent) \
case TypeKind::Id:
#define TYPE(Id, Parent)
#include "swift/AST/TypeNodes.def"
  case TypeKind::Error:
  case TypeKind::TypeVariable:
  case TypeKind::AssociatedType:
  case TypeKind::GenericTypeParam:
    return *this;

  case TypeKind::Enum:
  case TypeKind::Struct:
  case TypeKind::Class: {
    auto nominalTy = cast<NominalType>(base);
    if (auto parentTy = nominalTy->getParent()) {
      parentTy = parentTy.transform(ctx, fn);
      if (!parentTy)
        return Type();

      if (parentTy.getPointer() == nominalTy->getParent().getPointer())
        return *this;

      return NominalType::get(nominalTy->getDecl(), parentTy, ctx);
    }

    return *this;
  }

  case TypeKind::SILFunction: {
    auto fnTy = cast<SILFunctionType>(base);
    SILResultInfo origResult = fnTy->getResult();
    Type transResult = origResult.getType().transform(ctx, fn);
    if (!transResult)
      return Type();
    auto canTransResult = transResult->getCanonicalType();
    bool changed = (canTransResult != origResult.getType());

    SmallVector<SILParameterInfo, 8> transParams;
    for (auto origParam : fnTy->getParameters()) {
      Type transParam = origParam.getType().transform(ctx, fn);
      if (!transParam) return Type();

      CanType canTransParam = transParam->getCanonicalType();
      transParams.push_back(SILParameterInfo(canTransParam,
                                             origParam.getConvention()));
      changed = changed || (canTransParam != origParam.getType());
    }

    if (!changed) return *this;

    return SILFunctionType::get(fnTy->getGenericParams(), fnTy->getExtInfo(),
                                fnTy->getCalleeConvention(),
                                transParams,
                                SILResultInfo(canTransResult,
                                              origResult.getConvention()),
                                ctx);
  }

  case TypeKind::UnownedStorage:
  case TypeKind::WeakStorage: {
    auto storageTy = cast<ReferenceStorageType>(base);
    Type refTy = storageTy->getReferentType();
    Type substRefTy = refTy.transform(ctx, fn);
    if (!substRefTy)
      return Type();

    if (substRefTy.getPointer() == refTy.getPointer())
      return *this;

    return ReferenceStorageType::get(substRefTy, storageTy->getOwnership(),
                                     ctx);
  }

  case TypeKind::UnboundGeneric: {
    auto unbound = cast<UnboundGenericType>(base);
    Type substParentTy;
    if (auto parentTy = unbound->getParent()) {
      substParentTy = parentTy.transform(ctx, fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() == parentTy.getPointer())
        return *this;

      return UnboundGenericType::get(unbound->getDecl(), substParentTy, ctx);
    }

    return *this;
  }

  case TypeKind::BoundGenericClass:
  case TypeKind::BoundGenericEnum:
  case TypeKind::BoundGenericStruct: {
    auto bound = cast<BoundGenericType>(base);
    SmallVector<Type, 4> substArgs;
    bool anyChanged = false;
    Type substParentTy;
    if (auto parentTy = bound->getParent()) {
      substParentTy = parentTy.transform(ctx, fn);
      if (!substParentTy)
        return Type();

      if (substParentTy.getPointer() != parentTy.getPointer())
        anyChanged = true;
    }

    for (auto arg : bound->getGenericArgs()) {
      Type substArg = arg.transform(ctx, fn);
      if (!substArg)
        return Type();
      substArgs.push_back(substArg);
      if (substArg.getPointer() != arg.getPointer())
        anyChanged = true;
    }

    if (!anyChanged)
      return *this;

    return BoundGenericType::get(bound->getDecl(), substParentTy, substArgs);
  }

  case TypeKind::MetaType: {
    auto meta = cast<MetaTypeType>(base);
    auto instanceTy = meta->getInstanceType().transform(ctx, fn);
    if (!instanceTy)
      return Type();

    if (instanceTy.getPointer() == meta->getInstanceType().getPointer())
      return *this;

    return MetaTypeType::get(instanceTy, ctx);
  }

  case TypeKind::NameAlias: {
    auto alias = cast<NameAliasType>(base);
    auto dependentDecl = detail::dependentIdentity<F>(alias->getDecl());
    auto underlyingTy = dependentDecl->getUnderlyingType().transform(ctx,fn);
    if (!underlyingTy)
      return Type();

    if (underlyingTy.getPointer()
          == dependentDecl->getUnderlyingType().getPointer())
      return *this;

    return SubstitutedType::get(*this, underlyingTy, ctx);
  }

  case TypeKind::Paren: {
    auto paren = cast<ParenType>(base);
    Type underlying = paren->getUnderlyingType().transform(ctx, fn);
    if (!underlying)
      return Type();

    if (underlying.getPointer() == paren->getUnderlyingType().getPointer())
      return *this;

    return ParenType::get(ctx, underlying);
  }

  case TypeKind::Tuple: {
    auto tuple = cast<TupleType>(base);
    bool anyChanged = false;
    SmallVector<TupleTypeElt, 4> elements;
    unsigned Index = 0;
    for (const auto &elt : tuple->getFields()) {
      Type eltTy = elt.getType().transform(ctx, fn);
      if (!eltTy)
        return Type();

      // If nothing has changd, just keep going.
      if (!anyChanged && eltTy.getPointer() == elt.getType().getPointer()) {
        ++Index;
        continue;
      }

      // If this is the first change we've seen, copy all of the previous
      // elements.
      if (!anyChanged) {
        // Copy all of the previous elements.
        for (unsigned I = 0; I != Index; ++I) {
          const TupleTypeElt &FromElt =tuple->getFields()[I];
          elements.push_back(TupleTypeElt(FromElt.getType(), FromElt.getName(),
                                          FromElt.getDefaultArgKind(),
                                          FromElt.isVararg()));
        }

        anyChanged = true;
      }

      // Add the new tuple element, with the new type, no initializer,
      elements.push_back(TupleTypeElt(eltTy, elt.getName(),
                                      elt.getDefaultArgKind(), elt.isVararg()));
      ++Index;
    }

    if (!anyChanged)
      return *this;

    return TupleType::get(elements, ctx);
  }


  case TypeKind::DependentMember: {
    auto dependent = cast<DependentMemberType>(base);
    auto dependentBase = dependent->getBase().transform(ctx, fn);
    if (!dependentBase)
      return Type();

    if (dependentBase.getPointer() == dependent->getBase().getPointer())
      return *this;

    return DependentMemberType::get(dependentBase, dependent->getName(),
                                    ctx);
  }

  case TypeKind::Substituted: {
    auto substAT = cast<SubstitutedType>(base);
    auto substTy = substAT->getReplacementType().transform(ctx, fn);
    if (!substTy)
      return Type();

    if (substTy.getPointer() == substAT->getReplacementType().getPointer())
      return *this;

    return SubstitutedType::get(substAT->getOriginal(), substTy, ctx);
  }

  case TypeKind::Function:
  case TypeKind::PolymorphicFunction: {
    auto function = cast<AnyFunctionType>(base);
    auto inputTy = function->getInput().transform(ctx, fn);
    if (!inputTy)
      return Type();
    auto resultTy = function->getResult().transform(ctx, fn);
    if (!resultTy)
      return Type();

    if (inputTy.getPointer() == function->getInput().getPointer() &&
        resultTy.getPointer() == function->getResult().getPointer())
      return *this;

    // Function types always parenthesized input types, but some clients
    // (e.g., the type-checker) occasionally perform transformations that
    // don't abide this. Fix up the input type appropriately.
    if (!inputTy->hasTypeVariable() &&
        !isa<ParenType>(inputTy.getPointer()) &&
        !isa<TupleType>(inputTy.getPointer())) {
      inputTy = ParenType::get(ctx, inputTy);
    }

    if (auto polyFn = dyn_cast<PolymorphicFunctionType>(function)) {
      return PolymorphicFunctionType::get(inputTy, resultTy,
                                          &polyFn->getGenericParams(),
                                          function->getExtInfo(),
                                          ctx);
    }

    return FunctionType::get(inputTy, resultTy,
                             function->getExtInfo(),
                             ctx);
  }

  case TypeKind::GenericFunction: {
    GenericFunctionType *function = cast<GenericFunctionType>(base);
    bool anyChanges = false;

    // Transform generic parameters.
    SmallVector<GenericTypeParamType *, 4> genericParams;
    for (auto param : function->getGenericParams()) {
      Type paramTy = Type(param).transform(ctx, fn);
      if (!paramTy)
        return Type();

      if (auto newParam = paramTy->getAs<GenericTypeParamType>()) {
        if (newParam != param)
          anyChanges = true;

        genericParams.push_back(newParam);
      } else {
        anyChanges = true;
      }
    }

    // Transform requirements.
    SmallVector<Requirement, 4> requirements;
    for (const auto &req : function->getRequirements()) {
      auto firstType = req.getFirstType().transform(ctx, fn);
      if (!firstType)
        return Type();

      Type secondType = req.getSecondType();
      if (secondType) {
        secondType = secondType.transform(ctx, fn);
        if (!secondType)
          return Type();
      }

      if (firstType->isDependentType() || secondType->isDependentType()) {
        if (firstType.getPointer() != req.getFirstType().getPointer() ||
            secondType.getPointer() != req.getSecondType().getPointer())
          anyChanges = true;

        requirements.push_back(Requirement(req.getKind(), firstType,
                                           secondType));
      } else
        anyChanges = true;
    }

    // Transform input type.
    auto inputTy = function->getInput().transform(ctx, fn);
    if (!inputTy)
      return Type();

    // Transform result type.
    auto resultTy = function->getResult().transform(ctx, fn);
    if (!resultTy)
      return Type();

    // Check whether anything changed.
    if (!anyChanges &&
        inputTy.getPointer() == function->getInput().getPointer() &&
        resultTy.getPointer() == function->getResult().getPointer())
      return *this;

    // If no generic parameters remain, this is a non-generic function type.
    if (genericParams.empty())
      return FunctionType::get(inputTy, resultTy, function->getExtInfo(), ctx);

    // Produce the new generic function type.
    return GenericFunctionType::get(genericParams, requirements, inputTy,
                                    resultTy, function->getExtInfo(), ctx);
  }

  case TypeKind::Array: {
    auto array = cast<ArrayType>(base);
    auto baseTy = array->getBaseType().transform(ctx, fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == array->getBaseType().getPointer())
      return *this;

    return ArrayType::get(baseTy, array->getSize(), ctx);
  }

  case TypeKind::ArraySlice: {
    auto slice = cast<ArraySliceType>(base);
    auto baseTy = slice->getBaseType().transform(ctx, fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == slice->getBaseType().getPointer())
      return *this;

    return ArraySliceType::get(baseTy, ctx);
  }

  case TypeKind::Optional: {
    auto optional = cast<OptionalType>(base);
    auto baseTy = optional->getBaseType().transform(ctx, fn);
    if (!baseTy)
      return Type();

    if (baseTy.getPointer() == optional->getBaseType().getPointer())
      return *this;

    return OptionalType::get(baseTy, ctx);
  }

  case TypeKind::LValue: {
    auto lvalue = cast<LValueType>(base);
    auto objectTy = lvalue->getObjectType().transform(ctx, fn);
    if (!objectTy)
      return Type();

    if (objectTy.getPointer() == lvalue->getObjectType().getPointer())
      return *this;

    return LValueType::get(objectTy, lvalue->getQualifiers(), ctx);
  }

  case TypeKind::ProtocolComposition: {
    auto pc = cast<ProtocolCompositionType>(base);
    SmallVector<Type, 4> protocols;
    bool anyChanged = false;
    unsigned index = 0;
    for (auto proto : pc->getProtocols()) {
      auto substProto = proto.transform(ctx, fn);
      if (!substProto)
        return Type();
      
      if (anyChanged) {
        protocols.push_back(substProto);
        ++index;
        continue;
      }
      
      if (substProto.getPointer() != proto.getPointer()) {
        anyChanged = true;
        protocols.append(protocols.begin(), protocols.begin() + index);
        protocols.push_back(substProto);
      }
      
      ++index;
    }
    
    if (!anyChanged)
      return *this;
    
    return ProtocolCompositionType::get(ctx, protocols);
  }
  }
  
  llvm_unreachable("Unhandled type in transformation");
}

inline bool TypeBase::hasTypeVariable() const {
  return TypeBaseBits.HasTypeVariable;
}

inline bool TypeBase::isExistentialType() {
  CanType T = getCanonicalType();
  return T->getKind() == TypeKind::Protocol
         || T->getKind() == TypeKind::ProtocolComposition;
}

inline bool TypeBase::isClassExistentialType() {
  CanType T = getCanonicalType();
  if (auto pt = dyn_cast<ProtocolType>(T))
    return pt->requiresClass();
  if (auto pct = dyn_cast<ProtocolCompositionType>(T))
    return pct->requiresClass();
  return false;
}

inline bool TypeBase::isBuiltinIntegerType(unsigned n) {
  if (auto intTy = dyn_cast<BuiltinIntegerType>(getCanonicalType()))
    return intTy->getWidth().isFixedWidth()
      && intTy->getWidth().getFixedWidth() == n;
  return false;
}

inline Type TypeBase::getRValueType() {
  if (!is<LValueType>())
    return this;

  return castTo<LValueType>()->getObjectType();
}

inline bool TypeBase::isSettableLValue() {
  if (!is<LValueType>())
    return false;
  
  return castTo<LValueType>()->isSettable();
}

inline bool TypeBase::mayHaveSuperclass() {
  if (getClassOrBoundGenericClass())
    return true;

  auto archetype = getAs<ArchetypeType>();
  if (!archetype)
    return nullptr;

  return (bool)archetype->requiresClass();
}

inline TupleTypeElt::TupleTypeElt(Type ty,
                                  Identifier name,
                                  DefaultArgumentKind defArg,
                                  bool isVarArg)
  : Name(name), TyAndDefaultOrVarArg(ty.getPointer(),
                                     DefaultArgOrVarArg::None) {
  assert(!isVarArg || isa<ArraySliceType>(ty.getPointer()) ||
         (isa<BoundGenericType>(ty.getPointer()) &&
          ty->castTo<BoundGenericType>()->getGenericArgs().size() == 1));

  if (isVarArg) {
    assert(defArg == DefaultArgumentKind::None && "Defaulted vararg");
    TyAndDefaultOrVarArg.setInt(DefaultArgOrVarArg::VarArg);
  } else {
    switch (defArg) {
    case DefaultArgumentKind::None:
      break;

    case DefaultArgumentKind::Normal:
      TyAndDefaultOrVarArg.setInt(DefaultArgOrVarArg::DefaultArgument);
      break;

    case DefaultArgumentKind::File:
      TyAndDefaultOrVarArg.setInt(DefaultArgOrVarArg::FileArgument);
      break;

    case DefaultArgumentKind::Line:
      TyAndDefaultOrVarArg.setInt(DefaultArgOrVarArg::LineArgument);
      break;

    case DefaultArgumentKind::Column:
      TyAndDefaultOrVarArg.setInt(DefaultArgOrVarArg::ColumnArgument);
      break;
    }
  }
}

inline Type TupleTypeElt::getVarargBaseTy() const {
  TypeBase *T = getType().getPointer();
  if (ArraySliceType *AT = dyn_cast<ArraySliceType>(T))
    return AT->getBaseType();
  // It's the stdlib Slice<T>.
  return cast<BoundGenericType>(T)->getGenericArgs()[0];
}

inline Identifier SubstitutableType::getName() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getName();

  llvm_unreachable("Not a substitutable type");
}

inline SubstitutableType *SubstitutableType::getParent() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getParent();

  return nullptr;
}

inline ArchetypeType *SubstitutableType::getArchetype() {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype;

  llvm_unreachable("Not a substitutable type");
}

inline bool SubstitutableType::isPrimary() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->isPrimary();

  llvm_unreachable("Not a substitutable type");
}

inline unsigned SubstitutableType::getPrimaryIndex() const {
  if (auto Archetype = dyn_cast<ArchetypeType>(this))
    return Archetype->getPrimaryIndex();
  llvm_unreachable("Not a substitutable type");
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
