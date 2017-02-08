//===--- Type.h - Swift Language Type ASTs ----------------------*- C++ -*-===//
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
// This file defines the Type class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPE_H
#define SWIFT_TYPE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/STLExtras.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/LayoutConstraint.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/TypeAlignments.h"
#include "swift/Basic/OptionSet.h"
#include <functional>
#include <string>

namespace swift {

class ASTPrinter;
class ArchetypeType;
class ClassDecl;
class CanType;
class EnumDecl;
class GenericSignature;
class LazyResolver;
class ModuleDecl;
class NominalTypeDecl;
class GenericTypeDecl;
class NormalProtocolConformance;
enum OptionalTypeKind : unsigned;
class ProtocolConformanceRef;
class ProtocolDecl;
class ProtocolType;
class StructDecl;
class SubstitutableType;
class SubstitutionMap;
class TypeBase;
class Type;
class TypeWalker;

/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<SubstitutableType *, Type> TypeSubstitutionMap;

/// Function used to provide substitutions.
///
/// Returns a null \c Type to indicate that there is no substitution for
/// this substitutable type; otherwise, the replacement type.
using TypeSubstitutionFn
  = llvm::function_ref<Type(SubstitutableType *dependentType)>;

/// A function object suitable for use as a \c TypeSubstitutionFn that
/// queries an underlying \c TypeSubstitutionMap.
struct QueryTypeSubstitutionMap {
  const TypeSubstitutionMap &substitutions;

  Type operator()(SubstitutableType *type) const;
};

/// A function object suitable for use as a \c TypeSubstitutionFn that
/// queries an underlying \c SubstitutionMap.
struct QuerySubstitutionMap {
  const SubstitutionMap &subMap;

  Type operator()(SubstitutableType *type) const;
};

/// Function used to resolve conformances.
using GenericFunction = auto(CanType dependentType,
  Type conformingReplacementType,
  ProtocolType *conformedProtocol)
  ->Optional<ProtocolConformanceRef>;
using LookupConformanceFn = llvm::function_ref<GenericFunction>;
  
/// Functor class suitable for use as a \c LookupConformanceFn to look up a
/// conformance through a module.
class LookUpConformanceInModule {
  ModuleDecl *M;
public:
  explicit LookUpConformanceInModule(ModuleDecl *M)
    : M(M) {}
  
  Optional<ProtocolConformanceRef>
  operator()(CanType dependentType,
             Type conformingReplacementType,
             ProtocolType *conformedProtocol) const;
};

/// Functor class suitable for use as a \c LookupConformanceFn to look up a
/// conformance in a \c SubstitutionMap.
class LookUpConformanceInSubstitutionMap {
  const SubstitutionMap &Subs;
public:
  explicit LookUpConformanceInSubstitutionMap(const SubstitutionMap &Subs)
    : Subs(Subs) {}
  
  Optional<ProtocolConformanceRef>
  operator()(CanType dependentType,
             Type conformingReplacementType,
             ProtocolType *conformedProtocol) const;
};

/// Functor class suitable for use as a \c LookupConformanceFn that provides
/// only abstract conformances for generic types. Asserts that the replacement
/// type is an opaque generic type.
class MakeAbstractConformanceForGenericType {
public:
  Optional<ProtocolConformanceRef>
  operator()(CanType dependentType,
             Type conformingReplacementType,
             ProtocolType *conformedProtocol) const;
};

/// Functor class suitable for use as a \c LookupConformanceFn that fetches
/// conformances from a generic signature.
class LookUpConformanceInSignature {
  const GenericSignature &Sig;
public:
  LookUpConformanceInSignature(const GenericSignature &Sig)
    : Sig(Sig) {}
  
  Optional<ProtocolConformanceRef>
  operator()(CanType dependentType,
             Type conformingReplacementType,
             ProtocolType *conformedProtocol) const;
};
  
/// Flags that can be passed when substituting into a type.
enum class SubstFlags {
  /// If a type cannot be produced because some member type is
  /// missing, place an 'error' type into the position of the base.
  UseErrorType = 0x01,
  /// Allow substitutions to recurse into SILFunctionTypes.
  /// Normally, SILType::subst() should be used for lowered
  /// types, however in special cases where the substitution
  /// is just changing between contextual and interface type
  /// representations, using Type::subst() is allowed.
  AllowLoweredTypes = 0x02,
  /// Map member types to their desugared witness type.
  DesugarMemberTypes = 0x04,
};

/// Options for performing substitutions into a type.
typedef OptionSet<SubstFlags> SubstOptions;

inline SubstOptions operator|(SubstFlags lhs, SubstFlags rhs) {
  return SubstOptions(lhs) | rhs;
}

/// Enumeration describing foreign languages to which Swift may be
/// bridged.
enum class ForeignLanguage {
  C,
  ObjectiveC,
};

/// Describes how a particular type is representable in a foreign language.
enum class ForeignRepresentableKind : uint8_t {
  /// This type is not representable in the foreign language.
  None,
  /// This type is trivially representable in the foreign language.
  Trivial,
  /// This type is trivially representable as an object in the foreign
  /// language.
  Object,
  /// This type is representable in the foreign language via bridging.
  Bridged,
  /// This type is representable in the foreign language via bridging
  /// of Error.
  BridgedError,
  /// This type is representable in the foreign language via static
  /// bridging code, only (which is not available at runtime).
  StaticBridged,
};

/// Type - This is a simple value object that contains a pointer to a type
/// class.  This is potentially sugared.  We use this throughout the codebase
/// instead of a raw "TypeBase*" to disable equality comparison, which is unsafe
/// for sugared types.
class Type {
  TypeBase *Ptr;
public:
  /*implicit*/ Type(TypeBase *P = 0) : Ptr(P) {}
  
  TypeBase *getPointer() const { return Ptr; }
  
  bool isNull() const { return Ptr == 0; }
  
  TypeBase *operator->() const { return Ptr; }
  
  explicit operator bool() const { return Ptr != 0; }

  /// Walk this type.
  ///
  /// Returns true if the walk was aborted.
  bool walk(TypeWalker &walker) const;
  bool walk(TypeWalker &&walker) const {
    return walk(walker);
  }

  /// Look through the given type and its children to find a type for
  /// which the given predicate returns true.
  ///
  /// \param pred A predicate function object. It should return true if the give
  /// type node satisfies the criteria.
  ///
  /// \returns true if the predicate returns true for the given type or any of
  /// its children.
  bool findIf(llvm::function_ref<bool(Type)> pred) const;

  /// Transform the given type by applying the user-provided function to
  /// each type.
  ///
  /// This routine applies the given function to transform one type into
  /// another. If the function leaves the type unchanged, recurse into the
  /// child type nodes and transform those. If any child type node changes,
  /// the parent type node will be rebuilt.
  ///
  /// If at any time the function returns a null type, the null will be
  /// propagated out.
  ///
  /// \param fn A function object with the signature \c Type(Type), which
  /// accepts a type and returns either a transformed type or a null type.
  ///
  /// \returns the result of transforming the type.
  Type transform(llvm::function_ref<Type(Type)> fn) const;

  /// Transform the given type by applying the user-provided function to
  /// each type.
  ///
  /// This routine applies the given function to transform one type into
  /// another. If the function leaves the type unchanged, recurse into the
  /// child type nodes and transform those. If any child type node changes,
  /// the parent type node will be rebuilt.
  ///
  /// If at any time the function returns a null type, the null will be
  /// propagated out.
  ///
  /// If the function returns \c None, the transform operation will
  ///
  /// \param fn A function object with the signature
  /// \c Optional<Type>(TypeBase *), which accepts a type pointer and returns a
  /// transformed type, a null type (which will propagate the null type to the
  /// outermost \c transform() call), or None (to indicate that the transform
  /// operation should recursively transform the subtypes). The function object
  /// should use \c dyn_cast rather \c getAs, because the transform itself
  /// handles desugaring.
  ///
  /// \returns the result of transforming the type.
  Type transformRec(llvm::function_ref<Optional<Type>(TypeBase *)> fn) const;

  /// Look through the given type and its children and apply fn to them.
  void visit(llvm::function_ref<void (Type)> fn) const {
    findIf([&fn](Type t) -> bool {
        fn(t);
        return false;
      });
  }

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// \param substitutions The mapping from substitutable types to their
  /// replacements and conformances.
  ///
  /// \param options Options that affect the substitutions.
  ///
  /// \returns the substituted type, or a null type if an error occurred.
  Type subst(const SubstitutionMap &substitutions,
             SubstOptions options = None) const;

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// \param substitutions A function mapping from substitutable types to their
  /// replacements.
  ///
  /// \param conformances A function for looking up conformances.
  ///
  /// \param options Options that affect the substitutions.
  ///
  /// \returns the substituted type, or a null type if an error occurred.
  Type subst(TypeSubstitutionFn substitutions,
             LookupConformanceFn conformances,
             SubstOptions options = None) const;

  /// Replace references to substitutable types with error types.
  Type substDependentTypesWithErrorTypes() const;

  bool isPrivateStdlibType(bool whitelistProtocols=true) const;

  void dump() const;
  void dump(raw_ostream &os, unsigned indent = 0) const;

  void print(raw_ostream &OS, const PrintOptions &PO = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &PO) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;

  /// Computes the join between two types.
  ///
  /// The join of two types is the most specific type that is a supertype of
  /// both \c type1 and \c type2, e.g., the least upper bound in the type
  /// lattice. For example, given a simple class hierarchy as follows:
  ///
  /// \code
  /// class A { }
  /// class B : A { }
  /// class C : A { }
  /// class D { }
  /// \endcode
  ///
  /// The join of B and C is A, the join of A and B is A. However, there is no
  /// join of D and A (or D and B, or D and C) because there is no common
  /// superclass. One would have to jump to an existential (e.g., \c AnyObject)
  /// to find a common type.
  /// 
  /// \returns the join of the two types, if there is a concrete type that can
  /// express the join, or a null type if the only join would be a more-general
  /// existential type (e.g., \c Any).
  static Type join(Type type1, Type type2);

private:
  // Direct comparison is disabled for types, because they may not be canonical.
  void operator==(Type T) const = delete;
  void operator!=(Type T) const = delete;
};

/// CanType - This is a Type that is statically known to be canonical.  To get
/// one of these, use Type->getCanonicalType().  Since all CanType's can be used
/// as 'Type' (they just don't have sugar) we derive from Type.
class CanType : public Type {
  bool isActuallyCanonicalOrNull() const;

  static bool isReferenceTypeImpl(CanType type, bool functionsCount);
  static bool isExistentialTypeImpl(CanType type);
  static bool isAnyExistentialTypeImpl(CanType type);
  static bool isExistentialTypeImpl(CanType type,
                                    SmallVectorImpl<ProtocolDecl*> &protocols);
  static bool isAnyExistentialTypeImpl(CanType type,
                                    SmallVectorImpl<ProtocolDecl*> &protocols);
  static void getAnyExistentialTypeProtocolsImpl(CanType type,
                                    SmallVectorImpl<ProtocolDecl*> &protocols);
  static bool isObjCExistentialTypeImpl(CanType type);
  static CanType getAnyOptionalObjectTypeImpl(CanType type,
                                              OptionalTypeKind &kind);
  static CanType getReferenceStorageReferentImpl(CanType type);
  static CanType getLValueOrInOutObjectTypeImpl(CanType type);
  static ClassDecl *getClassBoundImpl(CanType type);

public:
  explicit CanType(TypeBase *P = 0) : Type(P) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }
  explicit CanType(Type T) : Type(T) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }

  // Provide a few optimized accessors that are really type-class queries.

  /// Do values of this type have reference semantics?
  bool hasReferenceSemantics() const {
    return isReferenceTypeImpl(*this, /*functions count*/ true);
  }

  /// Are values of this type essentially just class references,
  /// possibly with some extra metadata?
  ///
  ///   - any of the builtin reference types
  ///   - a class type
  ///   - a bound generic class type
  ///   - a class-bounded archetype type
  ///   - a class-bounded existential type
  ///   - a dynamic Self type
  bool isAnyClassReferenceType() const {
    return isReferenceTypeImpl(*this, /*functions count*/ false);
  }

  /// Is this type existential?
  bool isExistentialType() const {
    return isExistentialTypeImpl(*this);
  }

  /// Is this type existential?
  bool isExistentialType(SmallVectorImpl<ProtocolDecl *> &protocols) {
    return isExistentialTypeImpl(*this, protocols);
  }

  /// Is this type an existential or an existential metatype?
  bool isAnyExistentialType() const {
    return isAnyExistentialTypeImpl(*this);
  }

  /// Is this type an existential or an existential metatype?
  bool isAnyExistentialType(SmallVectorImpl<ProtocolDecl *> &protocols) {
    return isAnyExistentialTypeImpl(*this, protocols);
  }

  /// Given that this type is any kind of existential, return its
  /// protocols in a canonical order.
  void getAnyExistentialTypeProtocols(
                                SmallVectorImpl<ProtocolDecl *> &protocols) {
    return getAnyExistentialTypeProtocolsImpl(*this, protocols);
  }

  /// Is this an ObjC-compatible existential type?
  bool isObjCExistentialType() const {
    return isObjCExistentialTypeImpl(*this);
  }

  ClassDecl *getClassOrBoundGenericClass() const; // in Types.h
  StructDecl *getStructOrBoundGenericStruct() const; // in Types.h
  EnumDecl *getEnumOrBoundGenericEnum() const; // in Types.h
  NominalTypeDecl *getNominalOrBoundGenericNominal() const; // in Types.h
  CanType getNominalParent() const; // in Types.h
  NominalTypeDecl *getAnyNominal() const;
  GenericTypeDecl *getAnyGeneric() const;

  /// Returns information about the layout constraint represented by
  /// this type. If this type does not represent a layout constraint,
  /// it returns an empty LayoutConstraint.
  LayoutConstraint getLayoutConstraint() const;

  /// \brief Retrieve the most-specific class bound of this type,
  /// which is either a class, a bound-generic class, or a class-bounded
  /// archetype.
  ///
  /// Returns nil if this is an archetype with a non-specific class bound.
  ClassDecl *getClassBound() const {
    return getClassBoundImpl(*this);
  }

  CanType getAnyOptionalObjectType() const {
    OptionalTypeKind kind;
    return getAnyOptionalObjectTypeImpl(*this, kind);
  }

  CanType getAnyOptionalObjectType(OptionalTypeKind &kind) const {
    return getAnyOptionalObjectTypeImpl(*this, kind);
  }

  CanType getReferenceStorageReferent() const {
    return getReferenceStorageReferentImpl(*this);
  }
  
  CanType getLValueOrInOutObjectType() const {
    return getLValueOrInOutObjectTypeImpl(*this);
  }

  // Direct comparison is allowed for CanTypes - they are known canonical.
  bool operator==(CanType T) const { return getPointer() == T.getPointer(); }
  bool operator!=(CanType T) const { return !operator==(T); }

  bool operator<(CanType T) const { return getPointer() < T.getPointer(); }
};

template <class Proxied> class CanTypeWrapper;

// Define a database of CanType wrapper classes for ease of metaprogramming.
// By definition, this maps 'FOO' to 'CanFOO'.
template <class Proxied> struct CanTypeWrapperTraits;
template <> struct CanTypeWrapperTraits<Type> { typedef CanType type; };

// A wrapper which preserves the fact that a type is canonical.
//
// Intended to be used as follows:
//   DEFINE_LEAF_CAN_TYPE_WRAPPER(BuiltinNativeObject, BuiltinType)
// or
//   BEGIN_CAN_TYPE_WRAPPER(MetatypeType, Type)
//     PROXY_CAN_TYPE_SIMPLE_GETTER(getInstanceType)
//   END_CAN_TYPE_WRAPPER(MetatypeType, Type)
#define BEGIN_CAN_TYPE_WRAPPER(TYPE, BASE)                          \
typedef CanTypeWrapper<TYPE> Can##TYPE;                             \
template <>                                                         \
class CanTypeWrapper<TYPE> : public Can##BASE {                     \
public:                                                             \
  explicit CanTypeWrapper(TYPE *theType = nullptr)                  \
    : Can##BASE(theType) {}                                         \
                                                                    \
  TYPE *getPointer() const {                                        \
    return static_cast<TYPE*>(Type::getPointer());                  \
  }                                                                 \
  TYPE *operator->() const { return getPointer(); }                 \
  operator TYPE *() const { return getPointer(); }                  \
  explicit operator bool() const { return getPointer() != nullptr; }

#define PROXY_CAN_TYPE_SIMPLE_GETTER(METHOD)                        \
  CanType METHOD() const { return CanType(getPointer()->METHOD()); }

#define END_CAN_TYPE_WRAPPER(TYPE, BASE)                            \
};                                                                  \
template <> struct CanTypeWrapperTraits<TYPE> {                     \
  typedef Can##TYPE type;                                           \
};

#define DEFINE_EMPTY_CAN_TYPE_WRAPPER(TYPE, BASE)                   \
BEGIN_CAN_TYPE_WRAPPER(TYPE, BASE)                                  \
END_CAN_TYPE_WRAPPER(TYPE, BASE)

// Disallow direct uses of isa/cast/dyn_cast on Type to eliminate a
// certain class of bugs.
template <class X> inline bool
isa(const Type&) = delete; // Use TypeBase::is instead.
template <class X> inline typename llvm::cast_retty<X, Type>::ret_type
cast(const Type&) = delete; // Use TypeBase::castTo instead.
template <class X> inline typename llvm::cast_retty<X, Type>::ret_type
dyn_cast(const Type&) = delete; // Use TypeBase::getAs instead.
template <class X> inline typename llvm::cast_retty<X, Type>::ret_type
dyn_cast_or_null(const Type&) = delete;

// Permit direct uses of isa/cast/dyn_cast on CanType and preserve
// canonicality.
template <class X> inline bool isa(CanType type) {
  return isa<X>(type.getPointer());
}
template <class X> inline CanTypeWrapper<X> cast(CanType type) {
  return CanTypeWrapper<X>(cast<X>(type.getPointer()));
}
template <class X> inline CanTypeWrapper<X> cast_or_null(CanType type) {
  return CanTypeWrapper<X>(cast_or_null<X>(type.getPointer()));
}
template <class X> inline CanTypeWrapper<X> dyn_cast(CanType type) {
  return CanTypeWrapper<X>(dyn_cast<X>(type.getPointer()));
}
template <class X> inline CanTypeWrapper<X> dyn_cast_or_null(CanType type) {
  return CanTypeWrapper<X>(dyn_cast_or_null<X>(type.getPointer()));
}

// Permit direct uses of isa/cast/dyn_cast on CanTypeWrapper<T> and
// preserve canonicality.
template <class X, class P>
inline bool isa(CanTypeWrapper<P> type) {
  return isa<X>(type.getPointer());
}
template <class X, class P>
inline CanTypeWrapper<X> cast(CanTypeWrapper<P> type) {
  return CanTypeWrapper<X>(cast<X>(type.getPointer()));
}
template <class X, class P>
inline CanTypeWrapper<X> dyn_cast(CanTypeWrapper<P> type) {
  return CanTypeWrapper<X>(dyn_cast<X>(type.getPointer()));
}
template <class X, class P>
inline CanTypeWrapper<X> dyn_cast_or_null(CanTypeWrapper<P> type) {
  return CanTypeWrapper<X>(dyn_cast_or_null<X>(type.getPointer()));
}
  
class GenericTypeParamType;
  
/// A reference to a canonical generic signature.
class CanGenericSignature {
  GenericSignature *Signature;
  
public:
  CanGenericSignature() : Signature(nullptr) {}
  CanGenericSignature(std::nullptr_t) : Signature(nullptr) {}
  
  // in Decl.h
  explicit CanGenericSignature(GenericSignature *Signature);
  ArrayRef<CanTypeWrapper<GenericTypeParamType>> getGenericParams() const;

  /// Retrieve the canonical generic environment associated with this
  /// generic signature.
  GenericEnvironment *getGenericEnvironment(ModuleDecl &module) const;

  GenericSignature *operator->() const {
    return Signature;
  }
  
  operator GenericSignature *() const {
    return Signature;
  }
  
  GenericSignature *getPointer() const {
    return Signature;
  }
};

} // end namespace swift

namespace llvm {
  static inline raw_ostream &
  operator<<(raw_ostream &OS, swift::Type Ty) {
    Ty.print(OS);
    return OS;
  }

  // A Type casts like a TypeBase*.
  template<> struct simplify_type<const ::swift::Type> {
    typedef ::swift::TypeBase *SimpleType;
    static SimpleType getSimplifiedValue(const ::swift::Type &Val) {
      return Val.getPointer();
    }
  };
  template<> struct simplify_type< ::swift::Type>
    : public simplify_type<const ::swift::Type> {};
  
  // Type hashes just like pointers.
  template<> struct DenseMapInfo<swift::Type> {
    static swift::Type getEmptyKey() {
      return llvm::DenseMapInfo<swift::TypeBase*>::getEmptyKey();
    }
    static swift::Type getTombstoneKey() {
      return llvm::DenseMapInfo<swift::TypeBase*>::getTombstoneKey();
    }
    static unsigned getHashValue(swift::Type Val) {
      return DenseMapInfo<swift::TypeBase*>::getHashValue(Val.getPointer());
    }
    static bool isEqual(swift::Type LHS, swift::Type RHS) {
      return LHS.getPointer() == RHS.getPointer();
    }
  };
  template<> struct DenseMapInfo<swift::CanType>
    : public DenseMapInfo<swift::Type> {
    static swift::CanType getEmptyKey() {
      return swift::CanType(nullptr);
    }
    static swift::CanType getTombstoneKey() {
      return swift::CanType(llvm::DenseMapInfo<swift::
                              TypeBase*>::getTombstoneKey());
    }
  };

  // A Type is "pointer like".
  template<>
  class PointerLikeTypeTraits<swift::Type> {
  public:
    static inline void *getAsVoidPointer(swift::Type I) {
      return (void*)I.getPointer();
    }
    static inline swift::Type getFromVoidPointer(void *P) {
      return (swift::TypeBase*)P;
    }
    enum { NumLowBitsAvailable = swift::TypeAlignInBits };
  };
  
  template<>
  class PointerLikeTypeTraits<swift::CanType> :
    public PointerLikeTypeTraits<swift::Type> {
  public:
    static inline swift::CanType getFromVoidPointer(void *P) {
      return swift::CanType((swift::TypeBase*)P);
    }
  };

  template<>
  class PointerLikeTypeTraits<swift::CanGenericSignature> {
  public:
    static inline swift::CanGenericSignature getFromVoidPointer(void *P) {
      return swift::CanGenericSignature((swift::GenericSignature*)P);
    }
    static inline void *getAsVoidPointer(swift::CanGenericSignature S) {
      return (void*)S.getPointer();
    }
    enum { NumLowBitsAvailable = swift::TypeAlignInBits };
  };
  
} // end namespace llvm

#endif
