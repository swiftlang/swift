//===--- Type.h - Swift Language Type ASTs ----------------------*- C++ -*-===//
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
// This file defines the Type class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_TYPE_H
#define SWIFT_TYPE_H

#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "swift/Basic/LLVM.h"
#include "swift/AST/PrintOptions.h"
#include "swift/AST/TypeAlignments.h"
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
class Module;
class NominalTypeDecl;
class NormalProtocolConformance;
enum OptionalTypeKind : unsigned;
class ProtocolDecl;
class StructDecl;
class TypeBase;
class Type;
class TypeWalker;

/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<TypeBase *, Type> TypeSubstitutionMap;

/// Options for performing substitutions into a type.
class SubstOptions {
public:
  /// Describes the kind of options.
  enum OptionsKind : unsigned {
    /// If a type cannot be produced because some member type is
    /// missing, return the identity type rather than a null type.
    IgnoreMissing = 0x01,
  };

private:
  unsigned Options = 0;
  NormalProtocolConformance *Conformance = nullptr;

public:
  SubstOptions() { }
  SubstOptions(NoneType) { }
  SubstOptions(OptionsKind options,
               NormalProtocolConformance *conformance = nullptr)
    : Options(options), Conformance(conformance) { }

  /// Whether to ignore missing member types.
  bool ignoreMissing() const { return Options & IgnoreMissing; }

  /// Retrieve the protocol conformance whose not-already-available
  /// type witnesses will be treated as missing.
  NormalProtocolConformance *getSkippedConformance() const {
    return Conformance;
  }

  friend SubstOptions &operator|=(SubstOptions &lhs, const SubstOptions &rhs) {
    lhs.Options |= rhs.Options;
    assert((!lhs.Conformance || !rhs.Conformance ||
            lhs.Conformance == rhs.Conformance) &&
           "skipped protocol conformance collision");
    if (rhs.Conformance)
      lhs.Conformance = rhs.Conformance;
    return lhs;
  }
};

inline SubstOptions::OptionsKind operator|(SubstOptions::OptionsKind lhs,
                                           SubstOptions::OptionsKind rhs) {
  return static_cast<SubstOptions::OptionsKind>(lhs | rhs);
}

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
  bool findIf(const std::function<bool(Type)> &pred) const;

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
  Type transform(const std::function<Type(Type)> &fn) const;

  /// Look through the given type and its children and apply fn to them.
  void visit(const std::function<void (Type)> &fn) const {
    findIf([&fn](Type t) -> bool {
        fn(t);
        return false;
      });
  }

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// \param module The module in which the substitution occurs.
  ///
  /// \param substitutions The mapping from substitutable types to their
  /// replacements.
  ///
  /// \param options Options that affect the substitutions.
  ///
  /// \returns the substituted type, or a null type if an error occurred.
  Type subst(Module *module, TypeSubstitutionMap &substitutions,
             SubstOptions options) const;

  bool isPrivateStdlibType() const;

  void dump() const;
  void dump(raw_ostream &os, unsigned indent = 0) const;

  void print(raw_ostream &OS, const PrintOptions &PO = PrintOptions()) const;
  void print(ASTPrinter &Printer, const PrintOptions &PO) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;

  /// Get the canonical type, or return null if the type is null.
  CanType getCanonicalTypeOrNull() const; // in Types.h
  
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
  static bool isClassReferenceTypeImpl(CanType type);
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

  /// Is this type any kind of existential?
  bool isAnyExistentialType() const {
    return isAnyExistentialTypeImpl(*this);
  }

  /// Is this type any kind of existential?
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
  NominalTypeDecl *getAnyNominal() const; // in Types.h

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
      return swift::CanType(0);
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
} // end namespace llvm

#endif
