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
#include <string>

namespace swift {

class ArchetypeType;
class ASTContext;
class LazyResolver;
class Module;
class TypeBase;
class Type;
class SubstitutableType;

/// \brief Type substitution mapping from substitutable types to their
/// replacements.
typedef llvm::DenseMap<SubstitutableType *, Type> TypeSubstitutionMap;

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

  /// Look through the given type and its children to find a type for
  /// which the given predicate returns true.
  ///
  /// \param pred A predicate function object with the signature
  /// \c bool(Type). It should return true if the give type node satisfies the
  /// criteria.
  ///
  /// \returns true if the predicate returns true for the given type or any of
  /// its children.
  template<typename Pred>
  bool findIf(const Pred &pred) const;

  /// Transform the given type by applying the user-provided function to
  /// each type
  ///
  /// This routine applies the given function to transform one type into
  /// another. If the function leaves the type unchanged, recurse into the
  /// child type nodes and transform those. If any child type node changes,
  /// the parent type node will be rebuilt.
  ///
  /// If at any time the function returns a null type, the null will be
  /// propagated out.
  ///
  /// \param ctx The ASTContext in which the type resides.
  ///
  /// \param fn A function object with the signature \c Type(Type), which
  /// accepts a type and returns either a transformed type or a null type.
  ///
  /// \returns the result of transforming the type.
  template<typename F>
  Type transform(const ASTContext &ctx, const F &fn) const;

  /// Replace references to substitutable types with new, concrete types and
  /// return the substituted result.
  ///
  /// \param module The module in which the substitution occurs.
  ///
  /// \param substitutions The mapping from substitutable types to their
  /// replacements.
  ///
  /// \param ignoreMissing Whether we should ignore missing substitutions
  /// (keeping the original type) rather than returning an error.
  ///
  /// \param resolver A lazy resolver used to find member types, form
  /// protocol conformances, etc.
  ///
  /// \returns the substituted type, or a null type if an error occurred.
  Type subst(Module *module, TypeSubstitutionMap &substitutions,
             bool ignoreMissing, LazyResolver *resolver) const;

  void dump() const;

  void print(raw_ostream &OS, const PrintOptions &PO = PrintOptions()) const;

  /// Return the name of the type as a string, for use in diagnostics only.
  std::string getString(const PrintOptions &PO = PrintOptions()) const;

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

  static bool hasReferenceSemanticsImpl(CanType type);
  static bool isExistentialTypeImpl(CanType type);

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
    return hasReferenceSemanticsImpl(*this);
  }

  /// Is this type existential?
  bool isExistentialType() const {
    return isExistentialTypeImpl(*this);
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
//   DEFINE_LEAF_CAN_TYPE_WRAPPER(BuiltinObjectPointer, BuiltinType)
// or
//   BEGIN_CAN_TYPE_WRAPPER(MetaTypeType, Type)
//     PROXY_CAN_TYPE_SIMPLE_GETTER(getInstanceType)
//   END_CAN_TYPE_WRAPPER(MetaTypeType, Type)
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
  operator bool() const { return getPointer() != nullptr; }         \

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

  template<typename T> class PointerLikeTypeTraits;

  // TypeBase* is always at least eight-byte aligned; make the three tag bits
  // available through PointerLikeTypeTraits.
  template<>
  class PointerLikeTypeTraits<swift::TypeBase*> {
  public:
    static inline void *getAsVoidPointer(swift::TypeBase *I) {
      return (void*)I;
    }
    static inline swift::TypeBase *getFromVoidPointer(void *P) {
      return (swift::TypeBase*)P;
    }
    enum { NumLowBitsAvailable = 3 };
  };

  // ArchetypeType* is always at least eight-byte aligned; make the three tag
  // bits available through PointerLikeTypeTraits.
  template<>
  class PointerLikeTypeTraits<swift::ArchetypeType*> {
  public:
    static inline void *getAsVoidPointer(swift::ArchetypeType *I) {
      return (void*)I;
    }
    static inline swift::ArchetypeType *getFromVoidPointer(void *P) {
      return (swift::ArchetypeType*)P;
    }
    enum { NumLowBitsAvailable = 3 };
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
    enum { NumLowBitsAvailable = 3 };
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
