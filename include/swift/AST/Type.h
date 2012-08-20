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

#include "llvm/ADT/DenseMapInfo.h"
#include "swift/Basic/LLVM.h"
#include <string>

namespace swift {
  class TypeBase;

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
  operator bool() const { return Ptr != 0; }
  
  void dump() const;
  void print(raw_ostream &OS) const;

  /// getString - Return the name of the type as a string, for use in
  /// diagnostics only.
  std::string getString() const;
  
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
public:
  explicit CanType(TypeBase *P = 0) : Type(P) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }
  explicit CanType(Type T) : Type(T) {
    assert(isActuallyCanonicalOrNull() &&
           "Forming a CanType out of a non-canonical type!");
  }
  
  // Direct comparison is allowed for CanTypes - they are known canonical.
  bool operator==(CanType T) const { return getPointer() == T.getPointer(); }
  bool operator!=(CanType T) const { return !operator==(T); }

};

template <class X> inline bool
isa(const Type&) = delete; // Use TypeBase::is instead.
template <class X> inline typename llvm::cast_retty<X, Type>::ret_type
cast(const Type&) = delete; // Use TypeBase::castTo instead.
template <class X> inline typename llvm::cast_retty<X, Type>::ret_type
dyn_cast(const Type&) = delete; // Use TypeBase::getAs instead.

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
  template<> struct simplify_type<const ::swift::CanType>
    : public simplify_type<const ::swift::Type> {};
  template<> struct simplify_type< ::swift::CanType>
    : public simplify_type<const ::swift::CanType> {};
    
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

  
  // An Type is "pointer like".
  template<typename T> class PointerLikeTypeTraits;
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
    static inline swift::CanType getFromVoidPointer(void *P) {
      return swift::CanType((swift::TypeBase*)P);
    }
  };
} // end namespace llvm

#endif
