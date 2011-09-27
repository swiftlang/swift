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
#include "llvm/ADT/PointerIntPair.h"
#include "swift/AST/LLVM.h"
#include <string>

namespace swift {
  class TypeBase;

/// ValueKind - The classification of different kinds of expression
/// value.  Currently, we only have l-values and r-values.  Eventually
/// we may want "computed" l-values or something like it for dealing
/// with things like generalized property access.
enum class ValueKind : uint8_t {
  RValue, LValue
};
  
/// Type - This is a simple value object that contains a pointer to a type
/// class.  This is potentially sugared.
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

/// TypeJudgement - The "type judgement" is the result of fully
/// type-checking an expression.  In an imperative language, it
/// includes both the formal type of an expression and whether it
/// yields an assignable result.
class TypeJudgement {
  llvm::PointerIntPair<TypeBase*, 1> Data;

public:
  TypeJudgement() {}
  TypeJudgement(Type Ty, ValueKind VK)
    : Data(Ty.getPointer(), unsigned(VK)) {}

  Type getType() const { return Data.getPointer(); }
  ValueKind getValueKind() const { return ValueKind(Data.getInt()); }
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
} // end namespace llvm

#endif
