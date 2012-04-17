//===--- Identifier.h - Uniqued Identifier ----------------------*- C++ -*-===//
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
// This file defines the Identifier interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_IDENTIFIER_H
#define SWIFT_AST_IDENTIFIER_H

#include "swift/AST/LLVM.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/StringRef.h"
#include <cstring>

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ASTContext;
  
/// Identifier - This is an instance of a uniqued identifier created by
/// ASTContext.  It just wraps a nul-terminated "const char*".
class Identifier {
  friend class ASTContext;
  const char *Pointer;
  
  /// Constructor, only accessible by ASTContext, which handles the uniquing.
  explicit Identifier(const char *Ptr) : Pointer(Ptr) {}
public:
  explicit Identifier() : Pointer(0) {}
  
  const char *get() const { return Pointer; }
  
  StringRef str() const { return Pointer; }
  
  unsigned getLength() const {
    return ::strlen(Pointer);
  }
  
  bool empty() const { return Pointer == 0; }
  
  /// isOperator - Return true if this identifier is an operator, false if it is
  /// a normal identifier.
  bool isOperator() const {
    return !empty() && isOperatorChar(Pointer[0]);
  }
  
  /// isOperatorChar - Return true if the specified character is a
  /// valid part of an operator.
  static bool isOperatorChar(char C) {
    return strchr("/=-+*%<>!&|^~.", C) != 0;
  }
  
  void *getAsOpaquePointer() const { return (void *)Pointer; }
  
  static Identifier getFromOpaquePointer(void *P) {
    return Identifier((const char*)P);
  }
  
  bool operator==(Identifier RHS) const { return Pointer == RHS.Pointer; }
  bool operator!=(Identifier RHS) const { return Pointer != RHS.Pointer; }
  
  static Identifier getEmptyKey() {
    return Identifier((const char*)
                      llvm::DenseMapInfo<const void*>::getEmptyKey());
  }
  static Identifier getTombstoneKey() {
    return Identifier((const char*)
                      llvm::DenseMapInfo<const void*>::getTombstoneKey());
  }
};
  
} // end namespace swift

namespace llvm {
  raw_ostream &operator<<(raw_ostream &OS, swift::Identifier I);
  
  // Identifiers hash just like pointers.
  template<> struct DenseMapInfo<swift::Identifier> {
    static swift::Identifier getEmptyKey() {
      return swift::Identifier::getEmptyKey();
    }
    static swift::Identifier getTombstoneKey() {
      return swift::Identifier::getTombstoneKey();
    }
    static unsigned getHashValue(swift::Identifier Val) {
      return DenseMapInfo<const void*>::getHashValue(Val.get());
    }
    static bool isEqual(swift::Identifier LHS, swift::Identifier RHS) {
      return LHS == RHS;
    }
  };
  
  // An Identifier is "pointer like".
  template<typename T> class PointerLikeTypeTraits;
  template<>
  class PointerLikeTypeTraits<swift::Identifier> {
  public:
    static inline void *getAsVoidPointer(swift::Identifier I) {
      return (void*)I.get();
    }
    static inline swift::Identifier getFromVoidPointer(void *P) {
      return swift::Identifier::getFromOpaquePointer(P);
    }
    enum { NumLowBitsAvailable = 3 };
  };
  
} // end namespace llvm

#endif
