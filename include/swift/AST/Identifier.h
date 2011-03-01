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
  
  llvm::StringRef str() const { return Pointer; }
  
  unsigned getLength() const {
    return ::strlen(Pointer);
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
  
}

#endif
