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

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringRef.h"
#include <cstring>

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ASTContext;

/// DeclRefKind - The kind of reference to an identifier.
enum class DeclRefKind {
  /// An ordinary reference to an identifier, e.g. 'foo'.
  Ordinary,

  /// A reference to an identifier as a binary operator, e.g. '+' in 'a+b'.
  BinaryOperator,

  /// A reference to an identifier as a postfix unary operator, e.g. '++' in
  /// 'a++'.
  PostfixOperator,

  /// A reference to an identifier as a prefix unary operator, e.g. '--' in
  /// '--a'.
  PrefixOperator
};

/// Identifier - This is an instance of a uniqued identifier created by
/// ASTContext.  It just wraps a nul-terminated "const char*".
class Identifier {
  friend class ASTContext;
  const char *Pointer;
  
  /// Constructor, only accessible by ASTContext, which handles the uniquing.
  explicit Identifier(const char *Ptr) : Pointer(Ptr) {}
public:
  explicit Identifier() : Pointer(nullptr) {}
  
  const char *get() const { return Pointer; }
  
  StringRef str() const { return Pointer; }
  
  unsigned getLength() const {
    return ::strlen(Pointer);
  }
  
  bool empty() const { return Pointer == 0; }
  
  /// isOperator - Return true if this identifier is an operator, false if it is
  /// a normal identifier.
  /// FIXME: We should maybe cache this.
  bool isOperator() const {
    if (empty())
      return false;
    if ((unsigned char)Pointer[0] < 0x80)
      return isOperatorStartCodePoint((unsigned char)Pointer[0]);

    // Handle the high unicode case out of line.
    return isOperatorSlow();
  }
  
  /// isOperatorStartCodePoint - Return true if the specified code point is a
  /// valid start of an operator.
  static bool isOperatorStartCodePoint(uint32_t C) {
    // ASCII operator chars.
    static const char OpChars[] = "/=-+*%<>!&|^~.";
    if (C < 0x80)
      return memchr(OpChars, C, sizeof(OpChars) - 1) != 0;
    
    // Unicode math, symbol, arrow, dingbat, and line/box drawing chars.
    return (C >= 0x00A1 && C <= 0x00A7)
        || C == 0x00A9 || C == 0x00AB || C == 0x00AC || C == 0x00AE
        || C == 0x00B0 || C == 0x00B1 || C == 0x00B6 || C == 0x00BB
        || C == 0x00BF || C == 0x00D7 || C == 0x00F7
        || C == 0x2016 || C == 0x2017 || (C >= 0x2020 && C <= 0x2027)
        || (C >= 0x2030 && C <= 0x203E) || (C >= 0x2041 && C <= 0x2053)
        || (C >= 0x2055 && C <= 0x205E) || (C >= 0x2190 && C <= 0x23FF)
        || (C >= 0x2500 && C <= 0x2775) || (C >= 0x2794 && C <= 0x2BFF)
        || (C >= 0x2E00 && C <= 0x2E7F) || (C >= 0x3001 && C <= 0x3003)
        || (C >= 0x3008 && C <= 0x3030);
  }
  
  /// isOperatorContinuationCodePoint - Return true if the specified code point
  /// is a valid operator code point.
  static bool isOperatorContinuationCodePoint(uint32_t C) {
    // '.' is a special case. It can only appear in '..'.
    if (C == '.')
      return false;
    if (isOperatorStartCodePoint(C))
      return true;

    // Unicode combining characters.
    return (C >= 0x0300 && C <= 0x036F)
        || (C >= 0x1DC0 && C <= 0x1DFF)
        || (C >= 0x20D0 && C <= 0x20FF)
        || (C >= 0xFE20 && C <= 0xFE2F);
  }
  
  void *getAsOpaquePointer() const { return (void *)Pointer; }
  
  static Identifier getFromOpaquePointer(void *P) {
    return Identifier((const char*)P);
  }
  
  bool operator==(Identifier RHS) const { return Pointer == RHS.Pointer; }
  bool operator!=(Identifier RHS) const { return Pointer != RHS.Pointer; }

  bool operator<(Identifier RHS) const { return Pointer < RHS.Pointer; }
  
  static Identifier getEmptyKey() {
    return Identifier((const char*)
                      llvm::DenseMapInfo<const void*>::getEmptyKey());
  }
  static Identifier getTombstoneKey() {
    return Identifier((const char*)
                      llvm::DenseMapInfo<const void*>::getTombstoneKey());
  }

private:
  bool isOperatorSlow() const;
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
    enum { NumLowBitsAvailable = 2 };
  };
  
} // end namespace llvm

namespace swift {
  
/// A declaration name, which may comprise one or more identifier pieces.
class DeclName {
  friend class ASTContext;
  
  struct alignas(Identifier*) CompoundDeclName : llvm::FoldingSetNode {
    size_t Size;
    
    explicit CompoundDeclName(size_t Size) : Size(Size) {}
    
    ArrayRef<Identifier> getComponents() const {
      return {reinterpret_cast<const Identifier*>(this + 1), Size};
    }
    MutableArrayRef<Identifier> getComponents() {
      return {reinterpret_cast<Identifier*>(this + 1), Size};
    }
      
    /// Uniquing for the ASTContext.
    static void Profile(llvm::FoldingSetNodeID &id,
                        ArrayRef<Identifier> components);
    
    void Profile(llvm::FoldingSetNodeID &id) {
      Profile(id, getComponents());
    }
  };
  
  // Either a single identifier piece stored inline, or a reference to a
  // run-length-encoded array of identifier pieces owned by the ASTContext.
  llvm::PointerUnion<Identifier, CompoundDeclName*> SimpleOrCompound;
  
public:
  /// Build a null name.
  DeclName() : SimpleOrCompound(Identifier()) {}
  
  /// Build a simple value name with one component.
  /*implicit*/ DeclName(Identifier simpleName)
    : SimpleOrCompound(simpleName) {}
  
  /// Build a compound value name by copying a list of components.
  DeclName(ASTContext &C, ArrayRef<Identifier> components);
  
  /// Get the first name component. This is the name that is looked up in
  /// "C-style" property accesses, such as 'foo.bar' or 'foo.bar(1, 2)'.
  ///
  /// TODO: Eventually compound names should not return a name here.
  Identifier getSimpleName() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return compound->getComponents()[0];
    
    return SimpleOrCompound.get<Identifier>();
  }
  
  /// Get the components array.
  ArrayRef<Identifier> getComponents() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return compound->getComponents();
    
    auto identifier = SimpleOrCompound.get<Identifier>();
    if (identifier.get())
      return identifier;
    return {};
  }
  
  explicit operator bool() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return true;
    return SimpleOrCompound.get<Identifier>().get();
  }
  
  /// True if this is a simple one-component name.
  bool isSimpleName() const {
    return !SimpleOrCompound.dyn_cast<CompoundDeclName*>();
  }
  
  /// True if this name is an operator.
  bool isOperator() const {
    return isSimpleName() && getSimpleName().isOperator();
  }
};
  
} // end namespace swift

#endif
