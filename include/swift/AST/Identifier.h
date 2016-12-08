//===--- Identifier.h - Uniqued Identifier ----------------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2016 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the Identifier interface.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_IDENTIFIER_H
#define SWIFT_AST_IDENTIFIER_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/ADT/PointerUnion.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/Support/TrailingObjects.h"

namespace llvm {
  class raw_ostream;
}

namespace swift {
  class ASTContext;
  class ParameterList;

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
    assert(Pointer != nullptr && "Tried getting length of empty identifier");
    return ::strlen(Pointer);
  }
  
  bool empty() const { return Pointer == nullptr; }
  
  /// isOperator - Return true if this identifier is an operator, false if it is
  /// a normal identifier.
  /// FIXME: We should maybe cache this.
  bool isOperator() const {
    if (empty())
      return false;
    if (isEditorPlaceholder())
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
    static const char OpChars[] = "/=-+*%<>!&|^~.?";
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
    if (isOperatorStartCodePoint(C))
      return true;

    // Unicode combining characters and variation selectors.
    return (C >= 0x0300 && C <= 0x036F)
        || (C >= 0x1DC0 && C <= 0x1DFF)
        || (C >= 0x20D0 && C <= 0x20FF)
        || (C >= 0xFE00 && C <= 0xFE0F)
        || (C >= 0xFE20 && C <= 0xFE2F)
        || (C >= 0xE0100 && C <= 0xE01EF);
  }

  static bool isEditorPlaceholder(StringRef name) {
    return name.startswith("<#");
  }

  bool isEditorPlaceholder() const {
    return !empty() && isEditorPlaceholder(str());
  }
  
  const void *getAsOpaquePointer() const {
      return static_cast<const void *>(Pointer);
  }
  
  static Identifier getFromOpaquePointer(void *P) {
    return Identifier((const char*)P);
  }

  /// Compare two identifiers, producing -1 if \c *this comes before \c other,
  /// 1 if \c *this comes after \c other, and 0 if they are equal.
  ///
  /// Null identifiers come after all other identifiers.
  int compare(Identifier other) const;

  bool operator==(Identifier RHS) const { return Pointer == RHS.Pointer; }
  bool operator!=(Identifier RHS) const { return !(*this==RHS); }

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
  
class DeclName;
class ObjCSelector;

} // end namespace swift

namespace llvm {
  raw_ostream &operator<<(raw_ostream &OS, swift::Identifier I);
  raw_ostream &operator<<(raw_ostream &OS, swift::DeclName I);
  raw_ostream &operator<<(raw_ostream &OS, swift::ObjCSelector S);

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
  
enum class DeclNameKind {
  /// The name consists of an identifier and possibly arguments or is empty iff
  /// the identifier is empty
  Normal,
  /// The name represents a subscript and doesn't have an identifier
  Subscript,
};
  
/// A declaration name, which may comprise one or more identifier pieces.
class DeclName {
  friend class ASTContext;

  /// Represents a compound declaration name.
  struct alignas(Identifier) CompoundDeclName final : llvm::FoldingSetNode,
      private llvm::TrailingObjects<CompoundDeclName, Identifier> {
    friend TrailingObjects;
    friend class DeclName;

    Identifier BaseName;
    size_t NumArgs;
    
    explicit CompoundDeclName(Identifier BaseName, size_t NumArgs)
      : BaseName(BaseName), NumArgs(NumArgs)
    {
      assert(NumArgs > 0 && "Should use IdentifierAndCompound");
    }
    
    ArrayRef<Identifier> getArgumentNames() const {
      return {getTrailingObjects<Identifier>(), NumArgs};
    }
    MutableArrayRef<Identifier> getArgumentNames() {
      return {getTrailingObjects<Identifier>(), NumArgs};
    }
      
    /// Uniquing for the ASTContext.
    static void Profile(llvm::FoldingSetNodeID &id,
                        Identifier baseName,
                        ArrayRef<Identifier> argumentNames);
    
    void Profile(llvm::FoldingSetNodeID &id) {
      Profile(id, BaseName, getArgumentNames());
    }
  };

  // A single stored identifier, along with a bit stating whether it is the
  // base name for a zero-argument compound name.
  typedef llvm::PointerIntPair<Identifier, 1, bool> IdentifierAndCompound;

  /// The name of a subscript DeclName to be returned by the \c str() method
  static const std::string subscriptName;
  
  // Either a single identifier piece stored inline (with a bit to say whether
  // it is simple or compound), or a reference to a compound declaration name.
  llvm::PointerUnion<IdentifierAndCompound, CompoundDeclName*> SimpleOrCompound;
  
  /// Is this DeclName a special one (subscript etc.) or represented by an
  /// identifier?
  DeclNameKind Kind;
  
  DeclName(void *Opaque)
    : SimpleOrCompound(decltype(SimpleOrCompound)::getFromOpaqueValue(Opaque))
  {}

  void initialize(ASTContext &C, Identifier baseName,
                  ArrayRef<Identifier> argumentNames);
  
public:
  /// Build a null name.
  DeclName() : SimpleOrCompound(IdentifierAndCompound()),
               Kind(DeclNameKind::Normal) {}
  
  /// Build a simple value name with one component.
  /*implicit*/ DeclName(Identifier simpleName)
    : SimpleOrCompound(IdentifierAndCompound(simpleName, false)),
      Kind(DeclNameKind::Normal) {}
  
  /// Build a compound value name given a base name and a set of argument names.
  DeclName(ASTContext &C, Identifier baseName,
           ArrayRef<Identifier> argumentNames) {
    initialize(C, baseName, argumentNames);
  }

  /// Build a compound value name given a base name and a set of argument names
  /// extracted from a parameter list.
  DeclName(ASTContext &C, Identifier baseName, ParameterList *paramList);
  
  /// Retrieve the 'base' name, i.e., the name that follows the introducer,
  /// such as the 'foo' in 'func foo(x:Int, y:Int)' or the 'bar' in
  /// 'var bar: Int'.
  Identifier getIdentifier() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return compound->BaseName;
    
    return SimpleOrCompound.get<IdentifierAndCompound>().getPointer();
  }

  /// Retrieve the 'base' name, i.e., the name that follows the introducer,
  /// such as the 'foo' in 'func foo(x:Int, y:Int)' or the 'bar' in
  /// 'var bar: Int'.
  DeclName getBaseName() const {
    DeclName base;
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName*>()) {
      base = compound->BaseName;
    } else {
      base = SimpleOrCompound.get<IdentifierAndCompound>().getPointer();
    }
    base.Kind = Kind;
    return base;
  }
  
  /// Create a new subscript \c DeclName with no arguments
  static DeclName createSubscript() {
    auto name = DeclName();
    name.Kind = DeclNameKind::Subscript;
    return name;
  }
  
  /// Create a compound subscript \c DeclName and a set of argument names.
  static DeclName createSubscript(ASTContext &C,
                                  ArrayRef<Identifier> argumentNames) {
    auto name = DeclName(C, Identifier(), argumentNames);
    name.Kind = DeclNameKind::Subscript;
    return name;
  }
  
  /// Create a compound value name for a subscript and a set of argument names
  /// extracted from a parameter list.
  static DeclName createSubscript(ASTContext &C,
                                  ParameterList *paramList);
  
  /// Retrieve the names of the arguments, if there are any.
  ArrayRef<Identifier> getArgumentNames() const {
    if (auto compound = SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return compound->getArgumentNames();

    return { };
  }

  explicit operator bool() const {
    if (isSpecialName())
      return true;
    if (SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return true;
    return !SimpleOrCompound.get<IdentifierAndCompound>().getPointer().empty();
  }
  
  /// True if this is a simple one-component name.
  bool isSimpleName() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return false;

    return !SimpleOrCompound.get<IdentifierAndCompound>().getInt();
  }

  /// True if this is a compound name.
  bool isCompoundName() const {
    if (SimpleOrCompound.dyn_cast<CompoundDeclName*>())
      return true;

    return SimpleOrCompound.get<IdentifierAndCompound>().getInt();
  }
  
  /// True if this name is a simple one-component name identical to the
  /// given identifier.
  bool isSimpleName(Identifier name) const {
    return isSimpleName() && !isSpecialName() && getIdentifier() == name;
  }
  
  /// True if this name is a simple one-component name equal to the
  /// given string.
  bool isSimpleName(StringRef name) const {
    return isSimpleName() && !isSpecialName() &&
             getIdentifier().str().equals(name);
  }
  
  bool isEditorPlaceholder() const {
    return isSimpleName() && getIdentifier().isEditorPlaceholder();
  }
  
  /// True if this name is an operator.
  bool isOperator() const {
    if (isSpecialName())
      return false;
    return getIdentifier().isOperator();
  }
  
  DeclNameKind getKind() const { return Kind; }
  
  /// True if this DeclName does not have a dedicated identifier
  bool isSpecialName() const { return Kind != DeclNameKind::Normal; }
  
  /// True if this name should be found by a decl ref or member ref under the
  /// name specified by 'refName'.
  ///
  /// We currently match compound names either when their first component
  /// matches a simple name lookup or when the full compound name matches.
  bool matchesRef(DeclName refName) const {
    // Identical names always match.
    if (SimpleOrCompound == refName.SimpleOrCompound && Kind == refName.Kind)
      return true;
    // If the reference is a simple name, try simple name matching.
    if (!isSpecialName()) {
      if (refName.isSimpleName())
        return refName.getIdentifier() == getIdentifier();
      // The names don't match.
      return false;
    } else {
      return refName.Kind == Kind;
    }
  }
  
  /// Add a DeclName to a lookup table so that it can be found by its simple
  /// name or its compound name.
  template<typename LookupTable, typename Element>
  void addToLookupTable(LookupTable &table, const Element &elt) {
    table[*this].push_back(elt);
    if (!isSimpleName()) {
      table[getBaseName()].push_back(elt);
    }
  }

  /// Compare two declaration names, producing -1 if \c *this comes before
  /// \c other,  1 if \c *this comes after \c other, and 0 if they are equal.
  ///
  /// Null declaration names come after all other declaration names.
  int compare(DeclName other) const;

  friend bool operator==(DeclName lhs, DeclName rhs) {
    if (lhs.Kind != rhs.Kind)
      return false;
    if (lhs.isSimpleName() != rhs.isSimpleName())
      return false;
    if (lhs.Kind == DeclNameKind::Normal)
      if (lhs.getIdentifier() != rhs.getIdentifier())
        return false;
    
    auto lhsArgNames = lhs.getArgumentNames();
    auto rhsArgNames = rhs.getArgumentNames();
    if (lhsArgNames.size() != rhsArgNames.size())
      return false;
    for (unsigned i = 0; i < lhsArgNames.size(); i++) {
      if (lhsArgNames[i] != rhsArgNames[i])
        return false;
    }
    return true;
  }

  friend bool operator!=(DeclName lhs, DeclName rhs) {
    return !(lhs == rhs);
  }

  friend bool operator<(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) < 0;
  }

  friend bool operator<=(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) <= 0;
  }

  friend bool operator>(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) > 0;
  }

  friend bool operator>=(DeclName lhs, DeclName rhs) {
    return lhs.compare(rhs) >= 0;
  }

  bool operator ==(StringRef other) {
    return getBaseName().isSimpleName(other);
  }
  
  bool operator !=(StringRef other) {
    return !getBaseName().isSimpleName(other);
  }

  bool operator ==(Identifier other) {
    return getBaseName().isSimpleName(other);
  }
  
  bool operator !=(Identifier other) {
    return !getBaseName().isSimpleName(other);
  }
  
  /// Get a string representation of the name,
  ///
  /// \param scratch Scratch space to use.
  StringRef getString(llvm::SmallVectorImpl<char> &scratch,
                      bool skipEmptyArgumentNames = false) const;
  
  /// Return the string representation of the base name (i.e. without
  /// arguements). Special \c DeclNames return their string representation.
  StringRef str() const {
    switch (Kind) {
      case DeclNameKind::Normal:
        return getIdentifier().str();
      case DeclNameKind::Subscript:
        return subscriptName;
    }
  }
  
  /// Return the string representation of the name used for serialization. This
  /// encodes special \c DeclNames with a leading '$'
  StringRef serializationString() const {
    switch (getKind()) {
      case DeclNameKind::Normal:
        return getIdentifier().str();
      case DeclNameKind::Subscript:
        return "$subscript";
    }
  }
  
  /// Print the representation of this declaration name to the given
  /// stream.
  ///
  /// \param skipEmptyArgumentNames When true, don't print the argument labels
  /// if they are all empty.
  llvm::raw_ostream &print(llvm::raw_ostream &os,
                           bool skipEmptyArgumentNames = false) const;

  /// Print a "pretty" representation of this declaration name to the given
  /// stream.
  ///
  /// This is the name used for diagnostics; it is not necessarily the
  /// fully-specified name that would be written in the source.
  llvm::raw_ostream &printPretty(llvm::raw_ostream &os) const;

  /// Dump this name to standard error.
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                            "only for use within the debugger");
};

/// Represents an Objective-C selector.
class ObjCSelector {
  /// The storage for an Objective-C selector.
  ///
  /// A zero-argument selector is represented as simple name.
  /// A selector with N arguments is represented as a compound name with
  /// N arguments, where the simple name is a placeholder.
  DeclName Storage;

  explicit ObjCSelector(DeclName storage) : Storage(storage) { }

  friend struct llvm::DenseMapInfo<ObjCSelector>;

public:
  /// Form a selector with the given number of arguments and the given selector
  /// pieces.
  ObjCSelector(ASTContext &ctx, unsigned numArgs, ArrayRef<Identifier> pieces);

  /// Determine the number of arguments in the selector.
  ///
  /// When this is zero, the number of selector pieces will be one. Otherwise,
  /// it equals the number of selector pieces.
  unsigned getNumArgs() const {
    if (Storage.isSimpleName()) {
      return 0;
    }

    return Storage.getArgumentNames().size();
  }

  /// Determine the number of selector pieces in the selector.
  ///
  /// When this is one, the number of arguments may either be zero or one.
  /// Otherwise, it equals the number of arguments.
  unsigned getNumSelectorPieces() const {
    return getSelectorPieces().size();
  }

  /// Retrieve the pieces in this selector.
  ArrayRef<Identifier> getSelectorPieces() const {
    if (Storage.isSimpleName()) {
      return { reinterpret_cast<const Identifier*>(&Storage), 1 };
    }

    return Storage.getArgumentNames();
  }

  /// Get a string representation of the selector.
  ///
  /// \param scratch Scratch space to use.
  StringRef getString(llvm::SmallVectorImpl<char> &scratch) const;

  /// Dump this selector to standard error.
  LLVM_ATTRIBUTE_DEPRECATED(void dump() const,
                            "only for use within the debugger");

  /// Compare two Objective-C selectors, producing -1 if \c *this comes before
  /// \c other,  1 if \c *this comes after \c other, and 0 if they are equal.
  int compare(ObjCSelector other) const {
    return Storage.compare(other.Storage);
  }

  friend bool operator==(ObjCSelector lhs, ObjCSelector rhs) {
    return lhs.Storage == rhs.Storage;
  }

  friend bool operator!=(ObjCSelector lhs, ObjCSelector rhs) {
    return !(lhs == rhs);
  }

  friend bool operator<(ObjCSelector lhs, ObjCSelector rhs) {
    return lhs.compare(rhs) < 0;
  }

  friend bool operator<=(ObjCSelector lhs, ObjCSelector rhs) {
    return lhs.compare(lhs) <= 0;
  }

  friend bool operator>(ObjCSelector lhs, ObjCSelector rhs) {
    return lhs.compare(lhs) > 0;
  }

  friend bool operator>=(ObjCSelector lhs, ObjCSelector rhs) {
    return lhs.compare(lhs) >= 0;
  }
};

} // end namespace swift

namespace llvm {
  template<> struct DenseMapInfo<swift::DeclName> {
    static swift::DeclName getEmptyKey() {
      return swift::Identifier::getEmptyKey();
    }
    static swift::DeclName getTombstoneKey() {
      return swift::Identifier::getTombstoneKey();
    }
    static unsigned getHashValue(swift::DeclName Val) {
      unsigned H = 0;
      H ^= static_cast<unsigned>(Val.getKind()) << 1;
      H ^= static_cast<unsigned>(Val.isSimpleName());
      if (!Val.isSpecialName())
        H ^= llvm::HashString(Val.getIdentifier().get());
      
      auto argNames = Val.getArgumentNames();
      for (unsigned i = 0; i < argNames.size(); i++) {
        H ^= llvm::HashString(argNames[i].get());
      }
      return H;
    }
    static bool isEqual(swift::DeclName LHS, swift::DeclName RHS) {
      return LHS == RHS;
    }
  };  

  template<> struct DenseMapInfo<swift::ObjCSelector> {
    static swift::ObjCSelector getEmptyKey() {
      return swift::ObjCSelector(DenseMapInfo<swift::DeclName>::getEmptyKey());
    }
    static swift::ObjCSelector getTombstoneKey() {
      return swift::ObjCSelector(
               DenseMapInfo<swift::DeclName>::getTombstoneKey());
    }
    static unsigned getHashValue(swift::ObjCSelector Val) {
      return DenseMapInfo<swift::DeclName>::getHashValue(Val.Storage);
    }
    static bool isEqual(swift::ObjCSelector LHS, swift::ObjCSelector RHS) {
      return LHS == RHS;
    }
  };
} // end namespace llvm

#endif
