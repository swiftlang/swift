//===--- YieldList.h - Functions & closures yield type lists -*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the YieldList class and support logic.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_YIELDLIST_H
#define SWIFT_AST_YIELDLIST_H

#include "swift/AST/TypeRepr.h"
#include "swift/AST/Types.h"
#include "swift/Basic/Debug.h"
#include "llvm/Support/TrailingObjects.h"

namespace swift {
class FuncDecl;

class Yield final {
  TypeRepr *typeRepr = nullptr;
  llvm::PointerIntPair<Type, 3, uint8_t> typeAndFlags;

public:
  explicit Yield(TypeRepr *typeRepr) : typeRepr(typeRepr), typeAndFlags() {}
  Yield(TypeRepr *typeRepr, YieldTypeFlags flags)
      : typeRepr(typeRepr), typeAndFlags(nullptr, flags.toRaw()) {}
  explicit Yield(Type type) : typeAndFlags(type) {}
  Yield(Type type, YieldTypeFlags flags) : typeAndFlags(type, flags.toRaw()) {}
  explicit Yield(AnyFunctionType::Yield yield)
      : Yield(yield.getType(), yield.getFlags()) {}

  TypeRepr *getTypeRepr() const { return typeRepr; }
  Type getInterfaceType(const FuncDecl *parent) const;

  YieldTypeFlags getFlags() const {
    return YieldTypeFlags::fromRaw(typeAndFlags.getInt());
  }
  void setType(Type type, YieldTypeFlags flags) {
    typeAndFlags.setPointerAndInt(type, flags.toRaw());
  }

  AnyFunctionType::Yield toFunctionYield(const FuncDecl *parent) const {
    return AnyFunctionType::Yield(getInterfaceType(parent), getFlags());
  }

  friend class YieldsTypeRequest;
  friend class YieldList;
};

/// This describes a list of yields. Each yield descriptor is tail allocated
/// onto this list.
class YieldList final : public ASTAllocated<YieldList>,
                        private llvm::TrailingObjects<YieldList, Yield> {
  friend TrailingObjects;

  SourceLoc LParenLoc, RParenLoc;
  size_t numYields;

  YieldList(SourceLoc LParenLoc, size_t numYields, SourceLoc RParenLoc)
      : LParenLoc(LParenLoc), RParenLoc(RParenLoc), numYields(numYields) {}
  void operator=(const YieldList &) = delete;

public:
  /// Create a yield list with the specified yield types.
  static YieldList *create(const ASTContext &C, SourceLoc LParenLoc,
                           ArrayRef<TypeRepr *> yields, SourceLoc RParenLoc);

  /// Create a yield list with the specified yield types, with no location
  /// info for the parens.
  static YieldList *create(const ASTContext &C, ArrayRef<TypeRepr *> yields) {
    return create(C, SourceLoc(), yields, SourceLoc());
  }

  static YieldList *create(const ASTContext &C,
                           ArrayRef<AnyFunctionType::Yield> yields);

  static YieldList *create(const ASTContext &C, ArrayRef<Yield> yields);

  SourceLoc getLParenLoc() const { return LParenLoc; }
  SourceLoc getRParenLoc() const { return RParenLoc; }

  typedef MutableArrayRef<Yield>::iterator iterator;
  typedef ArrayRef<Yield>::iterator const_iterator;
  iterator begin() { return getArray().begin(); }
  iterator end() { return getArray().end(); }
  const_iterator begin() const { return getArray().begin(); }
  const_iterator end() const { return getArray().end(); }

  const Yield &front() const { return getArray().front(); }
  const Yield &back() const { return getArray().back(); }

  MutableArrayRef<Yield> getArray() { return getTrailingObjects(numYields); }
  ArrayRef<Yield> getArray() const { return getTrailingObjects(numYields); }

  size_t size() const { return numYields; }

  const Yield &get(unsigned i) const { return getArray()[i]; }

  Yield &get(unsigned i) { return getArray()[i]; }

  const Yield &operator[](unsigned i) const { return get(i); }
  Yield &operator[](unsigned i) { return get(i); }

  /// Return the full source range of this parameter.
  SourceRange getSourceRange() const;
  SourceLoc getStartLoc() const { return getSourceRange().Start; }
  SourceLoc getEndLoc() const { return getSourceRange().End; }

  SWIFT_DEBUG_DUMP;
  void dump(raw_ostream &OS, unsigned Indent = 0) const;

  //  void print(raw_ostream &OS) const;
};

} // end namespace swift

#endif
