//===--- Term.h - A term in the generics rewrite system ---------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2021 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "Symbol.h"

#ifndef SWIFT_RQM_TERM_H
#define SWIFT_RQM_TERM_H

namespace llvm {
  class raw_ostream;
}

namespace swift {

namespace rewriting {

/// See the implementation of MutableTerm::checkForOverlap() for a discussion.
enum class OverlapKind {
  /// Terms do not overlap.
  None,
  /// First kind of overlap (TUV vs U).
  First,
  /// Second kind of overlap (TU vs UV).
  Second
};

/// A term is a sequence of one or more symbols.
///
/// The Term type is a uniqued, permanently-allocated representation,
/// used to represent terms in the rewrite rules themselves. See also
/// MutableTerm for the other representation.
///
/// The first symbol in the term must be a protocol, generic parameter, or
/// associated type symbol.
///
/// A layout, superclass or concrete type symbol must only appear at the
/// end of a term.
///
/// Out-of-line methods are documented in Term.cpp.
class Term final {
  friend class RewriteContext;

  struct Storage;

  const Storage *Ptr;

  Term(const Storage *ptr) : Ptr(ptr) {}

public:
  size_t size() const;

  ArrayRef<Symbol>::iterator begin() const;
  ArrayRef<Symbol>::iterator end() const;

  ArrayRef<Symbol>::reverse_iterator rbegin() const;
  ArrayRef<Symbol>::reverse_iterator rend() const;

  Symbol back() const;

  Symbol operator[](size_t index) const;

  /// Returns an opaque pointer that uniquely identifies this term.
  const void *getOpaquePointer() const {
    return Ptr;
  }

  static Term get(const MutableTerm &term, RewriteContext &ctx);

  OverlapKind checkForOverlap(Term other,
                              MutableTerm &t,
                              MutableTerm &v) const;

  ArrayRef<const ProtocolDecl *> getRootProtocols() const {
    return begin()->getRootProtocols();
  }

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(Term lhs, Term rhs) {
    return lhs.Ptr == rhs.Ptr;
  }

  friend bool operator!=(Term lhs, Term rhs) {
    return !(lhs == rhs);
  }

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out, Term term) {
    term.dump(out);
    return out;
  }
};

/// A term is a sequence of one or more symbols.
///
/// The MutableTerm type is a dynamically-allocated representation,
/// used to represent temporary values in simplification and completion.
/// See also Term for the other representation.
///
/// The first symbol in the term must be a protocol, generic parameter, or
/// associated type symbol.
///
/// A layout constraint symbol must only appear at the end of a term.
///
/// Out-of-line methods are documented in RewriteSystem.cpp.
class MutableTerm final {
  llvm::SmallVector<Symbol, 3> Symbols;

public:
  /// Creates an empty term. At least one symbol must be added for the term
  /// to become valid.
  MutableTerm() {}

  explicit MutableTerm(decltype(Symbols)::const_iterator begin,
                       decltype(Symbols)::const_iterator end)
    : Symbols(begin, end) {}

  explicit MutableTerm(llvm::SmallVector<Symbol, 3> &&symbols)
    : Symbols(std::move(symbols)) {}

  explicit MutableTerm(ArrayRef<Symbol> symbols)
    : Symbols(symbols.begin(), symbols.end()) {}

  explicit MutableTerm(Term term)
    : Symbols(term.begin(), term.end()) {}

  void add(Symbol symbol) {
    Symbols.push_back(symbol);
  }

  void append(Term other) {
    Symbols.append(other.begin(), other.end());
  }

  void append(const MutableTerm &other) {
    Symbols.append(other.begin(), other.end());
  }

  int compare(const MutableTerm &other, const ProtocolGraph &protos) const;

  bool empty() const { return Symbols.empty(); }

  size_t size() const { return Symbols.size(); }

  ArrayRef<const ProtocolDecl *> getRootProtocols() const {
    return begin()->getRootProtocols();
  }

  decltype(Symbols)::const_iterator begin() const { return Symbols.begin(); }
  decltype(Symbols)::const_iterator end() const { return Symbols.end(); }

  decltype(Symbols)::iterator begin() { return Symbols.begin(); }
  decltype(Symbols)::iterator end() { return Symbols.end(); }

  decltype(Symbols)::const_reverse_iterator rbegin() const { return Symbols.rbegin(); }
  decltype(Symbols)::const_reverse_iterator rend() const { return Symbols.rend(); }

  decltype(Symbols)::reverse_iterator rbegin() { return Symbols.rbegin(); }
  decltype(Symbols)::reverse_iterator rend() { return Symbols.rend(); }

  Symbol back() const {
    return Symbols.back();
  }

  Symbol &back() {
    return Symbols.back();
  }

  Symbol operator[](size_t index) const {
    return Symbols[index];
  }

  Symbol &operator[](size_t index) {
    return Symbols[index];
  }

  void rewriteSubTerm(decltype(Symbols)::iterator from,
                      decltype(Symbols)::iterator to,
                      Term rhs);

  void dump(llvm::raw_ostream &out) const;

  friend bool operator==(const MutableTerm &lhs, const MutableTerm &rhs) {
    if (lhs.size() != rhs.size())
      return false;

    return std::equal(lhs.begin(), lhs.end(), rhs.begin());
  }

  friend bool operator!=(const MutableTerm &lhs, const MutableTerm &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::raw_ostream &operator<<(llvm::raw_ostream &out,
                                       const MutableTerm &term) {
    term.dump(out);
    return out;
  }
};

} // end namespace rewriting

} // end namespace swift

#endif