//===--- Term.cpp - A term in the generics rewrite system -----------------===//
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

#include "swift/AST/Decl.h"
#include "swift/AST/Types.h"
#include "llvm/ADT/FoldingSet.h"
#include "llvm/Support/raw_ostream.h"
#include <algorithm>
#include <vector>
#include "ProtocolGraph.h"
#include "RewriteContext.h"
#include "Symbol.h"
#include "Term.h"

using namespace swift;
using namespace rewriting;

/// Terms are uniqued and immutable, stored as a single pointer;
/// the Storage type is the allocated backing storage.
struct Term::Storage final
  : public llvm::FoldingSetNode,
    public llvm::TrailingObjects<Storage, Symbol> {
  friend class Symbol;

  unsigned Size;

  explicit Storage(unsigned size) : Size(size) {}

  size_t numTrailingObjects(OverloadToken<Symbol>) const {
    return Size;
  }

  MutableArrayRef<Symbol> getElements() {
    return {getTrailingObjects<Symbol>(), Size};
  }

  ArrayRef<Symbol> getElements() const {
    return {getTrailingObjects<Symbol>(), Size};
  }

  void Profile(llvm::FoldingSetNodeID &id) const;
};

size_t Term::size() const { return Ptr->Size; }

ArrayRef<Symbol>::iterator Term::begin() const {
  return Ptr->getElements().begin();
}

ArrayRef<Symbol>::iterator Term::end() const {
  return Ptr->getElements().end();
}

ArrayRef<Symbol>::reverse_iterator Term::rbegin() const {
  return Ptr->getElements().rbegin();
}

ArrayRef<Symbol>::reverse_iterator Term::rend() const {
  return Ptr->getElements().rend();
}

Symbol Term::back() const {
  return Ptr->getElements().back();
}

Symbol Term::operator[](size_t index) const {
  return Ptr->getElements()[index];
}

void Term::dump(llvm::raw_ostream &out) const {
  MutableTerm(*this).dump(out);
}

Term Term::get(const MutableTerm &mutableTerm, RewriteContext &ctx) {
  unsigned size = mutableTerm.size();
  assert(size > 0 && "Term must have at least one symbol");

  llvm::FoldingSetNodeID id;
  id.AddInteger(size);
  for (auto symbol : mutableTerm)
    id.AddPointer(symbol.getOpaquePointer());

  void *insertPos = nullptr;
  if (auto *term = ctx.Terms.FindNodeOrInsertPos(id, insertPos))
    return term;

  void *mem = ctx.Allocator.Allocate(
      Storage::totalSizeToAlloc<Symbol>(size),
      alignof(Storage));
  auto *term = new (mem) Storage(size);
  for (unsigned i = 0; i < size; ++i)
    term->getElements()[i] = mutableTerm[i];

  ctx.Terms.InsertNode(term, insertPos);
  ctx.TermHistogram.add(size);

  return term;
}

void Term::Storage::Profile(llvm::FoldingSetNodeID &id) const {
  id.AddInteger(Size);

  for (auto symbol : getElements())
    id.AddPointer(symbol.getOpaquePointer());
}

/// Shortlex order on terms.
///
/// First we compare length, then perform a lexicographic comparison
/// on symbols if the two terms have the same length.
int MutableTerm::compare(const MutableTerm &other,
                         const ProtocolGraph &graph) const {
  if (size() != other.size())
    return size() < other.size() ? -1 : 1;

  for (unsigned i = 0, e = size(); i < e; ++i) {
    auto lhs = (*this)[i];
    auto rhs = other[i];

    int result = lhs.compare(rhs, graph);
    if (result != 0) {
      assert(lhs != rhs);
      return result;
    }

    assert(lhs == rhs);
  }

  return 0;
}

/// Replace the subterm in the range [from,to) with \p rhs.
///
/// Note that \p rhs must precede [from,to) in the linear
/// order on terms.
void MutableTerm::rewriteSubTerm(
    decltype(MutableTerm::Symbols)::iterator from,
    decltype(MutableTerm::Symbols)::iterator to,
    Term rhs) {
  auto oldSize = size();
  unsigned lhsLength = (unsigned)(to - from);
  assert(rhs.size() <= lhsLength);

  // Overwrite the occurrence of the left hand side with the
  // right hand side.
  auto newIter = std::copy(rhs.begin(), rhs.end(), from);

  // If the right hand side is shorter than the left hand side,
  // then newIter will point to a location before oldIter, eg
  // if this term is 'T.A.B.C', lhs is 'A.B' and rhs is 'X',
  // then we now have:
  //
  // T.X  .C
  //       ^--- oldIter
  //     ^--- newIter
  //
  // Shift everything over to close the gap (by one location,
  // in this case).
  if (newIter != to) {
    auto newEnd = std::copy(to, end(), newIter);

    // Now, we've moved the gap to the end of the term; close
    // it by shortening the term.
    Symbols.erase(newEnd, end());
  }

  assert(size() == oldSize - lhsLength + rhs.size());
}

void MutableTerm::dump(llvm::raw_ostream &out) const {
  bool first = true;

  for (auto symbol : Symbols) {
    if (!first)
      out << ".";
    else
      first = false;

    symbol.dump(out);
  }
}