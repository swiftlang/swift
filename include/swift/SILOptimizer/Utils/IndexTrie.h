//===--- IndexTrie - Trie for a sequence of integer indices ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2017 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_INDEXTREE_H
#define SWIFT_SILOPTIMIZER_UTILS_INDEXTREE_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include <algorithm>

namespace swift {

// Trie node representing a sequence of unsigned integer indices.
class IndexTrieNode {
  static const unsigned RootIdx = ~0U;
  unsigned Index;
  llvm::SmallVector<IndexTrieNode*, 8> Children;
  IndexTrieNode *Parent;

public:
  IndexTrieNode(): Index(RootIdx), Parent(nullptr) {}

  explicit IndexTrieNode(unsigned V, IndexTrieNode *P): Index(V), Parent(P) {}

  IndexTrieNode(IndexTrieNode &) =delete;
  IndexTrieNode &operator=(const IndexTrieNode&) =delete;

  ~IndexTrieNode() {
    for (auto *N : Children)
      delete N;
  }

  bool isRoot() const { return Index == RootIdx; }

  bool isLeaf() const { return Children.empty(); }

  unsigned getIndex() const { return Index; }

  IndexTrieNode *getChild(unsigned Idx) {
    assert(Idx != RootIdx);

    auto I = std::lower_bound(Children.begin(), Children.end(), Idx,
                              [](IndexTrieNode *a, unsigned i) {
                                return a->Index < i;
                              });
    if (I != Children.end() && (*I)->Index == Idx)
      return *I;
    auto *N = new IndexTrieNode(Idx, this);
    Children.insert(I, N);
    return N;
  }

  ArrayRef<IndexTrieNode*> getChildren() const { return Children; }

  IndexTrieNode *getParent() const { return Parent; }

  /// Returns true when the sequence of indices represented by this
  /// node is a prefix of the sequence represented by the passed-in node.
  bool isPrefixOf(const IndexTrieNode *Other) const {
    const IndexTrieNode *I = Other;

    do {
      if (this == I)
        return true;

      I = I->getParent();
    } while (I);

    return false;
  }

  void dumpRef(llvm::raw_ostream &out, bool last = true) const {
    if (Parent) {
      Parent->dumpRef(out, false);
      out << ", ";
      out << Index;
    } else {
      out << "(Root";
    }

    if (last)
      out << ")";
  }

  LLVM_ATTRIBUTE_DEPRECATED(void dumpRef() const LLVM_ATTRIBUTE_USED,
                            "Only meant for use in the debugger") {
    dumpRef(llvm::errs());
  }
};

static inline
llvm::raw_ostream &operator<<(llvm::raw_ostream &out, IndexTrieNode *ITN) {
  ITN->dumpRef(out);
  return out;
}

} // end namespace swift

#endif
