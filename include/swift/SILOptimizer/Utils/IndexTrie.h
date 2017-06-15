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

public:
  IndexTrieNode(): Index(RootIdx) {}

  explicit IndexTrieNode(unsigned V): Index(V) {}

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
    auto *N = new IndexTrieNode(Idx);
    Children.insert(I, N);
    return N;
  }

  ArrayRef<IndexTrieNode*> getChildren() const { return Children; }
};

} // end namespace swift

#endif
