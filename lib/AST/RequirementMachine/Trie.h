//===--- Trie.h - Trie with terms as keys ---------------------------------===//
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

#ifndef SWIFT_RQM_TRIE_H
#define SWIFT_RQM_TRIE_H

#include "llvm/ADT/DenseMap.h"
#include "Histogram.h"

namespace swift {

namespace rewriting {

enum class MatchKind {
  Shortest,
  Longest
};

template<typename ValueType, MatchKind Kind>
class Trie {
public:
  struct Node;

  struct Entry {
    Optional<ValueType> Value;
    Node *Children = nullptr;
  };

  struct Node {
    llvm::SmallDenseMap<Symbol, Entry, 1> Entries;
  };

private:
  /// We never delete nodes, except for when the entire trie is torn down.
  std::vector<Node *> Nodes;

  /// The root is stored directly.
  Node Root;

public:
  void updateHistograms(Histogram &stats, Histogram &rootStats) const {
    for (const auto &node : Nodes)
      stats.add(node->Entries.size());
    rootStats.add(Root.Entries.size());
  }

  /// Delete all entries from the trie.
  void clear() {
    Root.Entries.clear();

    for (auto iter = Nodes.rbegin(); iter != Nodes.rend(); ++iter) {
      auto *node = *iter;
      delete node;
    }

    Nodes.clear();
  }

  ~Trie() {
    clear();
  }

  /// Insert an entry with the key given by the range [begin, end).
  /// Returns the old value if the trie already had an entry for this key;
  /// this is actually an invariant violation, but we can produce a better
  /// assertion further up the stack.
  template<typename Iter>
  Optional<ValueType> insert(Iter begin, Iter end, ValueType value) {
    assert(begin != end);
    auto *node = &Root;

    while (true) {
      auto &entry = node->Entries[*begin];
      ++begin;

      if (begin == end) {
        if (entry.Value)
          return entry.Value;

        entry.Value = value;
        return None;
      }

      if (entry.Children == nullptr) {
        entry.Children = new Node();
        Nodes.push_back(entry.Children);
      }

      node = entry.Children;
    }
  }

  /// Find the shortest or longest prefix of the range given by [begin,end),
  /// depending on whether the Kind template parameter was bound to
  /// MatchKind::Shortest or MatchKind::Longest.
  template<typename Iter>
  Optional<ValueType>
  find(Iter begin, Iter end) const {
    assert(begin != end);
    auto *node = &Root;

    Optional<ValueType> bestMatch = None;

    while (true) {
      auto found = node->Entries.find(*begin);
      ++begin;

      if (found == node->Entries.end())
        return bestMatch;

      const auto &entry = found->second;

      if (entry.Value) {
        if (Kind == MatchKind::Shortest)
          return entry.Value;

        bestMatch = entry.Value;
      }

      if (begin == end)
        return bestMatch;

      if (entry.Children == nullptr)
        return bestMatch;

      node = entry.Children;
    }
  }
};

}  // end namespace rewriting

}  // end namespace swift

#endif