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

class Trie {
public:
  struct Node;

  struct Entry {
    Optional<unsigned> RuleID;
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

  /// The destructor deletes all nodes.
  ~Trie() {
    for (auto iter = Nodes.rbegin(); iter != Nodes.rend(); ++iter) {
      auto *node = *iter;
      delete node;
    }

    Nodes.clear();
  }

  /// Insert an entry with the key given by the range [begin, end).
  /// Returns the old value if the trie already had an entry for this key;
  /// this is actually an invariant violation, but we can produce a better
  /// assertion further up the stack.
  template<typename Iter>
  Optional<unsigned> insert(Iter begin, Iter end, unsigned ruleID) {
    assert(begin != end);
    auto *node = &Root;

    while (true) {
      auto &entry = node->Entries[*begin];
      ++begin;

      if (begin == end) {
        if (entry.RuleID)
          return entry.RuleID;

        entry.RuleID = ruleID;
        return None;
      }

      if (entry.Children == nullptr) {
        entry.Children = new Node();
        Nodes.push_back(entry.Children);
      }

      node = entry.Children;
    }
  }

  /// Find the shortest prefix of the range given by [begin,end).
  template<typename Iter>
  Optional<unsigned>
  find(Iter begin, Iter end) const {
    assert(begin != end);
    auto *node = &Root;

    while (true) {
      auto found = node->Entries.find(*begin);
      ++begin;

      if (found == node->Entries.end())
        return None;

      const auto &entry = found->second;
      if (begin == end || entry.RuleID)
        return entry.RuleID;

      if (entry.Children == nullptr)
        return None;

      node = entry.Children;
    }
  }
};

}  // end namespace rewriting

}  // end namespace swift

#endif