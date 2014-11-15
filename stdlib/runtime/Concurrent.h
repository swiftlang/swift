//===--- Concurrent.h - Concurrent Data Structures  -----------------------===//
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
#ifndef SWIFT_RUNTIME_CONCURRENTUTILS_H
#define SWIFT_RUNTIME_CONCURRENTUTILS_H
#include <iterator>
#include <atomic>
#include <stdint.h>

/// This is a node in an concurrent linked list.
template <class ElemTy> struct ConcurrentListNode {
  /// C'tor.
  ConcurrentListNode(ElemTy Elem) : Payload(Elem), Next(nullptr) {}
  /// D'tor.
  ~ConcurrentListNode() {}
  /// The element.
  ElemTy Payload;
  /// Points to the next link in the chain.
  ConcurrentListNode<ElemTy> *Next;
};

/// This is a concurrent linked list. It supports insertion at the beginning
/// of the list and traversal using iterators.
template <class ElemTy> struct ConcurrentList {
  /// C'tor.
  ConcurrentList() : First(nullptr) {}
  /// D'tor.
  ~ConcurrentList() {
    // Iterate over the list and delete all the nodes.
    auto Ptr = First.load();
    while (Ptr) {
      auto N = Ptr->Next;
      delete Ptr;
      Ptr = N;
    }
  }

  /// A list iterator.
  struct ConcurrentListIterator {
    /// Points to the current link.
    ConcurrentListNode<ElemTy> *Ptr;
    /// C'tor.
    ConcurrentListIterator(ConcurrentListNode<ElemTy> *P) : Ptr(P) {}
    /// Move to the next element.
    ConcurrentListIterator &operator++() {
      Ptr = Ptr->Next;
      return *this;
    }
    /// Access the element.
    ElemTy &operator*() { return Ptr->Payload; }
    /// Same?
    bool operator==(const ConcurrentListIterator &o) const {
      return o.Ptr == Ptr;
    }
    /// Not the same?
    bool operator!=(const ConcurrentListIterator &o) const {
      return o.Ptr != Ptr;
    }
  };

  /// Iterator entry point.
  typedef ConcurrentListIterator iterator;
  /// Marks the beginning of the list.
  iterator begin() const { return ConcurrentListIterator(First); }
  /// Marks the end of the list.
  iterator end() const { return ConcurrentListIterator(nullptr); }

  /// Add a new item to the list.
  /// \returns a reference to the inserted element.
  ElemTy& push(ElemTy Elem) {
    /// Allocate a new node.
    ConcurrentListNode<ElemTy> *N = new ConcurrentListNode<ElemTy>(Elem);
    // Point to the first element in the list.
    N->Next = First.load();
    auto OldFirst = N->Next;
    // Try to replace the current First with the new node.
    while (!std::atomic_compare_exchange_weak(&First, &OldFirst, N)) {
      // If we fail, update the new node to point to the new head and try to
      // insert before the new
      // first element.
      N->Next = OldFirst;
    }
    return N->Payload;
  }
  std::atomic<ConcurrentListNode<ElemTy> *> First;
};

template <class KeyTy, class ValueTy> struct ConcurrentMapNode {
  /// C'tor.
  ConcurrentMapNode(KeyTy H)
      : Payload(), Left(nullptr), Right(nullptr), Key(H) {}

  // D'tor.
  ~ConcurrentMapNode() {
    delete Left.load();
    delete Right.load();
  }

  ValueTy Payload;
  typedef std::atomic<ConcurrentMapNode *> EdgeTy;
  EdgeTy Left;
  EdgeTy Right;
  KeyTy Key;
};

// A concurrent map that is implemented using a binary tree. It supports
// concurrent insertions but does not support removals or rebalancing of
// the tree.
template <class KeyTy, class ValueTy> class ConcurrentMap {
public:
  /// C'tor.
  ConcurrentMap() : Sentinel(0) {}
  /// A Sentinel root node that contains no elements.
  ConcurrentMapNode<KeyTy, ValueTy> Sentinel;

  /// Search a for a node with key value \p. If the node does not exist then
  /// allocate a new node and add it to the tree.
  ValueTy &findOrAllocateNode(KeyTy Key) {
    return findOrAllocateNode_rec(&Sentinel, Key);
  }

private:
  ValueTy &findOrAllocateNode_rec(ConcurrentMapNode<KeyTy, ValueTy> *P,
                                  KeyTy Key) {
    // Found the node we were looking for.
    if (P->Key == Key)
      return P->Payload;

    // Point to the edge we want to replace.
    typename ConcurrentMapNode<KeyTy, ValueTy>::EdgeTy *Edge = nullptr;

    // Select the edge to follow.
    if (P->Key > Key) {
      Edge = &P->Left;
    } else {
      Edge = &P->Right;
    }

    // Load the current edge value.
    ConcurrentMapNode<KeyTy, ValueTy> *CurrentVal = Edge->load();

    // If the edge is populated the follow the edge.
    if (CurrentVal)
      return findOrAllocateNode_rec(CurrentVal, Key);

    // Allocate a new node.
    ConcurrentMapNode<KeyTy, ValueTy> *New =
        new ConcurrentMapNode<KeyTy, ValueTy>(Key);

    // Try to set a new node:
    if (std::atomic_compare_exchange_weak(Edge, &CurrentVal, New)) {
      // On success return the new node.
      return New->Payload;
    }

    // If failed, deallocate the node and look for a new place in the
    // tree. Some other thread may have created a new entry and we may
    // discover it, so start searching with the current node.
    delete New;
    return findOrAllocateNode_rec(P, Key);
  }
};

#endif // SWIFT_RUNTIME_CONCURRENTUTILS_H
