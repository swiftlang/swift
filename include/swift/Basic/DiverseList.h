//===--- DiverseList.h - List of variably-sized objects ---------*- C++ -*-===//
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
//
// This file defines a data structure for representing a list of
// variably-sized objects.  It is a requirement that the object type
// be trivially movable, meaning that it has a trivial move
// constructor and a trivial destructor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIVERSELIST_H
#define SWIFT_BASIC_DIVERSELIST_H

#include "swift/Basic/Malloc.h"
#include <cassert>
#include <cstring>
#include <utility>

namespace swift {

template <class T> class DiverseListImpl;

/// DiverseList - A list of heterogeneously-typed objects.
///
/// \tparam T - A common base class of the objects in the list; must
///   provide an allocated_size() const method.
/// \tparam InlineCapacity - the amount of inline storage to provide, in bytes.
template <class T, unsigned InlineCapacity>
class DiverseList : public DiverseListImpl<T> {
  char InlineStorage[InlineCapacity];

public:
  DiverseList() : DiverseListImpl<T>(InlineStorage + InlineCapacity) {}
  DiverseList(const DiverseList &other)
    : DiverseListImpl<T>(other, InlineStorage + InlineCapacity) {}
  DiverseList(const DiverseListImpl<T> &other)
    : DiverseListImpl<T>(other, InlineStorage + InlineCapacity) {}
  DiverseList(DiverseList<T, InlineCapacity> &&other)
    : DiverseListImpl<T>(std::move(other), InlineStorage + InlineCapacity) {}
  DiverseList(DiverseListImpl<T> &&other)
    : DiverseListImpl<T>(std::move(other), InlineStorage + InlineCapacity) {}
};

/// A base class for DiverseListImpl.
class DiverseListBase {
public:
  /// The first element in the list and the beginning of the allocation.
  char *Begin;

  /// A pointer past the last element in the list.
  char *End;

  /// A pointer past the end of the allocation.
  char *EndOfAllocation;

  bool isAllocatedInline() const {
    return (Begin == reinterpret_cast<const char *>(this + 1));
  }

  void checkValid() const {
    assert(Begin <= End);
    assert(End <= EndOfAllocation);
  }

  void initialize(char *endOfAllocation) {
    EndOfAllocation = endOfAllocation;
    Begin = End = reinterpret_cast<char*>(this + 1);
  }
  void copyFrom(const DiverseListBase &other) {
    // Ensure that we're large enough to store all the data.
    std::size_t size = static_cast<std::size_t>(other.End - other.Begin);
    char *newStorage = addNewStorage(size);
    std::memcpy(newStorage, other.Begin, size);
  }
  char *addNewStorage(std::size_t needed) {
    checkValid();
    if (std::size_t(EndOfAllocation - End) >= needed) {
      char *newStorage = End;
      End += needed;
      return newStorage;
    }
    return addNewStorageSlow(needed);
  }
  char *addNewStorageSlow(std::size_t needed);

  /// A stable iterator is the equivalent of an index into the list.
  /// It's an iterator that stays stable across modification of the
  /// list.
  class stable_iterator {
    std::size_t Offset;
    friend class DiverseListBase;
    template <class T> friend class DiverseListImpl;
    stable_iterator(std::size_t offset) : Offset(offset) {}
  public:
    stable_iterator() = default;
    friend bool operator==(stable_iterator a, stable_iterator b) {
      return a.Offset == b.Offset;
    }
    friend bool operator!=(stable_iterator a, stable_iterator b) {
      return !operator==(a, b);
    }    
  };
  stable_iterator stable_begin() const {
    return stable_iterator(0);
  }
  stable_iterator stable_end() {
    return stable_iterator(std::size_t(End - Begin));
  }

protected:
  ~DiverseListBase() {
    checkValid();
    if (!isAllocatedInline())
      delete[] Begin;
  }
};

/// An "abstract" base class for DiverseList<T> which does not
/// explicitly set the preferred inline capacity.  Most of the
/// implementation is in this class.
template <class T> class DiverseListImpl : private DiverseListBase {
  DiverseListImpl(const DiverseListImpl<T> &other) = delete;
  DiverseListImpl(DiverseListImpl<T> &&other) = delete;

protected:
  DiverseListImpl(char *endOfAllocation) {
    initialize(endOfAllocation);
  }

  DiverseListImpl(const DiverseListImpl<T> &other, char *endOfAllocation) {
    initialize(endOfAllocation);
    copyFrom(other);
  }
  DiverseListImpl(DiverseListImpl<T> &&other, char *endOfAllocation) {
    // If the other is allocated inline, just initialize and copy.
    if (other.isAllocatedInline()) {
      initialize(endOfAllocation);
      copyFrom(other);
      return;
    }

    // Otherwise, steal its allocations.
    Begin = other.Begin;
    End = other.End;
    EndOfAllocation = other.EndOfAllocation;
    other.Begin = other.End = other.EndOfAllocation = (char*) (&other + 1);
    assert(other.isAllocatedInline());
  }
  
public:
  /// Query whether the stack is empty.
  bool empty() const {
    checkValid();
    return Begin == End;
  }

  /// Return a reference to the first element in the list.
  T &front() {
    assert(!empty());
    return *reinterpret_cast<T*>(Begin);
  }

  /// Return a reference to the first element in the list.
  const T &front() const {
    assert(!empty());
    return *reinterpret_cast<const T*>(Begin);
  }

  class const_iterator;
  class iterator {
    char *Ptr;
    friend class DiverseListImpl;
    friend class const_iterator;
    iterator(char *ptr) : Ptr(ptr) {}

  public:
    iterator() = default;

    T &operator*() const { return *reinterpret_cast<T*>(Ptr); }
    T *operator->() const { return reinterpret_cast<T*>(Ptr); }
    iterator &operator++() {
      Ptr += (*this)->allocated_size();
      return *this;
    }
    iterator operator++(int _) {
      auto copy = *this;
      operator++();
      return copy;
    }

    /// advancePast - Like operator++, but asserting that the current
    /// object has a known type.
    template <class U> void advancePast() {
      assert((*this)->allocated_size() == sizeof(U));
      Ptr += sizeof(U);
    }

    friend bool operator==(iterator a, iterator b) { return a.Ptr == b.Ptr; }
    friend bool operator!=(iterator a, iterator b) { return !operator==(a, b); }
  };
  iterator begin() { checkValid(); return iterator(Begin); }
  iterator end() { checkValid(); return iterator(End); }
  iterator find(stable_iterator it) {
    checkValid();
    assert(it.Offset <= End - Begin);
    return iterator(Begin + it.Offset);
  }
  stable_iterator stabilize(iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
    return stable_iterator(it.Ptr - Begin);
  } 

  class const_iterator {
    const char *Ptr;
    friend class DiverseListImpl;
    const_iterator(const char *ptr) : Ptr(ptr) {}
  public:
    const_iterator() = default;
    const_iterator(iterator it) : Ptr(it.Ptr) {}

    const T &operator*() const { return *reinterpret_cast<const T*>(Ptr); }
    const T *operator->() const { return reinterpret_cast<const T*>(Ptr); }
    const_iterator &operator++() {
      Ptr += (*this)->allocated_size();
      return *this;
    }
    const_iterator operator++(int _) {
      auto copy = *this;
      operator++();
      return copy;
    }

    /// advancePast - Like operator++, but asserting that the current
    /// object has a known type.
    template <class U> void advancePast() {
      assert((*this)->allocated_size() == sizeof(U));
      Ptr += sizeof(U);
    }

    friend bool operator==(const_iterator a, const_iterator b) {
      return a.Ptr == b.Ptr;
    }
    friend bool operator!=(const_iterator a, const_iterator b) {
      return !operator==(a, b);
    }
  };
  const_iterator begin() const { checkValid(); return const_iterator(Begin); }
  const_iterator end() const { checkValid(); return const_iterator(End); }
  const_iterator find(stable_iterator it) const {
    checkValid();
    assert(it.Offset <= End - Begin);
    return const_iterator(Begin + it.Offset);
  }
  stable_iterator stabilize(const_iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
    return stable_iterator(it.Ptr - Begin);
  }

  /// Add a new object onto the end of the list.
  template <class U, class... A> U &add(A && ...args) {
    char *storage = addNewStorage(sizeof(U));
    U &newObject = *::new (storage) U(::std::forward<A>(args)...);
    return newObject;
  }

  /// Add a new object onto the end of the list with some extra storage.
  template <class U, class... A> U &addWithExtra(size_t extra, A && ...args) {
    char *storage = addNewStorage(sizeof(U) + extra);
    U &newObject = *::new (storage) U(::std::forward<A>(args)...);
    return newObject;
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_DIVERSELIST_H
