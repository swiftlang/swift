//===--- DiverseStack.h - Stack of variably-sized objects -------*- C++ -*-===//
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
//
// This file defines a data structure for representing a stack of
// variably-sized objects.  It is a requirement that the object type
// be trivially movable, meaning that it has a trivial move
// constructor and a trivial destructor.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_DIVERSESTACK_H
#define SWIFT_IRGEN_DIVERSESTACK_H

#include <cassert>
#include <cstring>
#include <type_traits>
#include <utility>

namespace swift {
namespace irgen {

template <class T> class DiverseStackImpl;

/// DiverseStack - A stack of heterogenously-typed objects.
///
/// \param T - A common base class of the objects on the stack; must
///   provide an allocated_size() const method.
/// \param InlineCapacity - the amount of inline storage to provide, in bytes
template <class T, unsigned InlineCapacity>
class DiverseStack : public DiverseStackImpl<T> {
  char InlineStorage[InlineCapacity];

public:
  DiverseStack() : DiverseStackImpl<T>(InlineStorage + InlineCapacity) {}
  DiverseStack(const DiverseStack &other)
    : DiverseStackImpl<T>(other, InlineStorage + InlineCapacity) {}
  DiverseStack(const DiverseStackImpl<T> &other)
    : DiverseStackImpl<T>(other, InlineStorage + InlineCapacity) {}
  DiverseStack(DiverseStack<T, InlineCapacity> &&other)
    : DiverseStackImpl<T>(std::move(other), InlineStorage + InlineCapacity) {}
  DiverseStack(DiverseStackImpl<T> &&other)
    : DiverseStackImpl<T>(std::move(other), InlineStorage + InlineCapacity) {}
};

/// A base class for DiverseStackImpl.
class DiverseStackBase {
public:
  /// The top of the stack.
  char *Begin;

  /// The bottom of the stack, i.e. the end of the allocation.
  char *End;

  /// The beginning of the allocation.
  char *Allocated;

  bool isAllocatedInline() const {
    return (Allocated == reinterpret_cast<const char *>(this + 1));
  }

  void checkValid() const {
    assert(Allocated <= Begin);
    assert(Begin <= End);
  }

  void initialize(char *end) {
    Begin = End = end;
    Allocated = reinterpret_cast<char*>(this + 1);
  }
  void copyFrom(const DiverseStackBase &other) {
    // Ensure that we're large enough to store all the data.
    std::size_t size = static_cast<std::size_t>(other.End - other.Begin);
    pushNewStorage(size);
    std::memcpy(Begin, other.Begin, size);
  }
  void pushNewStorage(std::size_t needed) {
    checkValid();
    if (std::size_t(Begin - Allocated) >= needed) {
      Begin -= needed;
    } else {
      pushNewStorageSlow(needed);
    }
  }
  void pushNewStorageSlow(std::size_t needed);

  /// A stable iterator is the equivalent of an index into the stack.
  /// It's an iterator that stays stable across modification of the
  /// stack.
  class stable_iterator {
    std::size_t Depth;
    friend class DiverseStackBase;
    template <class T> friend class DiverseStackImpl;
    stable_iterator(std::size_t depth) : Depth(depth) {}
  public:
    stable_iterator() = default;
    friend bool operator==(stable_iterator a, stable_iterator b) {
      return a.Depth == b.Depth;
    }
    friend bool operator!=(stable_iterator a, stable_iterator b) {
      return a.Depth != b.Depth;
    }    
  };
  stable_iterator stable_begin() const {
    return stable_iterator(End - Begin);
  }
  static stable_iterator stable_end() {
    return stable_iterator(0);
  }
};

template <class T> class DiverseStackImpl : private DiverseStackBase {
  DiverseStackImpl(const DiverseStackImpl<T> &other) = delete;
  DiverseStackImpl(DiverseStackImpl<T> &&other) = delete;

  static_assert(std::is_trivially_destructible<T>::value,
                "element base type must be trivially destructible");
  static_assert(std::is_trivially_move_constructible<T>::value,
                "element base type must be trivially moveable");
  static_assert(std::is_trivially_copy_constructible<T>::value,
                "element base type msut be trivially copyable");

protected:
  DiverseStackImpl(char *end) {
    initialize(end);
  }

  DiverseStackImpl(const DiverseStackImpl<T> &other, char *end) {
    initialize(end);
    copyFrom(other);
  }

  DiverseStackImpl(DiverseStackImpl<T> &&other, char *end) {
    // If the other is allocated inline, just initialize and copy.
    if (other.isAllocatedInline()) {
      initialize(end);
      copyFrom(other);
      return;
    }

    // Otherwise, steal its allocations.
    Begin = other.Begin;
    End = other.End;
    Allocated = other.Allocated;
    other.Begin = other.End = other.Allocated = other + 1;
    assert(other.isAllocatedInline());
  }

public:
  ~DiverseStackImpl() {
    checkValid();
    if (!isAllocatedInline())
      delete[] Allocated;
  }

  bool empty() const { checkValid(); return Begin == End; }
  T *top() { assert(!empty()); return static_cast<T*>(Begin); }
  const T *top() const { assert(!empty()); return static_cast<T*>(Begin); }

  class const_iterator;
  class iterator {
    char *Ptr;
    friend class DiverseStackImpl;
    friend class const_iterator;
    iterator(char *ptr) : Ptr(ptr) {}
  public:
    iterator() = default;

    T &operator*() const { return *static_cast<T*>(Ptr); }
    T *operator->() const { return static_cast<T*>(Ptr); }
    iterator &operator++() {
      Ptr += (*this)->allocated_size();
      return *this;
    }
    iterator operator++(int _) {
      iterator copy = *this;
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
    friend bool operator!=(iterator a, iterator b) { return a.Ptr != b.Ptr; }
  };
  iterator begin() { checkValid(); return iterator(Begin); }
  iterator end() { checkValid(); return iterator(End); }
  iterator find(stable_iterator it) {
    checkValid();
    assert(it.Depth <= End - Begin);
    return iterator(End - it.Depth);
  }
  stable_iterator stabilize(iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
    return stable_iterator(End - it.Ptr);
  } 

  class const_iterator {
    const char *Ptr;
    friend class DiverseStackImpl;
    const_iterator(const char *ptr) : Ptr(ptr) {}
  public:
    const_iterator() = default;
    const_iterator(iterator it) : Ptr(it.Ptr) {}

    const T &operator*() const { return *static_cast<const T*>(Ptr); }
    const T *operator->() const { return static_cast<const T*>(Ptr); }
    const_iterator &operator++() {
      Ptr += (*this)->allocated_size();
      return *this;
    }
    const_iterator operator++(int _) {
      const_iterator copy = *this;
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
      return a.Ptr != b.Ptr;
    }
  };
  const_iterator begin() const { checkValid(); return const_iterator(Begin); }
  const_iterator end() const { checkValid(); return const_iterator(End); }
  const_iterator find(stable_iterator it) const {
    checkValid();
    assert(it.Depth <= End - Begin);
    return const_iterator(End - it.Depth);
  }
  stable_iterator stabilize(const_iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
    return stable_iterator(End - it.Ptr);
  }

  /// Push a new object onto the stack.
  template <class U, class... A> U *push(A && ...args) {
    static_assert(std::is_trivially_destructible<U>::value,
                  "pushed type must be trivially destructible");
    static_assert(std::is_trivially_move_constructible<U>::value,
                  "pushed type must be trivially moveable");
    static_assert(std::is_trivially_copy_constructible<U>::value,
                  "pushed type must be trivially copyable");

    pushNewStorage(sizeof(U));
    return ::new (Begin) U(::std::forward<A>(args)...);
  }

  /// Pop an object off the stack.
  void pop() {
    assert(!empty());
    Begin += top().allocated_size();
  }

  /// Pop an object of known type off the stack.
  template <class U> void pop() {
    assert(!empty());
    assert(sizeof(U) == top().allocated_size());
    Begin += sizeof(U);
  }
};

} // end namespace irgen
} // end namespace swift

#endif
