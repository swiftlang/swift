//===--- DiverseStack.h - Stack of variably-sized objects -------*- C++ -*-===//
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
///
/// \file
///
/// This file defines a data structure for representing a stack of
/// variably-sized objects.  It is a requirement that the object type
/// be trivially movable, meaning that it has a trivial move
/// constructor and a trivial destructor.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_DIVERSESTACK_H
#define SWIFT_BASIC_DIVERSESTACK_H

#include "swift/Basic/Malloc.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <cstring>
#include <utility>

namespace swift {

template <class T> class DiverseStackImpl;

/// DiverseStack - A stack of heterogeneously-typed objects.
///
/// \tparam T - A common base class of the objects on the stack; must
///   provide an allocated_size() const method.
/// \tparam InlineCapacity - the amount of inline storage to provide, in bytes.
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
      return !operator==(a, b);
    }

    static stable_iterator invalid() {
      return stable_iterator((std::size_t) -1);
    }
    bool isValid() const {
      return Depth != (std::size_t) -1;
    }

    std::size_t getDepth() const { return Depth; }

    /// A helper class that wraps a stable_iterator as something that
    /// pretends to be a non-null pointer.
    ///
    /// This allows stable_iterators to be placed in TinyPtrVector.
    ///
    /// A wrapper is needed because we don't want to give stable_iterator
    /// a null inhabitant, an operator bool, conversions from nullptr_t, or
    /// any similar features that TinyPtrVector reasonably requires of its
    /// element types.
    class AsPointer {
      void *EncodedValue;
      explicit AsPointer(void *encodedValue) : EncodedValue(encodedValue) {}
    public:
      enum { NumLowBitsAvailable = 3 };

      /// Allow a null AsPointer to be created with either 'nullptr' or
      /// 'AsPointer()'.
      /*implicit*/ AsPointer(std::nullptr_t _ = nullptr)
        : EncodedValue(nullptr) {}

      /// Allow an AsPointer to be tested as a boolean value.
      explicit operator bool() const { return EncodedValue != nullptr; }

      /// Allow an AsPointer to be compared for equality with a void*.
      friend bool operator==(AsPointer lhs, void *rhs) {
        return lhs.EncodedValue == rhs;
      }

      /// Allow an implicit conversion from stable_iterator.
      /*implicit*/ AsPointer(stable_iterator it) {
        assert(it.isValid() && "can't encode invalid stable_iterator");
        auto encodedDepth = (it.Depth + 1) << NumLowBitsAvailable;
        EncodedValue = reinterpret_cast<void*>(encodedDepth);
        assert(EncodedValue && "encoded pointer was null");
      }

      /// Allow an implicit conversion to stable_iterator.
      operator stable_iterator() const {
        assert(EncodedValue && "can't decode null pointer");
        auto encodedDepth = reinterpret_cast<std::size_t>(EncodedValue);
        auto depth = (encodedDepth >> NumLowBitsAvailable) - 1;
        auto it = stable_iterator(depth);
        assert(it.isValid() && "decoded stable_iterator was invalid");
        return it;
      }

      void *getAsVoidPointer() const { return EncodedValue; }
      static AsPointer getFromVoidPointer(void *ptr) { return AsPointer(ptr); }
    };
    AsPointer asPointer() const { return *this; }
  };
  stable_iterator stable_begin() const {
    return stable_iterator(End - Begin);
  }
  static stable_iterator stable_end() {
    return stable_iterator(0);
  }

  void checkIterator(stable_iterator it) const {
    assert(it.isValid() && "checking an invalid iterator");
    checkValid();
    assert(it.Depth <= size_t(End - Begin));
  }
};

template <class T> class DiverseStackImpl : private DiverseStackBase {
  DiverseStackImpl(const DiverseStackImpl<T> &other) = delete;
  DiverseStackImpl(DiverseStackImpl<T> &&other) = delete;

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
    other.Begin = other.End = other.Allocated = (char*) (&other + 1);
    assert(other.isAllocatedInline());
  }
  
public:
  ~DiverseStackImpl() {
    checkValid();
    if (!isAllocatedInline())
      delete[] Allocated;
  }

  /// Query whether the stack is empty.
  bool empty() const {
    checkValid();
    return Begin == End;
  }

  /// Return a reference to the top element on the stack.
  T &top() {
    assert(!empty());
    return *reinterpret_cast<T*>(Begin);
  }

  /// Return a reference to the top element on the stack.
  const T &top() const {
    assert(!empty());
    return *reinterpret_cast<const T*>(Begin);
  }

  using DiverseStackBase::stable_iterator;
  using DiverseStackBase::stable_begin;
  using DiverseStackBase::stable_end;

  class const_iterator;
  class iterator {
    char *Ptr;
    friend class DiverseStackImpl;
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

  using DiverseStackBase::checkIterator;
  void checkIterator(iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
  }

  iterator begin() { checkValid(); return iterator(Begin); }
  iterator end() { checkValid(); return iterator(End); }
  iterator find(stable_iterator it) {
    checkIterator(it);
    return iterator(End - it.Depth);
  }
  stable_iterator stabilize(iterator it) const {
    checkIterator(it);
    return stable_iterator(End - it.Ptr);
  } 

  class const_iterator {
    const char *Ptr;
    friend class DiverseStackImpl;
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
  void checkIterator(const_iterator it) const {
    checkValid();
    assert(Begin <= it.Ptr && it.Ptr <= End);
  }
  const_iterator find(stable_iterator it) const {
    checkIterator(it);
    return const_iterator(End - it.Depth);
  }
  stable_iterator stabilize(const_iterator it) const {
    checkIterator(it);
    return stable_iterator(End - it.Ptr);
  }

  /// Push a new object onto the stack.
  template <class U, class... A> U &push(A && ...args) {
    pushNewStorage(sizeof(U));
    return *::new (Begin) U(::std::forward<A>(args)...);
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

  /// Pop objects off of the stack until \p the object pointed to by stable_iter
  /// is the top element of the stack.
  void pop(stable_iterator stable_iter) {
    iterator iter = find(stable_iter);
    checkIterator(iter);
    while (Begin != iter.Ptr) {
      pop();
      checkIterator(iter);
    }
  }
};

} // end namespace swift

/// Allow stable_iterators to be put in things like TinyPtrVectors.
namespace llvm {
  template <>
  class PointerLikeTypeTraits<
                      swift::DiverseStackBase::stable_iterator::AsPointer> {
    using AsPointer = swift::DiverseStackBase::stable_iterator::AsPointer;
  public:
    static inline void *getAsVoidPointer(AsPointer ptr) {
      return ptr.getAsVoidPointer();
    }
    static inline AsPointer getFromVoidPointer(void *ptr) {
      return AsPointer::getFromVoidPointer(ptr);
    }

    enum {
      NumLowBitsAvailable = AsPointer::NumLowBitsAvailable
    };
  };
}

#endif // SWIFT_BASIC_DIVERSESTACK_H
