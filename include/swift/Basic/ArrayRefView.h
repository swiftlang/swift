//===- ArrayRefView.h - Adapter for iterating over an ArrayRef --*- C++ -*-===//
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
//  This file defines ArrayRefView, a template class which provides a
//  proxied view of the elements of an array.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ARRAYREFVIEW_H
#define SWIFT_BASIC_ARRAYREFVIEW_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/Support/Casting.h"

namespace swift {

/// An adapter for iterating over a range of values as a range of
/// values of a different type.
template <class Orig, class Projected, Projected (&Project)(const Orig &),
          bool AllowOrigAccess = false>
class ArrayRefView {
  llvm::ArrayRef<Orig> Array;
public:
  ArrayRefView() {}
  ArrayRefView(llvm::ArrayRef<Orig> array) : Array(array) {}

  class iterator {
    friend class ArrayRefView<Orig,Projected,Project,AllowOrigAccess>;
    const Orig *Ptr;
    iterator(const Orig *ptr) : Ptr(ptr) {}
  public:
    using value_type = Projected;
    using reference = Projected;
    using pointer = void;
    using difference_type = ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    Projected operator*() const { return Project(*Ptr); }
    Projected operator->() const { return operator*(); }
    iterator &operator++() { Ptr++; return *this; }
    iterator operator++(int) { return iterator(Ptr++); }
    iterator &operator--() { Ptr--; return *this; }
    iterator operator--(int) { return iterator(Ptr--); }
    bool operator==(iterator rhs) const { return Ptr == rhs.Ptr; }
    bool operator!=(iterator rhs) const { return Ptr != rhs.Ptr; }

    iterator &operator+=(difference_type i) {
      Ptr += i;
      return *this;
    }
    iterator operator+(difference_type i) const {
      return iterator(Ptr + i);
    }
    friend iterator operator+(difference_type i, iterator base) {
      return iterator(base.Ptr + i);
    }
    iterator &operator-=(difference_type i) {
      Ptr -= i;
      return *this;
    }
    iterator operator-(difference_type i) const {
      return iterator(Ptr - i);
    }
    difference_type operator-(iterator rhs) const {
      return Ptr - rhs.Ptr;
    }
    Projected operator[](difference_type i) const {
      return Project(Ptr[i]);
    }
    bool operator<(iterator rhs) const {
      return Ptr < rhs.Ptr;
    }
    bool operator<=(iterator rhs) const {
      return Ptr <= rhs.Ptr;
    }
    bool operator>(iterator rhs) const {
      return Ptr > rhs.Ptr;
    }
    bool operator>=(iterator rhs) const {
      return Ptr >= rhs.Ptr;
    }
  };
  iterator begin() const { return iterator(Array.begin()); }
  iterator end() const { return iterator(Array.end()); }

  bool empty() const { return Array.empty(); }
  size_t size() const { return Array.size(); }
  Projected operator[](unsigned i) const { return Project(Array[i]); }
  Projected front() const { return Project(Array.front()); }
  Projected back() const { return Project(Array.back()); }

  ArrayRefView drop_back(unsigned count = 1) const {
    return ArrayRefView(Array.drop_back(count));
  }

  ArrayRefView slice(unsigned start) const {
    return ArrayRefView(Array.slice(start));
  }
  ArrayRefView slice(unsigned start, unsigned length) const {
    return ArrayRefView(Array.slice(start, length));
  }

  /// Peek through to the underlying array.  This operation is not
  /// supported by default; it must be enabled at specialization time.
  llvm::ArrayRef<Orig> getOriginalArray() const {
    static_assert(AllowOrigAccess,
                  "original array access not enabled for this view");
    return Array;
  }

  friend bool operator==(ArrayRefView lhs, ArrayRefView rhs) {
    if (lhs.size() != rhs.size())
      return false;
    for (auto i : indices(lhs))
      if (lhs[i] != rhs[i])
        return false;
    return true;
  }
  friend bool operator==(llvm::ArrayRef<Projected> lhs, ArrayRefView rhs) {
    if (lhs.size() != rhs.size())
      return false;
    for (auto i : indices(lhs))
      if (lhs[i] != rhs[i])
        return false;
    return true;
  }
  friend bool operator==(ArrayRefView lhs, llvm::ArrayRef<Projected> rhs) {
    if (lhs.size() != rhs.size())
      return false;
    for (auto i : indices(lhs))
      if (lhs[i] != rhs[i])
        return false;
    return true;
  }

  friend bool operator!=(ArrayRefView lhs, ArrayRefView rhs) {
    return !(lhs == rhs);
  }
  friend bool operator!=(llvm::ArrayRef<Projected> lhs, ArrayRefView rhs) {
    return !(lhs == rhs);
  }
  friend bool operator!=(ArrayRefView lhs, llvm::ArrayRef<Projected> rhs) {
    return !(lhs == rhs);
  }
};

/// Helper for \c CastArrayRefView that casts the original type to the
/// projected type.
template<typename Projected, typename Orig>
inline Projected *arrayRefViewCastHelper(const Orig &value) {
  using llvm::cast_or_null;
  return cast_or_null<Projected>(value);
}

/// An ArrayRefView that performs a cast_or_null on each element in the
/// underlying ArrayRef.
template<typename Orig, typename Projected>
using CastArrayRefView =
  ArrayRefView<Orig, Projected *, arrayRefViewCastHelper<Projected, Orig>>;

namespace generator_details {
template <class T> struct is_array_ref_like;

template <class Orig, class Projected, Projected (&Project)(const Orig &),
          bool AllowOrigAccess>
struct is_array_ref_like<ArrayRefView<Orig, Projected, Project,
                                      AllowOrigAccess>> {
  enum { value = true };
};
}

} // end namespace swift

#endif // SWIFT_BASIC_ARRAYREFVIEW_H
