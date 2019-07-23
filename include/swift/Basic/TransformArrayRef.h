//===--- TransformArrayRef.h ------------------------------------*- C++ -*-===//
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
/// This file defines TransformArrayRef, a template class that provides a
/// transformed view of an ArrayRef. The difference from ArrayRefView is that
/// ArrayRefView takes its transform as a template argument, while
/// TransformArrayRef only takes a type as its template argument. This means it
/// can be used to define types used with forward declaration pointers without
/// needing to define the relevant function in headers.
///
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_TRANSFORMARRAYREF_H
#define SWIFT_BASIC_TRANSFORMARRAYREF_H

#include "swift/Basic/STLExtras.h"
#include "llvm/ADT/ArrayRef.h"

namespace swift {

/// A transformation of an ArrayRef using a function of type FuncTy. This is
/// different than ArrayRefView since the underlying function is stored instead
/// of used as a template parameter. This allows it to be used in declarations
/// where the underlying function is not known. This is useful when defining the
/// underlying function would require forward declarations to need to be
/// defined.
template <class FuncTy>
class TransformArrayRef {
public:
  using FunctionTraits = function_traits<FuncTy>;
  using Orig =
    typename std::tuple_element<0, typename FunctionTraits::argument_types>::type;
  using Projected = typename FunctionTraits::result_type;

private:
  llvm::ArrayRef<Orig> Array;
  FuncTy Func;

public:
  TransformArrayRef(llvm::ArrayRef<Orig> array, FuncTy func) : Array(array), Func(func) {}

  class iterator {
    friend class TransformArrayRef<FuncTy>;
    const Orig *Ptr;
    FuncTy Func;
    iterator(const Orig *ptr, FuncTy func) : Ptr(ptr), Func(func) {}
  public:
    using value_type = Projected;
    using reference = Projected;
    using pointer = void;
    using difference_type = ptrdiff_t;
    using iterator_category = std::random_access_iterator_tag;

    Projected operator*() const { return Func(*Ptr); }
    iterator &operator++() { Ptr++; return *this; }
    iterator operator++(int) { return iterator(Ptr++, Func); }
    iterator &operator--() { --Ptr; return *this; }
    iterator operator--(int) { return iterator(Ptr--, Func); }

    bool operator==(iterator rhs) const { return Ptr == rhs.Ptr; }
    bool operator!=(iterator rhs) const { return Ptr != rhs.Ptr; }

    iterator &operator+=(difference_type i) {
      Ptr += i;
      return *this;
    }
    iterator operator+(difference_type i) const {
      return iterator(Ptr + i, Func);
    }
    friend iterator operator+(difference_type i, iterator base) {
      return iterator(base.Ptr + i, base.Func);
    }
    iterator &operator-=(difference_type i) {
      Ptr -= i;
      return *this;
    }
    iterator operator-(difference_type i) const {
      return iterator(Ptr - i, Func);
    }
    difference_type operator-(iterator rhs) const {
      return Ptr - rhs.Ptr;
    }
    Projected operator[](difference_type i) const {
      return Func(Ptr[i]);
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
  iterator begin() const { return iterator(Array.begin(), Func); }
  iterator end() const { return iterator(Array.end(), Func); }

  bool empty() const { return Array.empty(); }
  size_t size() const { return Array.size(); }
  Projected operator[](unsigned i) const { return Func(Array[i]); }
  Projected front() const { return Func(Array.front()); }
  Projected back() const { return Func(Array.back()); }

  TransformArrayRef slice(unsigned start) const {
    return TransformArrayRef(Array.slice(start), Func);
  }
  TransformArrayRef slice(unsigned start, unsigned length) const {
    return TransformArrayRef(Array.slice(start, length), Func);
  }
};

template <class Orig, class Proj>
TransformArrayRef<std::function<Proj (Orig)>>
makeTransformArrayRef(llvm::ArrayRef<Orig> Array,
                      std::function<Proj (Orig)> Func) {
  return TransformArrayRef<decltype(Func)>(Array, Func);
}

} // namespace swift

#endif
