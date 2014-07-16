//===- swift/Basic/DenseMapInfo.h - DenseMapInfo Extras ---------*- C++ -*-===//
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
// This file defines extra DenseMapInfo traits.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_BASIC_DENSE_MAP_INFO_H
#define SWIFT_BASIC_DENSE_MAP_INFO_H

#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include <tuple>
#include <type_traits>

namespace llvm {

/// Provide DenseMapInfo for all tuples whose members have info.
template<typename ...Ts>
struct DenseMapInfo<std::tuple<Ts...> > {
  typedef std::tuple<Ts...> Tuple;

  /// Helper class 
  template<unsigned N> struct UnsignedC { };

  static inline Tuple getEmptyKey() {
    return std::make_tuple(DenseMapInfo<Ts>::getEmptyKey()...);
  }

  static inline Tuple getTombstoneKey() {
    return std::tuple<Ts...>(DenseMapInfo<Ts>::getTombstoneKey()...);
  }

  template<unsigned I> 
  static hash_code getHashValueImpl(const Tuple& values, std::false_type) {
    typedef typename std::tuple_element<I, Tuple>::type EltType;
    std::integral_constant<bool, I+1 == sizeof...(Ts)> atEnd;
    return hash_combine(
               DenseMapInfo<EltType>::getHashValue(std::get<I>(values)),
               getHashValueImpl<I+1>(values, atEnd));
  }

  template<unsigned I> 
  static hash_code getHashValueImpl(const Tuple& values, std::true_type) {
    return hash_code();
  }

  static unsigned getHashValue(const std::tuple<Ts...>& values) {
    // FIXME: Mix upper/lower 32-bit values together to produce
    // unsigned rather than truncating.
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return getHashValueImpl<0>(values, atEnd);
  }

  template<unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::false_type) {
    typedef typename std::tuple_element<I, Tuple>::type EltType;
    std::integral_constant<bool, I+1 == sizeof...(Ts)> atEnd;
    return DenseMapInfo<EltType>::isEqual(std::get<I>(lhs), std::get<I>(rhs))
      && isEqualImpl<I+1>(lhs, rhs, atEnd);
  }

  template<unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::true_type) {
    return true;
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return isEqualImpl<0>(lhs, rhs, atEnd);
  }
};

} // end namespace llvm

#endif // LLVM_SWIFT_BASIC_DENSE_MAP_INFO_H
