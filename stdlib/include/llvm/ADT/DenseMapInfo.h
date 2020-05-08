//===- llvm/ADT/DenseMapInfo.h - Type traits for DenseMap -------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines DenseMapInfo traits for DenseMap.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_DENSEMAPINFO_H
#define LLVM_ADT_DENSEMAPINFO_H

#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/StringRef.h"
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <utility>

namespace llvm {

namespace detail {

/// Simplistic combination of 32-bit hash values into 32-bit hash values.
static inline unsigned combineHashValue(unsigned a, unsigned b) {
  uint64_t key = (uint64_t)a << 32 | (uint64_t)b;
  key += ~(key << 32);
  key ^= (key >> 22);
  key += ~(key << 13);
  key ^= (key >> 8);
  key += (key << 3);
  key ^= (key >> 15);
  key += ~(key << 27);
  key ^= (key >> 31);
  return (unsigned)key;
}

} // end namespace detail

template<typename T>
struct DenseMapInfo {
  //static inline T getEmptyKey();
  //static inline T getTombstoneKey();
  //static unsigned getHashValue(const T &Val);
  //static bool isEqual(const T &LHS, const T &RHS);
};

// Provide DenseMapInfo for all pointers. Come up with sentinel pointer values
// that are aligned to alignof(T) bytes, but try to avoid requiring T to be
// complete. This allows clients to instantiate DenseMap<T*, ...> with forward
// declared key types. Assume that no pointer key type requires more than 4096
// bytes of alignment.
template<typename T>
struct DenseMapInfo<T*> {
  // The following should hold, but it would require T to be complete:
  // static_assert(alignof(T) <= (1 << Log2MaxAlign),
  //               "DenseMap does not support pointer keys requiring more than "
  //               "Log2MaxAlign bits of alignment");
  static constexpr uintptr_t Log2MaxAlign = 12;

  static inline T* getEmptyKey() {
    uintptr_t Val = static_cast<uintptr_t>(-1);
    Val <<= Log2MaxAlign;
    return reinterpret_cast<T*>(Val);
  }

  static inline T* getTombstoneKey() {
    uintptr_t Val = static_cast<uintptr_t>(-2);
    Val <<= Log2MaxAlign;
    return reinterpret_cast<T*>(Val);
  }

  static unsigned getHashValue(const T *PtrVal) {
    return (unsigned((uintptr_t)PtrVal) >> 4) ^
           (unsigned((uintptr_t)PtrVal) >> 9);
  }

  static bool isEqual(const T *LHS, const T *RHS) { return LHS == RHS; }
};

// Provide DenseMapInfo for chars.
template<> struct DenseMapInfo<char> {
  static inline char getEmptyKey() { return ~0; }
  static inline char getTombstoneKey() { return ~0 - 1; }
  static unsigned getHashValue(const char& Val) { return Val * 37U; }

  static bool isEqual(const char &LHS, const char &RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned chars.
template <> struct DenseMapInfo<unsigned char> {
  static inline unsigned char getEmptyKey() { return ~0; }
  static inline unsigned char getTombstoneKey() { return ~0 - 1; }
  static unsigned getHashValue(const unsigned char &Val) { return Val * 37U; }

  static bool isEqual(const unsigned char &LHS, const unsigned char &RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned shorts.
template <> struct DenseMapInfo<unsigned short> {
  static inline unsigned short getEmptyKey() { return 0xFFFF; }
  static inline unsigned short getTombstoneKey() { return 0xFFFF - 1; }
  static unsigned getHashValue(const unsigned short &Val) { return Val * 37U; }

  static bool isEqual(const unsigned short &LHS, const unsigned short &RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned ints.
template<> struct DenseMapInfo<unsigned> {
  static inline unsigned getEmptyKey() { return ~0U; }
  static inline unsigned getTombstoneKey() { return ~0U - 1; }
  static unsigned getHashValue(const unsigned& Val) { return Val * 37U; }

  static bool isEqual(const unsigned& LHS, const unsigned& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned longs.
template<> struct DenseMapInfo<unsigned long> {
  static inline unsigned long getEmptyKey() { return ~0UL; }
  static inline unsigned long getTombstoneKey() { return ~0UL - 1L; }

  static unsigned getHashValue(const unsigned long& Val) {
    return (unsigned)(Val * 37UL);
  }

  static bool isEqual(const unsigned long& LHS, const unsigned long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for unsigned long longs.
template<> struct DenseMapInfo<unsigned long long> {
  static inline unsigned long long getEmptyKey() { return ~0ULL; }
  static inline unsigned long long getTombstoneKey() { return ~0ULL - 1ULL; }

  static unsigned getHashValue(const unsigned long long& Val) {
    return (unsigned)(Val * 37ULL);
  }

  static bool isEqual(const unsigned long long& LHS,
                      const unsigned long long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for shorts.
template <> struct DenseMapInfo<short> {
  static inline short getEmptyKey() { return 0x7FFF; }
  static inline short getTombstoneKey() { return -0x7FFF - 1; }
  static unsigned getHashValue(const short &Val) { return Val * 37U; }
  static bool isEqual(const short &LHS, const short &RHS) { return LHS == RHS; }
};

// Provide DenseMapInfo for ints.
template<> struct DenseMapInfo<int> {
  static inline int getEmptyKey() { return 0x7fffffff; }
  static inline int getTombstoneKey() { return -0x7fffffff - 1; }
  static unsigned getHashValue(const int& Val) { return (unsigned)(Val * 37U); }

  static bool isEqual(const int& LHS, const int& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for longs.
template<> struct DenseMapInfo<long> {
  static inline long getEmptyKey() {
    return (1UL << (sizeof(long) * 8 - 1)) - 1UL;
  }

  static inline long getTombstoneKey() { return getEmptyKey() - 1L; }

  static unsigned getHashValue(const long& Val) {
    return (unsigned)(Val * 37UL);
  }

  static bool isEqual(const long& LHS, const long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for long longs.
template<> struct DenseMapInfo<long long> {
  static inline long long getEmptyKey() { return 0x7fffffffffffffffLL; }
  static inline long long getTombstoneKey() { return -0x7fffffffffffffffLL-1; }

  static unsigned getHashValue(const long long& Val) {
    return (unsigned)(Val * 37ULL);
  }

  static bool isEqual(const long long& LHS,
                      const long long& RHS) {
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for all pairs whose members have info.
template<typename T, typename U>
struct DenseMapInfo<std::pair<T, U>> {
  using Pair = std::pair<T, U>;
  using FirstInfo = DenseMapInfo<T>;
  using SecondInfo = DenseMapInfo<U>;

  static inline Pair getEmptyKey() {
    return std::make_pair(FirstInfo::getEmptyKey(),
                          SecondInfo::getEmptyKey());
  }

  static inline Pair getTombstoneKey() {
    return std::make_pair(FirstInfo::getTombstoneKey(),
                          SecondInfo::getTombstoneKey());
  }

  static unsigned getHashValue(const Pair& PairVal) {
    return detail::combineHashValue(FirstInfo::getHashValue(PairVal.first),
                                    SecondInfo::getHashValue(PairVal.second));
  }

  static bool isEqual(const Pair &LHS, const Pair &RHS) {
    return FirstInfo::isEqual(LHS.first, RHS.first) &&
           SecondInfo::isEqual(LHS.second, RHS.second);
  }
};

// Provide DenseMapInfo for all tuples whose members have info.
template <typename... Ts> struct DenseMapInfo<std::tuple<Ts...>> {
  using Tuple = std::tuple<Ts...>;

  static inline Tuple getEmptyKey() {
    return Tuple(DenseMapInfo<Ts>::getEmptyKey()...);
  }

  static inline Tuple getTombstoneKey() {
    return Tuple(DenseMapInfo<Ts>::getTombstoneKey()...);
  }

  template <unsigned I>
  static unsigned getHashValueImpl(const Tuple &values, std::false_type) {
    using EltType = typename std::tuple_element<I, Tuple>::type;
    std::integral_constant<bool, I + 1 == sizeof...(Ts)> atEnd;
    return detail::combineHashValue(
        DenseMapInfo<EltType>::getHashValue(std::get<I>(values)),
        getHashValueImpl<I + 1>(values, atEnd));
  }

  template <unsigned I>
  static unsigned getHashValueImpl(const Tuple &values, std::true_type) {
    return 0;
  }

  static unsigned getHashValue(const std::tuple<Ts...> &values) {
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return getHashValueImpl<0>(values, atEnd);
  }

  template <unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::false_type) {
    using EltType = typename std::tuple_element<I, Tuple>::type;
    std::integral_constant<bool, I + 1 == sizeof...(Ts)> atEnd;
    return DenseMapInfo<EltType>::isEqual(std::get<I>(lhs), std::get<I>(rhs)) &&
           isEqualImpl<I + 1>(lhs, rhs, atEnd);
  }

  template <unsigned I>
  static bool isEqualImpl(const Tuple &lhs, const Tuple &rhs, std::true_type) {
    return true;
  }

  static bool isEqual(const Tuple &lhs, const Tuple &rhs) {
    std::integral_constant<bool, 0 == sizeof...(Ts)> atEnd;
    return isEqualImpl<0>(lhs, rhs, atEnd);
  }
};

// Provide DenseMapInfo for StringRefs.
template <> struct DenseMapInfo<StringRef> {
  static inline StringRef getEmptyKey() {
    return StringRef(reinterpret_cast<const char *>(~static_cast<uintptr_t>(0)),
                     0);
  }

  static inline StringRef getTombstoneKey() {
    return StringRef(reinterpret_cast<const char *>(~static_cast<uintptr_t>(1)),
                     0);
  }

  static unsigned getHashValue(StringRef Val) {
    assert(Val.data() != getEmptyKey().data() && "Cannot hash the empty key!");
    assert(Val.data() != getTombstoneKey().data() &&
           "Cannot hash the tombstone key!");
    return (unsigned)(hash_value(Val));
  }

  static bool isEqual(StringRef LHS, StringRef RHS) {
    if (RHS.data() == getEmptyKey().data())
      return LHS.data() == getEmptyKey().data();
    if (RHS.data() == getTombstoneKey().data())
      return LHS.data() == getTombstoneKey().data();
    return LHS == RHS;
  }
};

// Provide DenseMapInfo for ArrayRefs.
template <typename T> struct DenseMapInfo<ArrayRef<T>> {
  static inline ArrayRef<T> getEmptyKey() {
    return ArrayRef<T>(reinterpret_cast<const T *>(~static_cast<uintptr_t>(0)),
                       size_t(0));
  }

  static inline ArrayRef<T> getTombstoneKey() {
    return ArrayRef<T>(reinterpret_cast<const T *>(~static_cast<uintptr_t>(1)),
                       size_t(0));
  }

  static unsigned getHashValue(ArrayRef<T> Val) {
    assert(Val.data() != getEmptyKey().data() && "Cannot hash the empty key!");
    assert(Val.data() != getTombstoneKey().data() &&
           "Cannot hash the tombstone key!");
    return (unsigned)(hash_value(Val));
  }

  static bool isEqual(ArrayRef<T> LHS, ArrayRef<T> RHS) {
    if (RHS.data() == getEmptyKey().data())
      return LHS.data() == getEmptyKey().data();
    if (RHS.data() == getTombstoneKey().data())
      return LHS.data() == getTombstoneKey().data();
    return LHS == RHS;
  }
};

template <> struct DenseMapInfo<hash_code> {
  static inline hash_code getEmptyKey() { return hash_code(-1); }
  static inline hash_code getTombstoneKey() { return hash_code(-2); }
  static unsigned getHashValue(hash_code val) { return val; }
  static bool isEqual(hash_code LHS, hash_code RHS) { return LHS == RHS; }
};

} // end namespace llvm

#endif // LLVM_ADT_DENSEMAPINFO_H
