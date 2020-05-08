//===- llvm/ADT/PointerUnion.h - Discriminated Union of 2 Ptrs --*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file defines the PointerUnion class, which is a discriminated union of
// pointer types.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_POINTERUNION_H
#define LLVM_ADT_POINTERUNION_H

#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/PointerIntPair.h"
#include "llvm/Support/PointerLikeTypeTraits.h"
#include <cassert>
#include <cstddef>
#include <cstdint>

namespace llvm {

template <typename T> struct PointerUnionTypeSelectorReturn {
  using Return = T;
};

/// Get a type based on whether two types are the same or not.
///
/// For:
///
/// \code
///   using Ret = typename PointerUnionTypeSelector<T1, T2, EQ, NE>::Return;
/// \endcode
///
/// Ret will be EQ type if T1 is same as T2 or NE type otherwise.
template <typename T1, typename T2, typename RET_EQ, typename RET_NE>
struct PointerUnionTypeSelector {
  using Return = typename PointerUnionTypeSelectorReturn<RET_NE>::Return;
};

template <typename T, typename RET_EQ, typename RET_NE>
struct PointerUnionTypeSelector<T, T, RET_EQ, RET_NE> {
  using Return = typename PointerUnionTypeSelectorReturn<RET_EQ>::Return;
};

template <typename T1, typename T2, typename RET_EQ, typename RET_NE>
struct PointerUnionTypeSelectorReturn<
    PointerUnionTypeSelector<T1, T2, RET_EQ, RET_NE>> {
  using Return =
      typename PointerUnionTypeSelector<T1, T2, RET_EQ, RET_NE>::Return;
};

namespace pointer_union_detail {
  /// Determine the number of bits required to store integers with values < n.
  /// This is ceil(log2(n)).
  constexpr int bitsRequired(unsigned n) {
    return n > 1 ? 1 + bitsRequired((n + 1) / 2) : 0;
  }

  template <typename... Ts> constexpr int lowBitsAvailable() {
    return std::min<int>({PointerLikeTypeTraits<Ts>::NumLowBitsAvailable...});
  }

  /// Find the index of a type in a list of types. TypeIndex<T, Us...>::Index
  /// is the index of T in Us, or sizeof...(Us) if T does not appear in the
  /// list.
  template <typename T, typename ...Us> struct TypeIndex;
  template <typename T, typename ...Us> struct TypeIndex<T, T, Us...> {
    static constexpr int Index = 0;
  };
  template <typename T, typename U, typename... Us>
  struct TypeIndex<T, U, Us...> {
    static constexpr int Index = 1 + TypeIndex<T, Us...>::Index;
  };
  template <typename T> struct TypeIndex<T> {
    static constexpr int Index = 0;
  };

  /// Find the first type in a list of types.
  template <typename T, typename...> struct GetFirstType {
    using type = T;
  };

  /// Provide PointerLikeTypeTraits for void* that is used by PointerUnion
  /// for the template arguments.
  template <typename ...PTs> class PointerUnionUIntTraits {
  public:
    static inline void *getAsVoidPointer(void *P) { return P; }
    static inline void *getFromVoidPointer(void *P) { return P; }
    static constexpr int NumLowBitsAvailable = lowBitsAvailable<PTs...>();
  };

  /// Implement assignment in terms of construction.
  template <typename Derived, typename T> struct AssignableFrom {
    Derived &operator=(T t) {
      return static_cast<Derived &>(*this) = Derived(t);
    }
  };

  template <typename Derived, typename ValTy, int I, typename ...Types>
  class PointerUnionMembers;

  template <typename Derived, typename ValTy, int I>
  class PointerUnionMembers<Derived, ValTy, I> {
  protected:
    ValTy Val;
    PointerUnionMembers() = default;
    PointerUnionMembers(ValTy Val) : Val(Val) {}

    friend struct PointerLikeTypeTraits<Derived>;
  };

  template <typename Derived, typename ValTy, int I, typename Type,
            typename ...Types>
  class PointerUnionMembers<Derived, ValTy, I, Type, Types...>
      : public PointerUnionMembers<Derived, ValTy, I + 1, Types...> {
    using Base = PointerUnionMembers<Derived, ValTy, I + 1, Types...>;
  public:
    using Base::Base;
    PointerUnionMembers() = default;
    PointerUnionMembers(Type V)
        : Base(ValTy(const_cast<void *>(
                         PointerLikeTypeTraits<Type>::getAsVoidPointer(V)),
                     I)) {}

    using Base::operator=;
    Derived &operator=(Type V) {
      this->Val = ValTy(
          const_cast<void *>(PointerLikeTypeTraits<Type>::getAsVoidPointer(V)),
          I);
      return static_cast<Derived &>(*this);
    };
  };
}

/// A discriminated union of two or more pointer types, with the discriminator
/// in the low bit of the pointer.
///
/// This implementation is extremely efficient in space due to leveraging the
/// low bits of the pointer, while exposing a natural and type-safe API.
///
/// Common use patterns would be something like this:
///    PointerUnion<int*, float*> P;
///    P = (int*)0;
///    printf("%d %d", P.is<int*>(), P.is<float*>());  // prints "1 0"
///    X = P.get<int*>();     // ok.
///    Y = P.get<float*>();   // runtime assertion failure.
///    Z = P.get<double*>();  // compile time failure.
///    P = (float*)0;
///    Y = P.get<float*>();   // ok.
///    X = P.get<int*>();     // runtime assertion failure.
template <typename... PTs>
class PointerUnion
    : public pointer_union_detail::PointerUnionMembers<
          PointerUnion<PTs...>,
          PointerIntPair<
              void *, pointer_union_detail::bitsRequired(sizeof...(PTs)), int,
              pointer_union_detail::PointerUnionUIntTraits<PTs...>>,
          0, PTs...> {
  // The first type is special because we want to directly cast a pointer to a
  // default-initialized union to a pointer to the first type. But we don't
  // want PointerUnion to be a 'template <typename First, typename ...Rest>'
  // because it's much more convenient to have a name for the whole pack. So
  // split off the first type here.
  using First = typename pointer_union_detail::GetFirstType<PTs...>::type;
  using Base = typename PointerUnion::PointerUnionMembers;

public:
  PointerUnion() = default;

  PointerUnion(std::nullptr_t) : PointerUnion() {}
  using Base::Base;

  /// Test if the pointer held in the union is null, regardless of
  /// which type it is.
  bool isNull() const { return !this->Val.getPointer(); }

  explicit operator bool() const { return !isNull(); }

  /// Test if the Union currently holds the type matching T.
  template <typename T> bool is() const {
    constexpr int Index = pointer_union_detail::TypeIndex<T, PTs...>::Index;
    static_assert(Index < sizeof...(PTs),
                  "PointerUnion::is<T> given type not in the union");
    return this->Val.getInt() == Index;
  }

  /// Returns the value of the specified pointer type.
  ///
  /// If the specified pointer type is incorrect, assert.
  template <typename T> T get() const {
    assert(is<T>() && "Invalid accessor called");
    return PointerLikeTypeTraits<T>::getFromVoidPointer(this->Val.getPointer());
  }

  /// Returns the current pointer if it is of the specified pointer type,
  /// otherwises returns null.
  template <typename T> T dyn_cast() const {
    if (is<T>())
      return get<T>();
    return T();
  }

  /// If the union is set to the first pointer type get an address pointing to
  /// it.
  First const *getAddrOfPtr1() const {
    return const_cast<PointerUnion *>(this)->getAddrOfPtr1();
  }

  /// If the union is set to the first pointer type get an address pointing to
  /// it.
  First *getAddrOfPtr1() {
    assert(is<First>() && "Val is not the first pointer");
    assert(
        PointerLikeTypeTraits<First>::getAsVoidPointer(get<First>()) ==
            this->Val.getPointer() &&
        "Can't get the address because PointerLikeTypeTraits changes the ptr");
    return const_cast<First *>(
        reinterpret_cast<const First *>(this->Val.getAddrOfPointer()));
  }

  /// Assignment from nullptr which just clears the union.
  const PointerUnion &operator=(std::nullptr_t) {
    this->Val.initWithPointer(nullptr);
    return *this;
  }

  /// Assignment from elements of the union.
  using Base::operator=;

  void *getOpaqueValue() const { return this->Val.getOpaqueValue(); }
  static inline PointerUnion getFromOpaqueValue(void *VP) {
    PointerUnion V;
    V.Val = decltype(V.Val)::getFromOpaqueValue(VP);
    return V;
  }
};

template <typename ...PTs>
bool operator==(PointerUnion<PTs...> lhs, PointerUnion<PTs...> rhs) {
  return lhs.getOpaqueValue() == rhs.getOpaqueValue();
}

template <typename ...PTs>
bool operator!=(PointerUnion<PTs...> lhs, PointerUnion<PTs...> rhs) {
  return lhs.getOpaqueValue() != rhs.getOpaqueValue();
}

template <typename ...PTs>
bool operator<(PointerUnion<PTs...> lhs, PointerUnion<PTs...> rhs) {
  return lhs.getOpaqueValue() < rhs.getOpaqueValue();
}

// Teach SmallPtrSet that PointerUnion is "basically a pointer", that has
// # low bits available = min(PT1bits,PT2bits)-1.
template <typename ...PTs>
struct PointerLikeTypeTraits<PointerUnion<PTs...>> {
  static inline void *getAsVoidPointer(const PointerUnion<PTs...> &P) {
    return P.getOpaqueValue();
  }

  static inline PointerUnion<PTs...> getFromVoidPointer(void *P) {
    return PointerUnion<PTs...>::getFromOpaqueValue(P);
  }

  // The number of bits available are the min of the pointer types minus the
  // bits needed for the discriminator.
  static constexpr int NumLowBitsAvailable = PointerLikeTypeTraits<decltype(
      PointerUnion<PTs...>::Val)>::NumLowBitsAvailable;
};

// Teach DenseMap how to use PointerUnions as keys.
template <typename ...PTs> struct DenseMapInfo<PointerUnion<PTs...>> {
  using Union = PointerUnion<PTs...>;
  using FirstInfo =
      DenseMapInfo<typename pointer_union_detail::GetFirstType<PTs...>::type>;

  static inline Union getEmptyKey() { return Union(FirstInfo::getEmptyKey()); }

  static inline Union getTombstoneKey() {
    return Union(FirstInfo::getTombstoneKey());
  }

  static unsigned getHashValue(const Union &UnionVal) {
    intptr_t key = (intptr_t)UnionVal.getOpaqueValue();
    return DenseMapInfo<intptr_t>::getHashValue(key);
  }

  static bool isEqual(const Union &LHS, const Union &RHS) {
    return LHS == RHS;
  }
};

} // end namespace llvm

#endif // LLVM_ADT_POINTERUNION_H
