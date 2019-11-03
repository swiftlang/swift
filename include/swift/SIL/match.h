//===--- Match.h - Range based matching function -------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2019 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
///
/// \file
/// This file contains the source for a range based matching function. The function takes a begin and end iterator
/// which it will use to try to match each element agains a corresponding parameter.
///
//===----------------------------------------------------------------------===//


#ifndef SWIFT_SIL_MATCH_H
#define SWIFT_SIL_MATCH_H

#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Optional.h"

namespace swift {

template <template <int...> class _Tp, class>
struct instance_of : std::false_type {};

template <template <int...> class _Tp, int... _Args>
struct instance_of<_Tp, _Tp<_Args...>> : std::true_type {};

/// InfoBlock is a struct that is used to give info about the element you want to match.
/// \p T is the type that you want to match. The match function will use
/// \code dyn_cast \endcode to try to cast the current element to this type. \p R is
/// a boolean which tells the matching function weather you want the element that you have just
/// matched back. If this is true, that element will be added to the returned tuple.
/// \p L is an optional lambda type. If the dynamic cast succeeds the lambda will be passed the
/// element. If the lambda returns true, the matching succeeds otherwise the matching fails.
template <class T, bool R, class L = char>
struct InfoBlock {
  typedef L pred_t;
  typedef T type;
  static constexpr inline bool value = R;

  L lambda;

  InfoBlock(L lambda = 0) : lambda(lambda) {}
};

/// make_info_block is a helper function for creating an InfoBlock.
/// make_info_block should be used when you want to supply an optional
/// lambda to the InfoBlock. \p lambda is the lambda, all other
/// arguments are the same as InfoBlock.
template <class T, bool R, class L = char>
static InfoBlock<T, R, L> make_info_block(L lambda = 0) {
  return InfoBlock<T, R, L>(lambda);
}

struct Dummy {
  static bool classof(const ValueBase *) { return true; };
  static bool classof(const SILNode *) { return true; }
};

/// A range can be used to ignore a certain number of elements. The range will
/// ignore at least \p Begin elements and will ignore up to \p End elements.
template <int Begin = 0, int End = std::numeric_limits<int>::max()>
struct Range {
  static constexpr inline bool value = false;
  static constexpr inline int begin = Begin;
  static constexpr inline int end = End;

  typedef char pred_t;
  typedef Dummy type;
};

/// Any range indicates that any number of elements may exist
/// before matching the next element using an InfoBlock.
using AnyRange = Range<>;

template <class, class>
struct OutPtr;

template <class T, class... Args>
struct OutPtr<T, std::tuple<Args...>> {
  using type = std::tuple<typename T::type *, Args...>;
};

template <class...>
struct filter;

template <>
struct filter<> { using type = std::tuple<>; };

template <class Head, class... Tail>
struct filter<Head, Tail...> {
  using type = typename std::conditional<
    Head::value,
    typename OutPtr<Head, typename filter<Tail...>::type>::type,
    typename filter<Tail...>::type
  >::type;
};

/// match takes a begin and end iterator as arguments \p first and \p last respectively.
/// Any arguments after that can be used to match the given range. These arguments are known
/// as \p blocks, and they can be either of type InfoBlock or Range.
///
/// Example usage:
///
/// \code
/// // Match test program:
/// //  func test(_x: Int) -> Int {
/// //      var x = _x
/// //      x += 1
/// //      x &= (x - 1)
/// //      return x - 1
/// //  }
/// ReturnInst *retVal;
///
/// for (auto &block : F) {
///   auto matchedResult = match(
///       block.begin(), block.end(), AnyRange(),
///       make_info_block<BuiltinInst, false>([](BuiltinInst *bi) {
///         return bi->getBuiltinKind().hasValue() &&
///                bi->getBuiltinKind().getValue() == BuiltinValueKind::SAddOver;
///       }),
///       AnyRange(), make_info_block<BuiltinInst, false>([](BuiltinInst *bi) {
///         return bi->getBuiltinKind().hasValue() &&
///                bi->getBuiltinKind().getValue() == BuiltinValueKind::SSubOver;
///       }),
///       AnyRange(), make_info_block<BuiltinInst, false>([](BuiltinInst *bi) {
///         return bi->getBuiltinKind().hasValue() &&
///                bi->getBuiltinKind().getValue() == BuiltinValueKind::And;
///       }),
///       AnyRange(), make_info_block<BuiltinInst, false>([](BuiltinInst *bi) {
///         return bi->getBuiltinKind().hasValue() &&
///                bi->getBuiltinKind().getValue() == BuiltinValueKind::SSubOver;
///       }),
///       AnyRange(), make_info_block<ReturnInst, true>());
///
///   if (!matchedResult.hasValue())
///     continue;
///
///   std::tie(retVal) = matchedResult.getValue();
/// }
/// \endcode
///
template <class T, class B1, class... Blocks>
Optional<typename filter<B1, Blocks...>::type> match(T first, T last, B1 block,
                                                     Blocks... blocks) {
  if (first == last)
    return None;

  if constexpr (instance_of<Range, B1>::value) {
    for (size_t i = B1::begin; first != last && i != B1::end; ++first, ++i) {
      if (auto others = match(first, last, blocks...)) {
        return {others.getValue()};
      }
    }
  }

  if constexpr (sizeof...(Blocks) > 0) {
    auto *element = dyn_cast<typename B1::type>(first);
    if (element == nullptr)
      return None;

    if (auto others = match(++first, last, blocks...)) {
      if constexpr (B1::value) {
        if constexpr (!std::is_same<typename B1::pred_t, char>::value) {
          if (!block.lambda(element))
            return None;
        }

        return {std::tuple_cat(std::make_tuple(element), others.getValue())};
      } else {
        return {others.getValue()};
      }
    }
  } else {
    if (auto *element = dyn_cast<typename B1::type>(first)) {
      if constexpr (!std::is_same<typename B1::pred_t, char>::value) {
        if (!block.lambda(element))
          return None;
      }

      if constexpr (!B1::value) {
        return {};
      }

      return {std::make_tuple(element)};
    }
  }

  return None;
}

} // namespace swift

#endif
