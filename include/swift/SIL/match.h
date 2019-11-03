#ifndef SWIFT_SIL_MATCH_H
#define SWIFT_SIL_MATCH_H

#include "swift/SIL/SILInstruction.h"
#include "llvm/ADT/Optional.h"

namespace swift {

template <template <int...> class _Tp, class>
struct instance_of : std::false_type {};

template <template <int...> class _Tp, int... _Args>
struct instance_of<_Tp, _Tp<_Args...>> : std::true_type {};

template <class T, bool R, class L = char> struct InfoBlock {
  typedef L pred_t;
  typedef T type;
  static constexpr inline bool value = R;

  L lambda;

  InfoBlock(L lambda = 0) : lambda(lambda) {}
};

template <class T, bool R, class L = char>
static InfoBlock<T, R, L> make_info_block(L lambda = 0) {
  return InfoBlock<T, R, L>(lambda);
}

struct Dummy {
  static bool classof(const ValueBase *) { return true; };
  static bool classof(const SILNode *) { return true; }
};

template <int Begin = 0, int End = std::numeric_limits<int>::max()>
struct Range {
  static constexpr inline bool value = false;
  static constexpr inline bool begin = Begin;
  static constexpr inline bool end = End;

  typedef char pred_t;
  typedef Dummy type;
};

using AnyRange = Range<>;

template <typename, typename> struct OutPtr;

template <typename T, typename... Args> struct OutPtr<T, std::tuple<Args...>> {
  using type = std::tuple<typename T::type *, Args...>;
};

template <typename...> struct filter;

template <> struct filter<> { using type = std::tuple<>; };

template <typename Head, typename... Tail> struct filter<Head, Tail...> {
  using type = typename std::conditional<
      Head::value, typename OutPtr<Head, typename filter<Tail...>::type>::type,
      typename filter<Tail...>::type>::type;
};

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
