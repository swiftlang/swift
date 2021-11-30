//===- llvm/ADT/STLExtras.h - Useful STL related functions ------*- C++ -*-===//
//
// Part of the LLVM Project, under the Apache License v2.0 with LLVM Exceptions.
// See https://llvm.org/LICENSE.txt for license information.
// SPDX-License-Identifier: Apache-2.0 WITH LLVM-exception
//
//===----------------------------------------------------------------------===//
//
// This file contains some templates that are useful if you are working with the
// STL at all.
//
// No library is required when using these functions.
//
//===----------------------------------------------------------------------===//

#ifndef LLVM_ADT_STLEXTRAS_H
#define LLVM_ADT_STLEXTRAS_H

#include "llvm/ADT/Optional.h"
#include "llvm/ADT/STLForwardCompat.h"
#include "llvm/ADT/iterator.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Config/abi-breaking.h"
#include "llvm/Support/ErrorHandling.h"
#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <functional>
#include <initializer_list>
#include <iterator>
#include <limits>
#include <memory>
#include <tuple>
#include <type_traits>
#include <utility>

#ifdef EXPENSIVE_CHECKS
#include <random> // for std::mt19937
#endif

namespace llvm {

// Only used by compiler if both template types are the same.  Useful when
// using SFINAE to test for the existence of member functions.
template <typename T, T> struct SameType;

namespace detail {

template <typename RangeT>
using IterOfRange = decltype(std::begin(std::declval<RangeT &>()));

template <typename RangeT>
using ValueOfRange = typename std::remove_reference<decltype(
    *std::begin(std::declval<RangeT &>()))>::type;

} // end namespace detail

//===----------------------------------------------------------------------===//
//     Extra additions to <type_traits>
//===----------------------------------------------------------------------===//

template <typename T> struct make_const_ptr {
  using type =
      typename std::add_pointer<typename std::add_const<T>::type>::type;
};

template <typename T> struct make_const_ref {
  using type = typename std::add_lvalue_reference<
      typename std::add_const<T>::type>::type;
};

namespace detail {
template <typename...> using void_t = void;
template <class, template <class...> class Op, class... Args> struct detector {
  using value_t = std::false_type;
};
template <template <class...> class Op, class... Args>
struct detector<void_t<Op<Args...>>, Op, Args...> {
  using value_t = std::true_type;
};
} // end namespace detail

/// Detects if a given trait holds for some set of arguments 'Args'.
/// For example, the given trait could be used to detect if a given type
/// has a copy assignment operator:
///   template<class T>
///   using has_copy_assign_t = decltype(std::declval<T&>()
///                                                 = std::declval<const T&>());
///   bool fooHasCopyAssign = is_detected<has_copy_assign_t, FooClass>::value;
template <template <class...> class Op, class... Args>
using is_detected = typename detail::detector<void, Op, Args...>::value_t;

namespace detail {
template <typename Callable, typename... Args>
using is_invocable =
    decltype(std::declval<Callable &>()(std::declval<Args>()...));
} // namespace detail

/// Check if a Callable type can be invoked with the given set of arg types.
template <typename Callable, typename... Args>
using is_invocable = is_detected<detail::is_invocable, Callable, Args...>;

/// This class provides various trait information about a callable object.
///   * To access the number of arguments: Traits::num_args
///   * To access the type of an argument: Traits::arg_t<Index>
///   * To access the type of the result:  Traits::result_t
template <typename T, bool isClass = std::is_class<T>::value>
struct function_traits : public function_traits<decltype(&T::operator())> {};

/// Overload for class function types.
template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType (ClassType::*)(Args...) const, false> {
  /// The number of arguments to this function.
  enum { num_args = sizeof...(Args) };

  /// The result type of this function.
  using result_t = ReturnType;

  /// The type of an argument to this function.
  template <size_t Index>
  using arg_t = typename std::tuple_element<Index, std::tuple<Args...>>::type;
};
/// Overload for class function types.
template <typename ClassType, typename ReturnType, typename... Args>
struct function_traits<ReturnType (ClassType::*)(Args...), false>
    : function_traits<ReturnType (ClassType::*)(Args...) const> {};
/// Overload for non-class function types.
template <typename ReturnType, typename... Args>
struct function_traits<ReturnType (*)(Args...), false> {
  /// The number of arguments to this function.
  enum { num_args = sizeof...(Args) };

  /// The result type of this function.
  using result_t = ReturnType;

  /// The type of an argument to this function.
  template <size_t i>
  using arg_t = typename std::tuple_element<i, std::tuple<Args...>>::type;
};
/// Overload for non-class function type references.
template <typename ReturnType, typename... Args>
struct function_traits<ReturnType (&)(Args...), false>
    : public function_traits<ReturnType (*)(Args...)> {};

//===----------------------------------------------------------------------===//
//     Extra additions to <functional>
//===----------------------------------------------------------------------===//

template <class Ty> struct identity {
  using argument_type = Ty;

  Ty &operator()(Ty &self) const {
    return self;
  }
  const Ty &operator()(const Ty &self) const {
    return self;
  }
};

/// An efficient, type-erasing, non-owning reference to a callable. This is
/// intended for use as the type of a function parameter that is not used
/// after the function in question returns.
///
/// This class does not own the callable, so it is not in general safe to store
/// a function_ref.
template<typename Fn> class function_ref;

template<typename Ret, typename ...Params>
class function_ref<Ret(Params...)> {
  Ret (*callback)(intptr_t callable, Params ...params) = nullptr;
  intptr_t callable;

  template<typename Callable>
  static Ret callback_fn(intptr_t callable, Params ...params) {
    return (*reinterpret_cast<Callable*>(callable))(
        std::forward<Params>(params)...);
  }

public:
  function_ref() = default;
  function_ref(std::nullptr_t) {}

  template <typename Callable>
  function_ref(
      Callable &&callable,
      // This is not the copy-constructor.
      std::enable_if_t<!std::is_same<remove_cvref_t<Callable>,
                                     function_ref>::value> * = nullptr,
      // Functor must be callable and return a suitable type.
      std::enable_if_t<std::is_void<Ret>::value ||
                       std::is_convertible<decltype(std::declval<Callable>()(
                                               std::declval<Params>()...)),
                                           Ret>::value> * = nullptr)
      : callback(callback_fn<typename std::remove_reference<Callable>::type>),
        callable(reinterpret_cast<intptr_t>(&callable)) {}

  Ret operator()(Params ...params) const {
    return callback(callable, std::forward<Params>(params)...);
  }

  explicit operator bool() const { return callback; }
};

//===----------------------------------------------------------------------===//
//     Extra additions to <iterator>
//===----------------------------------------------------------------------===//

namespace adl_detail {

using std::begin;

template <typename ContainerTy>
decltype(auto) adl_begin(ContainerTy &&container) {
  return begin(std::forward<ContainerTy>(container));
}

using std::end;

template <typename ContainerTy>
decltype(auto) adl_end(ContainerTy &&container) {
  return end(std::forward<ContainerTy>(container));
}

using std::swap;

template <typename T>
void adl_swap(T &&lhs, T &&rhs) noexcept(noexcept(swap(std::declval<T>(),
                                                       std::declval<T>()))) {
  swap(std::forward<T>(lhs), std::forward<T>(rhs));
}

} // end namespace adl_detail

template <typename ContainerTy>
decltype(auto) adl_begin(ContainerTy &&container) {
  return adl_detail::adl_begin(std::forward<ContainerTy>(container));
}

template <typename ContainerTy>
decltype(auto) adl_end(ContainerTy &&container) {
  return adl_detail::adl_end(std::forward<ContainerTy>(container));
}

template <typename T>
void adl_swap(T &&lhs, T &&rhs) noexcept(
    noexcept(adl_detail::adl_swap(std::declval<T>(), std::declval<T>()))) {
  adl_detail::adl_swap(std::forward<T>(lhs), std::forward<T>(rhs));
}

/// Test whether \p RangeOrContainer is empty. Similar to C++17 std::empty.
template <typename T>
constexpr bool empty(const T &RangeOrContainer) {
  return adl_begin(RangeOrContainer) == adl_end(RangeOrContainer);
}

/// Returns true if the given container only contains a single element.
template <typename ContainerTy> bool hasSingleElement(ContainerTy &&C) {
  auto B = std::begin(C), E = std::end(C);
  return B != E && std::next(B) == E;
}

/// Return a range covering \p RangeOrContainer with the first N elements
/// excluded.
template <typename T> auto drop_begin(T &&RangeOrContainer, size_t N = 1) {
  return make_range(std::next(adl_begin(RangeOrContainer), N),
                    adl_end(RangeOrContainer));
}

// mapped_iterator - This is a simple iterator adapter that causes a function to
// be applied whenever operator* is invoked on the iterator.

template <typename ItTy, typename FuncTy,
          typename FuncReturnTy =
            decltype(std::declval<FuncTy>()(*std::declval<ItTy>()))>
class mapped_iterator
    : public iterator_adaptor_base<
             mapped_iterator<ItTy, FuncTy>, ItTy,
             typename std::iterator_traits<ItTy>::iterator_category,
             typename std::remove_reference<FuncReturnTy>::type> {
public:
  mapped_iterator(ItTy U, FuncTy F)
    : mapped_iterator::iterator_adaptor_base(std::move(U)), F(std::move(F)) {}

  ItTy getCurrent() { return this->I; }

  FuncReturnTy operator*() const { return F(*this->I); }

private:
  FuncTy F;
};

// map_iterator - Provide a convenient way to create mapped_iterators, just like
// make_pair is useful for creating pairs...
template <class ItTy, class FuncTy>
inline mapped_iterator<ItTy, FuncTy> map_iterator(ItTy I, FuncTy F) {
  return mapped_iterator<ItTy, FuncTy>(std::move(I), std::move(F));
}

template <class ContainerTy, class FuncTy>
auto map_range(ContainerTy &&C, FuncTy F) {
  return make_range(map_iterator(C.begin(), F), map_iterator(C.end(), F));
}

/// Helper to determine if type T has a member called rbegin().
template <typename Ty> class has_rbegin_impl {
  using yes = char[1];
  using no = char[2];

  template <typename Inner>
  static yes& test(Inner *I, decltype(I->rbegin()) * = nullptr);

  template <typename>
  static no& test(...);

public:
  static const bool value = sizeof(test<Ty>(nullptr)) == sizeof(yes);
};

/// Metafunction to determine if T& or T has a member called rbegin().
template <typename Ty>
struct has_rbegin : has_rbegin_impl<typename std::remove_reference<Ty>::type> {
};

// Returns an iterator_range over the given container which iterates in reverse.
// Note that the container must have rbegin()/rend() methods for this to work.
template <typename ContainerTy>
auto reverse(ContainerTy &&C,
             std::enable_if_t<has_rbegin<ContainerTy>::value> * = nullptr) {
  return make_range(C.rbegin(), C.rend());
}

// Returns a std::reverse_iterator wrapped around the given iterator.
template <typename IteratorTy>
std::reverse_iterator<IteratorTy> make_reverse_iterator(IteratorTy It) {
  return std::reverse_iterator<IteratorTy>(It);
}

// Returns an iterator_range over the given container which iterates in reverse.
// Note that the container must have begin()/end() methods which return
// bidirectional iterators for this to work.
template <typename ContainerTy>
auto reverse(ContainerTy &&C,
             std::enable_if_t<!has_rbegin<ContainerTy>::value> * = nullptr) {
  return make_range(llvm::make_reverse_iterator(std::end(C)),
                    llvm::make_reverse_iterator(std::begin(C)));
}

/// An iterator adaptor that filters the elements of given inner iterators.
///
/// The predicate parameter should be a callable object that accepts the wrapped
/// iterator's reference type and returns a bool. When incrementing or
/// decrementing the iterator, it will call the predicate on each element and
/// skip any where it returns false.
///
/// \code
///   int A[] = { 1, 2, 3, 4 };
///   auto R = make_filter_range(A, [](int N) { return N % 2 == 1; });
///   // R contains { 1, 3 }.
/// \endcode
///
/// Note: filter_iterator_base implements support for forward iteration.
/// filter_iterator_impl exists to provide support for bidirectional iteration,
/// conditional on whether the wrapped iterator supports it.
template <typename WrappedIteratorT, typename PredicateT, typename IterTag>
class filter_iterator_base
    : public iterator_adaptor_base<
          filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>,
          WrappedIteratorT,
          typename std::common_type<
              IterTag, typename std::iterator_traits<
                           WrappedIteratorT>::iterator_category>::type> {
  using BaseT = iterator_adaptor_base<
      filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>,
      WrappedIteratorT,
      typename std::common_type<
          IterTag, typename std::iterator_traits<
                       WrappedIteratorT>::iterator_category>::type>;

protected:
  WrappedIteratorT End;
  PredicateT Pred;

  void findNextValid() {
    while (this->I != End && !Pred(*this->I))
      BaseT::operator++();
  }

  // Construct the iterator. The begin iterator needs to know where the end
  // is, so that it can properly stop when it gets there. The end iterator only
  // needs the predicate to support bidirectional iteration.
  filter_iterator_base(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : BaseT(Begin), End(End), Pred(Pred) {
    findNextValid();
  }

public:
  using BaseT::operator++;

  filter_iterator_base &operator++() {
    BaseT::operator++();
    findNextValid();
    return *this;
  }
};

/// Specialization of filter_iterator_base for forward iteration only.
template <typename WrappedIteratorT, typename PredicateT,
          typename IterTag = std::forward_iterator_tag>
class filter_iterator_impl
    : public filter_iterator_base<WrappedIteratorT, PredicateT, IterTag> {
  using BaseT = filter_iterator_base<WrappedIteratorT, PredicateT, IterTag>;

public:
  filter_iterator_impl(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : BaseT(Begin, End, Pred) {}
};

/// Specialization of filter_iterator_base for bidirectional iteration.
template <typename WrappedIteratorT, typename PredicateT>
class filter_iterator_impl<WrappedIteratorT, PredicateT,
                           std::bidirectional_iterator_tag>
    : public filter_iterator_base<WrappedIteratorT, PredicateT,
                                  std::bidirectional_iterator_tag> {
  using BaseT = filter_iterator_base<WrappedIteratorT, PredicateT,
                                     std::bidirectional_iterator_tag>;
  void findPrevValid() {
    while (!this->Pred(*this->I))
      BaseT::operator--();
  }

public:
  using BaseT::operator--;

  filter_iterator_impl(WrappedIteratorT Begin, WrappedIteratorT End,
                       PredicateT Pred)
      : BaseT(Begin, End, Pred) {}

  filter_iterator_impl &operator--() {
    BaseT::operator--();
    findPrevValid();
    return *this;
  }
};

namespace detail {

template <bool is_bidirectional> struct fwd_or_bidi_tag_impl {
  using type = std::forward_iterator_tag;
};

template <> struct fwd_or_bidi_tag_impl<true> {
  using type = std::bidirectional_iterator_tag;
};

/// Helper which sets its type member to forward_iterator_tag if the category
/// of \p IterT does not derive from bidirectional_iterator_tag, and to
/// bidirectional_iterator_tag otherwise.
template <typename IterT> struct fwd_or_bidi_tag {
  using type = typename fwd_or_bidi_tag_impl<std::is_base_of<
      std::bidirectional_iterator_tag,
      typename std::iterator_traits<IterT>::iterator_category>::value>::type;
};

} // namespace detail

/// Defines filter_iterator to a suitable specialization of
/// filter_iterator_impl, based on the underlying iterator's category.
template <typename WrappedIteratorT, typename PredicateT>
using filter_iterator = filter_iterator_impl<
    WrappedIteratorT, PredicateT,
    typename detail::fwd_or_bidi_tag<WrappedIteratorT>::type>;

/// Convenience function that takes a range of elements and a predicate,
/// and return a new filter_iterator range.
///
/// FIXME: Currently if RangeT && is a rvalue reference to a temporary, the
/// lifetime of that temporary is not kept by the returned range object, and the
/// temporary is going to be dropped on the floor after the make_iterator_range
/// full expression that contains this function call.
template <typename RangeT, typename PredicateT>
iterator_range<filter_iterator<detail::IterOfRange<RangeT>, PredicateT>>
make_filter_range(RangeT &&Range, PredicateT Pred) {
  using FilterIteratorT =
      filter_iterator<detail::IterOfRange<RangeT>, PredicateT>;
  return make_range(
      FilterIteratorT(std::begin(std::forward<RangeT>(Range)),
                      std::end(std::forward<RangeT>(Range)), Pred),
      FilterIteratorT(std::end(std::forward<RangeT>(Range)),
                      std::end(std::forward<RangeT>(Range)), Pred));
}

/// A pseudo-iterator adaptor that is designed to implement "early increment"
/// style loops.
///
/// This is *not a normal iterator* and should almost never be used directly. It
/// is intended primarily to be used with range based for loops and some range
/// algorithms.
///
/// The iterator isn't quite an `OutputIterator` or an `InputIterator` but
/// somewhere between them. The constraints of these iterators are:
///
/// - On construction or after being incremented, it is comparable and
///   dereferencable. It is *not* incrementable.
/// - After being dereferenced, it is neither comparable nor dereferencable, it
///   is only incrementable.
///
/// This means you can only dereference the iterator once, and you can only
/// increment it once between dereferences.
template <typename WrappedIteratorT>
class early_inc_iterator_impl
    : public iterator_adaptor_base<early_inc_iterator_impl<WrappedIteratorT>,
                                   WrappedIteratorT, std::input_iterator_tag> {
  using BaseT =
      iterator_adaptor_base<early_inc_iterator_impl<WrappedIteratorT>,
                            WrappedIteratorT, std::input_iterator_tag>;

  using PointerT = typename std::iterator_traits<WrappedIteratorT>::pointer;

protected:
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
  bool IsEarlyIncremented = false;
#endif

public:
  early_inc_iterator_impl(WrappedIteratorT I) : BaseT(I) {}

  using BaseT::operator*;
  decltype(*std::declval<WrappedIteratorT>()) operator*() {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(!IsEarlyIncremented && "Cannot dereference twice!");
    IsEarlyIncremented = true;
#endif
    return *(this->I)++;
  }

  using BaseT::operator++;
  early_inc_iterator_impl &operator++() {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(IsEarlyIncremented && "Cannot increment before dereferencing!");
    IsEarlyIncremented = false;
#endif
    return *this;
  }

  friend bool operator==(const early_inc_iterator_impl &LHS,
                         const early_inc_iterator_impl &RHS) {
#if LLVM_ENABLE_ABI_BREAKING_CHECKS
    assert(!LHS.IsEarlyIncremented && "Cannot compare after dereferencing!");
#endif
    return (const BaseT &)LHS == (const BaseT &)RHS;
  }
};

/// Make a range that does early increment to allow mutation of the underlying
/// range without disrupting iteration.
///
/// The underlying iterator will be incremented immediately after it is
/// dereferenced, allowing deletion of the current node or insertion of nodes to
/// not disrupt iteration provided they do not invalidate the *next* iterator --
/// the current iterator can be invalidated.
///
/// This requires a very exact pattern of use that is only really suitable to
/// range based for loops and other range algorithms that explicitly guarantee
/// to dereference exactly once each element, and to increment exactly once each
/// element.
template <typename RangeT>
iterator_range<early_inc_iterator_impl<detail::IterOfRange<RangeT>>>
make_early_inc_range(RangeT &&Range) {
  using EarlyIncIteratorT =
      early_inc_iterator_impl<detail::IterOfRange<RangeT>>;
  return make_range(EarlyIncIteratorT(std::begin(std::forward<RangeT>(Range))),
                    EarlyIncIteratorT(std::end(std::forward<RangeT>(Range))));
}

// forward declarations required by zip_shortest/zip_first/zip_longest
template <typename R, typename UnaryPredicate>
bool all_of(R &&range, UnaryPredicate P);
template <typename R, typename UnaryPredicate>
bool any_of(R &&range, UnaryPredicate P);

namespace detail {

using std::declval;

// We have to alias this since inlining the actual type at the usage site
// in the parameter list of iterator_facade_base<> below ICEs MSVC 2017.
template<typename... Iters> struct ZipTupleType {
  using type = std::tuple<decltype(*declval<Iters>())...>;
};

template <typename ZipType, typename... Iters>
using zip_traits = iterator_facade_base<
    ZipType, typename std::common_type<std::bidirectional_iterator_tag,
                                       typename std::iterator_traits<
                                           Iters>::iterator_category...>::type,
    // ^ TODO: Implement random access methods.
    typename ZipTupleType<Iters...>::type,
    typename std::iterator_traits<typename std::tuple_element<
        0, std::tuple<Iters...>>::type>::difference_type,
    // ^ FIXME: This follows boost::make_zip_iterator's assumption that all
    // inner iterators have the same difference_type. It would fail if, for
    // instance, the second field's difference_type were non-numeric while the
    // first is.
    typename ZipTupleType<Iters...>::type *,
    typename ZipTupleType<Iters...>::type>;

template <typename ZipType, typename... Iters>
struct zip_common : public zip_traits<ZipType, Iters...> {
  using Base = zip_traits<ZipType, Iters...>;
  using value_type = typename Base::value_type;

  std::tuple<Iters...> iterators;

protected:
  template <size_t... Ns> value_type deref(std::index_sequence<Ns...>) const {
    return value_type(*std::get<Ns>(iterators)...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_inc(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(std::next(std::get<Ns>(iterators))...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_dec(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(std::prev(std::get<Ns>(iterators))...);
  }

public:
  zip_common(Iters &&... ts) : iterators(std::forward<Iters>(ts)...) {}

  value_type operator*() { return deref(std::index_sequence_for<Iters...>{}); }

  const value_type operator*() const {
    return deref(std::index_sequence_for<Iters...>{});
  }

  ZipType &operator++() {
    iterators = tup_inc(std::index_sequence_for<Iters...>{});
    return *reinterpret_cast<ZipType *>(this);
  }

  ZipType &operator--() {
    static_assert(Base::IsBidirectional,
                  "All inner iterators must be at least bidirectional.");
    iterators = tup_dec(std::index_sequence_for<Iters...>{});
    return *reinterpret_cast<ZipType *>(this);
  }
};

template <typename... Iters>
struct zip_first : public zip_common<zip_first<Iters...>, Iters...> {
  using Base = zip_common<zip_first<Iters...>, Iters...>;

  bool operator==(const zip_first<Iters...> &other) const {
    return std::get<0>(this->iterators) == std::get<0>(other.iterators);
  }

  zip_first(Iters &&... ts) : Base(std::forward<Iters>(ts)...) {}
};

template <typename... Iters>
class zip_shortest : public zip_common<zip_shortest<Iters...>, Iters...> {
  template <size_t... Ns>
  bool test(const zip_shortest<Iters...> &other,
            std::index_sequence<Ns...>) const {
    return all_of(std::initializer_list<bool>{std::get<Ns>(this->iterators) !=
                                              std::get<Ns>(other.iterators)...},
                  identity<bool>{});
  }

public:
  using Base = zip_common<zip_shortest<Iters...>, Iters...>;

  zip_shortest(Iters &&... ts) : Base(std::forward<Iters>(ts)...) {}

  bool operator==(const zip_shortest<Iters...> &other) const {
    return !test(other, std::index_sequence_for<Iters...>{});
  }
};

template <template <typename...> class ItType, typename... Args> class zippy {
public:
  using iterator = ItType<decltype(std::begin(std::declval<Args>()))...>;
  using iterator_category = typename iterator::iterator_category;
  using value_type = typename iterator::value_type;
  using difference_type = typename iterator::difference_type;
  using pointer = typename iterator::pointer;
  using reference = typename iterator::reference;

private:
  std::tuple<Args...> ts;

  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) const {
    return iterator(std::begin(std::get<Ns>(ts))...);
  }
  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) const {
    return iterator(std::end(std::get<Ns>(ts))...);
  }

public:
  zippy(Args &&... ts_) : ts(std::forward<Args>(ts_)...) {}

  iterator begin() const {
    return begin_impl(std::index_sequence_for<Args...>{});
  }
  iterator end() const { return end_impl(std::index_sequence_for<Args...>{}); }
};

} // end namespace detail

/// zip iterator for two or more iteratable types.
template <typename T, typename U, typename... Args>
detail::zippy<detail::zip_shortest, T, U, Args...> zip(T &&t, U &&u,
                                                       Args &&... args) {
  return detail::zippy<detail::zip_shortest, T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

/// zip iterator that, for the sake of efficiency, assumes the first iteratee to
/// be the shortest.
template <typename T, typename U, typename... Args>
detail::zippy<detail::zip_first, T, U, Args...> zip_first(T &&t, U &&u,
                                                          Args &&... args) {
  return detail::zippy<detail::zip_first, T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

namespace detail {
template <typename Iter>
Iter next_or_end(const Iter &I, const Iter &End) {
  if (I == End)
    return End;
  return std::next(I);
}

template <typename Iter>
auto deref_or_none(const Iter &I, const Iter &End) -> llvm::Optional<
    std::remove_const_t<std::remove_reference_t<decltype(*I)>>> {
  if (I == End)
    return None;
  return *I;
}

template <typename Iter> struct ZipLongestItemType {
  using type =
      llvm::Optional<typename std::remove_const<typename std::remove_reference<
          decltype(*std::declval<Iter>())>::type>::type>;
};

template <typename... Iters> struct ZipLongestTupleType {
  using type = std::tuple<typename ZipLongestItemType<Iters>::type...>;
};

template <typename... Iters>
class zip_longest_iterator
    : public iterator_facade_base<
          zip_longest_iterator<Iters...>,
          typename std::common_type<
              std::forward_iterator_tag,
              typename std::iterator_traits<Iters>::iterator_category...>::type,
          typename ZipLongestTupleType<Iters...>::type,
          typename std::iterator_traits<typename std::tuple_element<
              0, std::tuple<Iters...>>::type>::difference_type,
          typename ZipLongestTupleType<Iters...>::type *,
          typename ZipLongestTupleType<Iters...>::type> {
public:
  using value_type = typename ZipLongestTupleType<Iters...>::type;

private:
  std::tuple<Iters...> iterators;
  std::tuple<Iters...> end_iterators;

  template <size_t... Ns>
  bool test(const zip_longest_iterator<Iters...> &other,
            std::index_sequence<Ns...>) const {
    return llvm::any_of(
        std::initializer_list<bool>{std::get<Ns>(this->iterators) !=
                                    std::get<Ns>(other.iterators)...},
        identity<bool>{});
  }

  template <size_t... Ns> value_type deref(std::index_sequence<Ns...>) const {
    return value_type(
        deref_or_none(std::get<Ns>(iterators), std::get<Ns>(end_iterators))...);
  }

  template <size_t... Ns>
  decltype(iterators) tup_inc(std::index_sequence<Ns...>) const {
    return std::tuple<Iters...>(
        next_or_end(std::get<Ns>(iterators), std::get<Ns>(end_iterators))...);
  }

public:
  zip_longest_iterator(std::pair<Iters &&, Iters &&>... ts)
      : iterators(std::forward<Iters>(ts.first)...),
        end_iterators(std::forward<Iters>(ts.second)...) {}

  value_type operator*() { return deref(std::index_sequence_for<Iters...>{}); }

  value_type operator*() const {
    return deref(std::index_sequence_for<Iters...>{});
  }

  zip_longest_iterator<Iters...> &operator++() {
    iterators = tup_inc(std::index_sequence_for<Iters...>{});
    return *this;
  }

  bool operator==(const zip_longest_iterator<Iters...> &other) const {
    return !test(other, std::index_sequence_for<Iters...>{});
  }
};

template <typename... Args> class zip_longest_range {
public:
  using iterator =
      zip_longest_iterator<decltype(adl_begin(std::declval<Args>()))...>;
  using iterator_category = typename iterator::iterator_category;
  using value_type = typename iterator::value_type;
  using difference_type = typename iterator::difference_type;
  using pointer = typename iterator::pointer;
  using reference = typename iterator::reference;

private:
  std::tuple<Args...> ts;

  template <size_t... Ns>
  iterator begin_impl(std::index_sequence<Ns...>) const {
    return iterator(std::make_pair(adl_begin(std::get<Ns>(ts)),
                                   adl_end(std::get<Ns>(ts)))...);
  }

  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) const {
    return iterator(std::make_pair(adl_end(std::get<Ns>(ts)),
                                   adl_end(std::get<Ns>(ts)))...);
  }

public:
  zip_longest_range(Args &&... ts_) : ts(std::forward<Args>(ts_)...) {}

  iterator begin() const {
    return begin_impl(std::index_sequence_for<Args...>{});
  }
  iterator end() const { return end_impl(std::index_sequence_for<Args...>{}); }
};
} // namespace detail

/// Iterate over two or more iterators at the same time. Iteration continues
/// until all iterators reach the end. The llvm::Optional only contains a value
/// if the iterator has not reached the end.
template <typename T, typename U, typename... Args>
detail::zip_longest_range<T, U, Args...> zip_longest(T &&t, U &&u,
                                                     Args &&... args) {
  return detail::zip_longest_range<T, U, Args...>(
      std::forward<T>(t), std::forward<U>(u), std::forward<Args>(args)...);
}

/// Iterator wrapper that concatenates sequences together.
///
/// This can concatenate different iterators, even with different types, into
/// a single iterator provided the value types of all the concatenated
/// iterators expose `reference` and `pointer` types that can be converted to
/// `ValueT &` and `ValueT *` respectively. It doesn't support more
/// interesting/customized pointer or reference types.
///
/// Currently this only supports forward or higher iterator categories as
/// inputs and always exposes a forward iterator interface.
template <typename ValueT, typename... IterTs>
class concat_iterator
    : public iterator_facade_base<concat_iterator<ValueT, IterTs...>,
                                  std::forward_iterator_tag, ValueT> {
  using BaseT = typename concat_iterator::iterator_facade_base;

  /// We store both the current and end iterators for each concatenated
  /// sequence in a tuple of pairs.
  ///
  /// Note that something like iterator_range seems nice at first here, but the
  /// range properties are of little benefit and end up getting in the way
  /// because we need to do mutation on the current iterators.
  std::tuple<IterTs...> Begins;
  std::tuple<IterTs...> Ends;

  /// Attempts to increment a specific iterator.
  ///
  /// Returns true if it was able to increment the iterator. Returns false if
  /// the iterator is already at the end iterator.
  template <size_t Index> bool incrementHelper() {
    auto &Begin = std::get<Index>(Begins);
    auto &End = std::get<Index>(Ends);
    if (Begin == End)
      return false;

    ++Begin;
    return true;
  }

  /// Increments the first non-end iterator.
  ///
  /// It is an error to call this with all iterators at the end.
  template <size_t... Ns> void increment(std::index_sequence<Ns...>) {
    // Build a sequence of functions to increment each iterator if possible.
    bool (concat_iterator::*IncrementHelperFns[])() = {
        &concat_iterator::incrementHelper<Ns>...};

    // Loop over them, and stop as soon as we succeed at incrementing one.
    for (auto &IncrementHelperFn : IncrementHelperFns)
      if ((this->*IncrementHelperFn)())
        return;

    llvm_unreachable("Attempted to increment an end concat iterator!");
  }

  /// Returns null if the specified iterator is at the end. Otherwise,
  /// dereferences the iterator and returns the address of the resulting
  /// reference.
  template <size_t Index> ValueT *getHelper() const {
    auto &Begin = std::get<Index>(Begins);
    auto &End = std::get<Index>(Ends);
    if (Begin == End)
      return nullptr;

    return &*Begin;
  }

  /// Finds the first non-end iterator, dereferences, and returns the resulting
  /// reference.
  ///
  /// It is an error to call this with all iterators at the end.
  template <size_t... Ns> ValueT &get(std::index_sequence<Ns...>) const {
    // Build a sequence of functions to get from iterator if possible.
    ValueT *(concat_iterator::*GetHelperFns[])() const = {
        &concat_iterator::getHelper<Ns>...};

    // Loop over them, and return the first result we find.
    for (auto &GetHelperFn : GetHelperFns)
      if (ValueT *P = (this->*GetHelperFn)())
        return *P;

    llvm_unreachable("Attempted to get a pointer from an end concat iterator!");
  }

public:
  /// Constructs an iterator from a sequence of ranges.
  ///
  /// We need the full range to know how to switch between each of the
  /// iterators.
  template <typename... RangeTs>
  explicit concat_iterator(RangeTs &&... Ranges)
      : Begins(std::begin(Ranges)...), Ends(std::end(Ranges)...) {}

  using BaseT::operator++;

  concat_iterator &operator++() {
    increment(std::index_sequence_for<IterTs...>());
    return *this;
  }

  ValueT &operator*() const {
    return get(std::index_sequence_for<IterTs...>());
  }

  bool operator==(const concat_iterator &RHS) const {
    return Begins == RHS.Begins && Ends == RHS.Ends;
  }
};

namespace detail {

/// Helper to store a sequence of ranges being concatenated and access them.
///
/// This is designed to facilitate providing actual storage when temporaries
/// are passed into the constructor such that we can use it as part of range
/// based for loops.
template <typename ValueT, typename... RangeTs> class concat_range {
public:
  using iterator =
      concat_iterator<ValueT,
                      decltype(std::begin(std::declval<RangeTs &>()))...>;

private:
  std::tuple<RangeTs...> Ranges;

  template <size_t... Ns> iterator begin_impl(std::index_sequence<Ns...>) {
    return iterator(std::get<Ns>(Ranges)...);
  }
  template <size_t... Ns> iterator end_impl(std::index_sequence<Ns...>) {
    return iterator(make_range(std::end(std::get<Ns>(Ranges)),
                               std::end(std::get<Ns>(Ranges)))...);
  }

public:
  concat_range(RangeTs &&... Ranges)
      : Ranges(std::forward<RangeTs>(Ranges)...) {}

  iterator begin() { return begin_impl(std::index_sequence_for<RangeTs...>{}); }
  iterator end() { return end_impl(std::index_sequence_for<RangeTs...>{}); }
};

} // end namespace detail

/// Concatenated range across two or more ranges.
///
/// The desired value type must be explicitly specified.
template <typename ValueT, typename... RangeTs>
detail::concat_range<ValueT, RangeTs...> concat(RangeTs &&... Ranges) {
  static_assert(sizeof...(RangeTs) > 1,
                "Need more than one range to concatenate!");
  return detail::concat_range<ValueT, RangeTs...>(
      std::forward<RangeTs>(Ranges)...);
}

/// A utility class used to implement an iterator that contains some base object
/// and an index. The iterator moves the index but keeps the base constant.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_iterator
    : public llvm::iterator_facade_base<DerivedT,
                                        std::random_access_iterator_tag, T,
                                        std::ptrdiff_t, PointerT, ReferenceT> {
public:
  ptrdiff_t operator-(const indexed_accessor_iterator &rhs) const {
    assert(base == rhs.base && "incompatible iterators");
    return index - rhs.index;
  }
  bool operator==(const indexed_accessor_iterator &rhs) const {
    return base == rhs.base && index == rhs.index;
  }
  bool operator<(const indexed_accessor_iterator &rhs) const {
    assert(base == rhs.base && "incompatible iterators");
    return index < rhs.index;
  }

  DerivedT &operator+=(ptrdiff_t offset) {
    this->index += offset;
    return static_cast<DerivedT &>(*this);
  }
  DerivedT &operator-=(ptrdiff_t offset) {
    this->index -= offset;
    return static_cast<DerivedT &>(*this);
  }

  /// Returns the current index of the iterator.
  ptrdiff_t getIndex() const { return index; }

  /// Returns the current base of the iterator.
  const BaseT &getBase() const { return base; }

protected:
  indexed_accessor_iterator(BaseT base, ptrdiff_t index)
      : base(base), index(index) {}
  BaseT base;
  ptrdiff_t index;
};

namespace detail {
/// The class represents the base of a range of indexed_accessor_iterators. It
/// provides support for many different range functionalities, e.g.
/// drop_front/slice/etc.. Derived range classes must implement the following
/// static methods:
///   * ReferenceT dereference_iterator(const BaseT &base, ptrdiff_t index)
///     - Dereference an iterator pointing to the base object at the given
///       index.
///   * BaseT offset_base(const BaseT &base, ptrdiff_t index)
///     - Return a new base that is offset from the provide base by 'index'
///       elements.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_range_base {
public:
  using RangeBaseT =
      indexed_accessor_range_base<DerivedT, BaseT, T, PointerT, ReferenceT>;

  /// An iterator element of this range.
  class iterator : public indexed_accessor_iterator<iterator, BaseT, T,
                                                    PointerT, ReferenceT> {
  public:
    // Index into this iterator, invoking a static method on the derived type.
    ReferenceT operator*() const {
      return DerivedT::dereference_iterator(this->getBase(), this->getIndex());
    }

  private:
    iterator(BaseT owner, ptrdiff_t curIndex)
        : indexed_accessor_iterator<iterator, BaseT, T, PointerT, ReferenceT>(
              owner, curIndex) {}

    /// Allow access to the constructor.
    friend indexed_accessor_range_base<DerivedT, BaseT, T, PointerT,
                                       ReferenceT>;
  };

  indexed_accessor_range_base(iterator begin, iterator end)
      : base(offset_base(begin.getBase(), begin.getIndex())),
        count(end.getIndex() - begin.getIndex()) {}
  indexed_accessor_range_base(const iterator_range<iterator> &range)
      : indexed_accessor_range_base(range.begin(), range.end()) {}
  indexed_accessor_range_base(BaseT base, ptrdiff_t count)
      : base(base), count(count) {}

  iterator begin() const { return iterator(base, 0); }
  iterator end() const { return iterator(base, count); }
  ReferenceT operator[](size_t Index) const {
    assert(Index < size() && "invalid index for value range");
    return DerivedT::dereference_iterator(base, static_cast<ptrdiff_t>(Index));
  }
  ReferenceT front() const {
    assert(!empty() && "expected non-empty range");
    return (*this)[0];
  }
  ReferenceT back() const {
    assert(!empty() && "expected non-empty range");
    return (*this)[size() - 1];
  }

  /// Compare this range with another.
  template <typename OtherT> bool operator==(const OtherT &other) const {
    return size() ==
               static_cast<size_t>(std::distance(other.begin(), other.end())) &&
           std::equal(begin(), end(), other.begin());
  }
  template <typename OtherT> bool operator!=(const OtherT &other) const {
    return !(*this == other);
  }

  /// Return the size of this range.
  size_t size() const { return count; }

  /// Return if the range is empty.
  bool empty() const { return size() == 0; }

  /// Drop the first N elements, and keep M elements.
  DerivedT slice(size_t n, size_t m) const {
    assert(n + m <= size() && "invalid size specifiers");
    return DerivedT(offset_base(base, n), m);
  }

  /// Drop the first n elements.
  DerivedT drop_front(size_t n = 1) const {
    assert(size() >= n && "Dropping more elements than exist");
    return slice(n, size() - n);
  }
  /// Drop the last n elements.
  DerivedT drop_back(size_t n = 1) const {
    assert(size() >= n && "Dropping more elements than exist");
    return DerivedT(base, size() - n);
  }

  /// Take the first n elements.
  DerivedT take_front(size_t n = 1) const {
    return n < size() ? drop_back(size() - n)
                      : static_cast<const DerivedT &>(*this);
  }

  /// Take the last n elements.
  DerivedT take_back(size_t n = 1) const {
    return n < size() ? drop_front(size() - n)
                      : static_cast<const DerivedT &>(*this);
  }

  /// Allow conversion to any type accepting an iterator_range.
  template <typename RangeT, typename = std::enable_if_t<std::is_constructible<
                                 RangeT, iterator_range<iterator>>::value>>
  operator RangeT() const {
    return RangeT(iterator_range<iterator>(*this));
  }

  /// Returns the base of this range.
  const BaseT &getBase() const { return base; }

private:
  /// Offset the given base by the given amount.
  static BaseT offset_base(const BaseT &base, size_t n) {
    return n == 0 ? base : DerivedT::offset_base(base, n);
  }

protected:
  indexed_accessor_range_base(const indexed_accessor_range_base &) = default;
  indexed_accessor_range_base(indexed_accessor_range_base &&) = default;
  indexed_accessor_range_base &
  operator=(const indexed_accessor_range_base &) = default;

  /// The base that owns the provided range of values.
  BaseT base;
  /// The size from the owning range.
  ptrdiff_t count;
};
} // end namespace detail

/// This class provides an implementation of a range of
/// indexed_accessor_iterators where the base is not indexable. Ranges with
/// bases that are offsetable should derive from indexed_accessor_range_base
/// instead. Derived range classes are expected to implement the following
/// static method:
///   * ReferenceT dereference(const BaseT &base, ptrdiff_t index)
///     - Dereference an iterator pointing to a parent base at the given index.
template <typename DerivedT, typename BaseT, typename T,
          typename PointerT = T *, typename ReferenceT = T &>
class indexed_accessor_range
    : public detail::indexed_accessor_range_base<
          DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT, ReferenceT> {
public:
  indexed_accessor_range(BaseT base, ptrdiff_t startIndex, ptrdiff_t count)
      : detail::indexed_accessor_range_base<
            DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT, ReferenceT>(
            std::make_pair(base, startIndex), count) {}
  using detail::indexed_accessor_range_base<
      DerivedT, std::pair<BaseT, ptrdiff_t>, T, PointerT,
      ReferenceT>::indexed_accessor_range_base;

  /// Returns the current base of the range.
  const BaseT &getBase() const { return this->base.first; }

  /// Returns the current start index of the range.
  ptrdiff_t getStartIndex() const { return this->base.second; }

  /// See `detail::indexed_accessor_range_base` for details.
  static std::pair<BaseT, ptrdiff_t>
  offset_base(const std::pair<BaseT, ptrdiff_t> &base, ptrdiff_t index) {
    // We encode the internal base as a pair of the derived base and a start
    // index into the derived base.
    return std::make_pair(base.first, base.second + index);
  }
  /// See `detail::indexed_accessor_range_base` for details.
  static ReferenceT
  dereference_iterator(const std::pair<BaseT, ptrdiff_t> &base,
                       ptrdiff_t index) {
    return DerivedT::dereference(base.first, base.second + index);
  }
};

/// Given a container of pairs, return a range over the first elements.
template <typename ContainerTy> auto make_first_range(ContainerTy &&c) {
  return llvm::map_range(
      std::forward<ContainerTy>(c),
      [](decltype((*std::begin(c))) elt) -> decltype((elt.first)) {
        return elt.first;
      });
}

/// Given a container of pairs, return a range over the second elements.
template <typename ContainerTy> auto make_second_range(ContainerTy &&c) {
  return llvm::map_range(
      std::forward<ContainerTy>(c),
      [](decltype((*std::begin(c))) elt) -> decltype((elt.second)) {
        return elt.second;
      });
}

//===----------------------------------------------------------------------===//
//     Extra additions to <utility>
//===----------------------------------------------------------------------===//

/// Function object to check whether the first component of a std::pair
/// compares less than the first component of another std::pair.
struct less_first {
  template <typename T> bool operator()(const T &lhs, const T &rhs) const {
    return lhs.first < rhs.first;
  }
};

/// Function object to check whether the second component of a std::pair
/// compares less than the second component of another std::pair.
struct less_second {
  template <typename T> bool operator()(const T &lhs, const T &rhs) const {
    return lhs.second < rhs.second;
  }
};

/// \brief Function object to apply a binary function to the first component of
/// a std::pair.
template<typename FuncTy>
struct on_first {
  FuncTy func;

  template <typename T>
  decltype(auto) operator()(const T &lhs, const T &rhs) const {
    return func(lhs.first, rhs.first);
  }
};

/// Utility type to build an inheritance chain that makes it easy to rank
/// overload candidates.
template <int N> struct rank : rank<N - 1> {};
template <> struct rank<0> {};

/// traits class for checking whether type T is one of any of the given
/// types in the variadic list.
template <typename T, typename... Ts>
using is_one_of = disjunction<std::is_same<T, Ts>...>;

/// traits class for checking whether type T is a base class for all
///  the given types in the variadic list.
template <typename T, typename... Ts>
using are_base_of = conjunction<std::is_base_of<T, Ts>...>;

namespace detail {
template <typename... Ts> struct Visitor;

template <typename HeadT, typename... TailTs>
struct Visitor<HeadT, TailTs...> : remove_cvref_t<HeadT>, Visitor<TailTs...> {
  explicit constexpr Visitor(HeadT &&Head, TailTs &&...Tail)
      : remove_cvref_t<HeadT>(std::forward<HeadT>(Head)),
        Visitor<TailTs...>(std::forward<TailTs>(Tail)...) {}
  using remove_cvref_t<HeadT>::operator();
  using Visitor<TailTs...>::operator();
};

template <typename HeadT> struct Visitor<HeadT> : remove_cvref_t<HeadT> {
  explicit constexpr Visitor(HeadT &&Head)
      : remove_cvref_t<HeadT>(std::forward<HeadT>(Head)) {}
  using remove_cvref_t<HeadT>::operator();
};
} // namespace detail

/// Returns an opaquely-typed Callable object whose operator() overload set is
/// the sum of the operator() overload sets of each CallableT in CallableTs.
///
/// The type of the returned object derives from each CallableT in CallableTs.
/// The returned object is constructed by invoking the appropriate copy or move
/// constructor of each CallableT, as selected by overload resolution on the
/// corresponding argument to makeVisitor.
///
/// Example:
///
/// \code
/// auto visitor = makeVisitor([](auto) { return "unhandled type"; },
///                            [](int i) { return "int"; },
///                            [](std::string s) { return "str"; });
/// auto a = visitor(42);    // `a` is now "int".
/// auto b = visitor("foo"); // `b` is now "str".
/// auto c = visitor(3.14f); // `c` is now "unhandled type".
/// \endcode
///
/// Example of making a visitor with a lambda which captures a move-only type:
///
/// \code
/// std::unique_ptr<FooHandler> FH = /* ... */;
/// auto visitor = makeVisitor(
///     [FH{std::move(FH)}](Foo F) { return FH->handle(F); },
///     [](int i) { return i; },
///     [](std::string s) { return atoi(s); });
/// \endcode
template <typename... CallableTs>
constexpr decltype(auto) makeVisitor(CallableTs &&...Callables) {
  return detail::Visitor<CallableTs...>(std::forward<CallableTs>(Callables)...);
}

//===----------------------------------------------------------------------===//
//     Extra additions for arrays
//===----------------------------------------------------------------------===//

// We have a copy here so that LLVM behaves the same when using different
// standard libraries.
template <class Iterator, class RNG>
void shuffle(Iterator first, Iterator last, RNG &&g) {
  // It would be better to use a std::uniform_int_distribution,
  // but that would be stdlib dependent.
  typedef
      typename std::iterator_traits<Iterator>::difference_type difference_type;
  for (auto size = last - first; size > 1; ++first, (void)--size) {
    difference_type offset = g() % size;
    // Avoid self-assignment due to incorrect assertions in libstdc++
    // containers (https://gcc.gnu.org/bugzilla/show_bug.cgi?id=85828).
    if (offset != difference_type(0))
      std::iter_swap(first, first + offset);
  }
}

/// Find the length of an array.
template <class T, std::size_t N>
constexpr inline size_t array_lengthof(T (&)[N]) {
  return N;
}

/// Adapt std::less<T> for array_pod_sort.
template<typename T>
inline int array_pod_sort_comparator(const void *P1, const void *P2) {
  if (std::less<T>()(*reinterpret_cast<const T*>(P1),
                     *reinterpret_cast<const T*>(P2)))
    return -1;
  if (std::less<T>()(*reinterpret_cast<const T*>(P2),
                     *reinterpret_cast<const T*>(P1)))
    return 1;
  return 0;
}

/// get_array_pod_sort_comparator - This is an internal helper function used to
/// get type deduction of T right.
template<typename T>
inline int (*get_array_pod_sort_comparator(const T &))
             (const void*, const void*) {
  return array_pod_sort_comparator<T>;
}

#ifdef EXPENSIVE_CHECKS
namespace detail {

inline unsigned presortShuffleEntropy() {
  static unsigned Result(std::random_device{}());
  return Result;
}

template <class IteratorTy>
inline void presortShuffle(IteratorTy Start, IteratorTy End) {
  std::mt19937 Generator(presortShuffleEntropy());
  llvm::shuffle(Start, End, Generator);
}

} // end namespace detail
#endif

/// array_pod_sort - This sorts an array with the specified start and end
/// extent.  This is just like std::sort, except that it calls qsort instead of
/// using an inlined template.  qsort is slightly slower than std::sort, but
/// most sorts are not performance critical in LLVM and std::sort has to be
/// template instantiated for each type, leading to significant measured code
/// bloat.  This function should generally be used instead of std::sort where
/// possible.
///
/// This function assumes that you have simple POD-like types that can be
/// compared with std::less and can be moved with memcpy.  If this isn't true,
/// you should use std::sort.
///
/// NOTE: If qsort_r were portable, we could allow a custom comparator and
/// default to std::less.
template<class IteratorTy>
inline void array_pod_sort(IteratorTy Start, IteratorTy End) {
  // Don't inefficiently call qsort with one element or trigger undefined
  // behavior with an empty sequence.
  auto NElts = End - Start;
  if (NElts <= 1) return;
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  qsort(&*Start, NElts, sizeof(*Start), get_array_pod_sort_comparator(*Start));
}

template <class IteratorTy>
inline void array_pod_sort(
    IteratorTy Start, IteratorTy End,
    int (*Compare)(
        const typename std::iterator_traits<IteratorTy>::value_type *,
        const typename std::iterator_traits<IteratorTy>::value_type *)) {
  // Don't inefficiently call qsort with one element or trigger undefined
  // behavior with an empty sequence.
  auto NElts = End - Start;
  if (NElts <= 1) return;
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  qsort(&*Start, NElts, sizeof(*Start),
        reinterpret_cast<int (*)(const void *, const void *)>(Compare));
}

namespace detail {
template <typename T>
// We can use qsort if the iterator type is a pointer and the underlying value
// is trivially copyable.
using sort_trivially_copyable = conjunction<
    std::is_pointer<T>,
    std::is_trivially_copyable<typename std::iterator_traits<T>::value_type>>;
} // namespace detail

// Provide wrappers to std::sort which shuffle the elements before sorting
// to help uncover non-deterministic behavior (PR35135).
template <typename IteratorTy,
          std::enable_if_t<!detail::sort_trivially_copyable<IteratorTy>::value,
                           int> = 0>
inline void sort(IteratorTy Start, IteratorTy End) {
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  std::sort(Start, End);
}

// Forward trivially copyable types to array_pod_sort. This avoids a large
// amount of code bloat for a minor performance hit.
template <typename IteratorTy,
          std::enable_if_t<detail::sort_trivially_copyable<IteratorTy>::value,
                           int> = 0>
inline void sort(IteratorTy Start, IteratorTy End) {
  array_pod_sort(Start, End);
}

template <typename Container> inline void sort(Container &&C) {
  llvm::sort(adl_begin(C), adl_end(C));
}

template <typename IteratorTy, typename Compare>
inline void sort(IteratorTy Start, IteratorTy End, Compare Comp) {
#ifdef EXPENSIVE_CHECKS
  detail::presortShuffle<IteratorTy>(Start, End);
#endif
  std::sort(Start, End, Comp);
}

template <typename Container, typename Compare>
inline void sort(Container &&C, Compare Comp) {
  llvm::sort(adl_begin(C), adl_end(C), Comp);
}

//===----------------------------------------------------------------------===//
//     Extra additions to <algorithm>
//===----------------------------------------------------------------------===//

/// Get the size of a range. This is a wrapper function around std::distance
/// which is only enabled when the operation is O(1).
template <typename R>
auto size(R &&Range,
          std::enable_if_t<
              std::is_base_of<std::random_access_iterator_tag,
                              typename std::iterator_traits<decltype(
                                  Range.begin())>::iterator_category>::value,
              void> * = nullptr) {
  return std::distance(Range.begin(), Range.end());
}

/// Provide wrappers to std::for_each which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryFunction>
UnaryFunction for_each(R &&Range, UnaryFunction F) {
  return std::for_each(adl_begin(Range), adl_end(Range), F);
}

/// Provide wrappers to std::all_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool all_of(R &&Range, UnaryPredicate P) {
  return std::all_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::any_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool any_of(R &&Range, UnaryPredicate P) {
  return std::any_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::none_of which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
bool none_of(R &&Range, UnaryPredicate P) {
  return std::none_of(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::find which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename T> auto find(R &&Range, const T &Val) {
  return std::find(adl_begin(Range), adl_end(Range), Val);
}

/// Provide wrappers to std::find_if which take ranges instead of having to pass
/// begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto find_if(R &&Range, UnaryPredicate P) {
  return std::find_if(adl_begin(Range), adl_end(Range), P);
}

template <typename R, typename UnaryPredicate>
auto find_if_not(R &&Range, UnaryPredicate P) {
  return std::find_if_not(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::remove_if which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto remove_if(R &&Range, UnaryPredicate P) {
  return std::remove_if(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::copy_if which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename OutputIt, typename UnaryPredicate>
OutputIt copy_if(R &&Range, OutputIt Out, UnaryPredicate P) {
  return std::copy_if(adl_begin(Range), adl_end(Range), Out, P);
}

template <typename R, typename OutputIt>
OutputIt copy(R &&Range, OutputIt Out) {
  return std::copy(adl_begin(Range), adl_end(Range), Out);
}

/// Provide wrappers to std::move which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename OutputIt>
OutputIt move(R &&Range, OutputIt Out) {
  return std::move(adl_begin(Range), adl_end(Range), Out);
}

/// Wrapper function around std::find to detect if an element exists
/// in a container.
template <typename R, typename E>
bool is_contained(R &&Range, const E &Element) {
  return std::find(adl_begin(Range), adl_end(Range), Element) != adl_end(Range);
}

/// Wrapper function around std::is_sorted to check if elements in a range \p R
/// are sorted with respect to a comparator \p C.
template <typename R, typename Compare> bool is_sorted(R &&Range, Compare C) {
  return std::is_sorted(adl_begin(Range), adl_end(Range), C);
}

/// Wrapper function around std::is_sorted to check if elements in a range \p R
/// are sorted in non-descending order.
template <typename R> bool is_sorted(R &&Range) {
  return std::is_sorted(adl_begin(Range), adl_end(Range));
}

/// Wrapper function around std::count to count the number of times an element
/// \p Element occurs in the given range \p Range.
template <typename R, typename E> auto count(R &&Range, const E &Element) {
  return std::count(adl_begin(Range), adl_end(Range), Element);
}

/// Wrapper function around std::count_if to count the number of times an
/// element satisfying a given predicate occurs in a range.
template <typename R, typename UnaryPredicate>
auto count_if(R &&Range, UnaryPredicate P) {
  return std::count_if(adl_begin(Range), adl_end(Range), P);
}

/// Wrapper function around std::transform to apply a function to a range and
/// store the result elsewhere.
template <typename R, typename OutputIt, typename UnaryFunction>
OutputIt transform(R &&Range, OutputIt d_first, UnaryFunction F) {
  return std::transform(adl_begin(Range), adl_end(Range), d_first, F);
}

/// Provide wrappers to std::partition which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename UnaryPredicate>
auto partition(R &&Range, UnaryPredicate P) {
  return std::partition(adl_begin(Range), adl_end(Range), P);
}

/// Provide wrappers to std::lower_bound which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename T> auto lower_bound(R &&Range, T &&Value) {
  return std::lower_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value));
}

template <typename R, typename T, typename Compare>
auto lower_bound(R &&Range, T &&Value, Compare C) {
  return std::lower_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value), C);
}

/// Provide wrappers to std::upper_bound which take ranges instead of having to
/// pass begin/end explicitly.
template <typename R, typename T> auto upper_bound(R &&Range, T &&Value) {
  return std::upper_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value));
}

template <typename R, typename T, typename Compare>
auto upper_bound(R &&Range, T &&Value, Compare C) {
  return std::upper_bound(adl_begin(Range), adl_end(Range),
                          std::forward<T>(Value), C);
}

template <typename R>
void stable_sort(R &&Range) {
  std::stable_sort(adl_begin(Range), adl_end(Range));
}

template <typename R, typename Compare>
void stable_sort(R &&Range, Compare C) {
  std::stable_sort(adl_begin(Range), adl_end(Range), C);
}

/// Binary search for the first iterator in a range where a predicate is false.
/// Requires that C is always true below some limit, and always false above it.
template <typename R, typename Predicate,
          typename Val = decltype(*adl_begin(std::declval<R>()))>
auto partition_point(R &&Range, Predicate P) {
  return std::partition_point(adl_begin(Range), adl_end(Range), P);
}

template<typename Range, typename Predicate>
auto unique(Range &&R, Predicate P) {
  return std::unique(adl_begin(R), adl_end(R), P);
}

/// Wrapper function around std::equal to detect if all elements
/// in a container are same.
template <typename R>
bool is_splat(R &&Range) {
  size_t range_size = size(Range);
  return range_size != 0 && (range_size == 1 ||
         std::equal(adl_begin(Range) + 1, adl_end(Range), adl_begin(Range)));
}

/// Provide a container algorithm similar to C++ Library Fundamentals v2's
/// `erase_if` which is equivalent to:
///
///   C.erase(remove_if(C, pred), C.end());
///
/// This version works for any container with an erase method call accepting
/// two iterators.
template <typename Container, typename UnaryPredicate>
void erase_if(Container &C, UnaryPredicate P) {
  C.erase(remove_if(C, P), C.end());
}

/// Wrapper function to remove a value from a container:
///
/// C.erase(remove(C.begin(), C.end(), V), C.end());
template <typename Container, typename ValueType>
void erase_value(Container &C, ValueType V) {
  C.erase(std::remove(C.begin(), C.end(), V), C.end());
}

/// Wrapper function to append a range to a container.
///
/// C.insert(C.end(), R.begin(), R.end());
template <typename Container, typename Range>
inline void append_range(Container &C, Range &&R) {
  C.insert(C.end(), R.begin(), R.end());
}

/// Given a sequence container Cont, replace the range [ContIt, ContEnd) with
/// the range [ValIt, ValEnd) (which is not from the same container).
template<typename Container, typename RandomAccessIterator>
void replace(Container &Cont, typename Container::iterator ContIt,
             typename Container::iterator ContEnd, RandomAccessIterator ValIt,
             RandomAccessIterator ValEnd) {
  while (true) {
    if (ValIt == ValEnd) {
      Cont.erase(ContIt, ContEnd);
      return;
    } else if (ContIt == ContEnd) {
      Cont.insert(ContIt, ValIt, ValEnd);
      return;
    }
    *ContIt++ = *ValIt++;
  }
}

/// Given a sequence container Cont, replace the range [ContIt, ContEnd) with
/// the range R.
template<typename Container, typename Range = std::initializer_list<
                                 typename Container::value_type>>
void replace(Container &Cont, typename Container::iterator ContIt,
             typename Container::iterator ContEnd, Range R) {
  replace(Cont, ContIt, ContEnd, R.begin(), R.end());
}

/// An STL-style algorithm similar to std::for_each that applies a second
/// functor between every pair of elements.
///
/// This provides the control flow logic to, for example, print a
/// comma-separated list:
/// \code
///   interleave(names.begin(), names.end(),
///              [&](StringRef name) { os << name; },
///              [&] { os << ", "; });
/// \endcode
template <typename ForwardIterator, typename UnaryFunctor,
          typename NullaryFunctor,
          typename = typename std::enable_if<
              !std::is_constructible<StringRef, UnaryFunctor>::value &&
              !std::is_constructible<StringRef, NullaryFunctor>::value>::type>
inline void interleave(ForwardIterator begin, ForwardIterator end,
                       UnaryFunctor each_fn, NullaryFunctor between_fn) {
  if (begin == end)
    return;
  each_fn(*begin);
  ++begin;
  for (; begin != end; ++begin) {
    between_fn();
    each_fn(*begin);
  }
}

template <typename Container, typename UnaryFunctor, typename NullaryFunctor,
          typename = typename std::enable_if<
              !std::is_constructible<StringRef, UnaryFunctor>::value &&
              !std::is_constructible<StringRef, NullaryFunctor>::value>::type>
inline void interleave(const Container &c, UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  interleave(c.begin(), c.end(), each_fn, between_fn);
}

/// Overload of interleave for the common case of string separator.
template <typename Container, typename UnaryFunctor, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleave(const Container &c, StreamT &os, UnaryFunctor each_fn,
                       const StringRef &separator) {
  interleave(c.begin(), c.end(), each_fn, [&] { os << separator; });
}
template <typename Container, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleave(const Container &c, StreamT &os,
                       const StringRef &separator) {
  interleave(
      c, os, [&](const T &a) { os << a; }, separator);
}

template <typename Container, typename UnaryFunctor, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleaveComma(const Container &c, StreamT &os,
                            UnaryFunctor each_fn) {
  interleave(c, os, each_fn, ", ");
}
template <typename Container, typename StreamT,
          typename T = detail::ValueOfRange<Container>>
inline void interleaveComma(const Container &c, StreamT &os) {
  interleaveComma(c, os, [&](const T &a) { os << a; });
}

//===----------------------------------------------------------------------===//
//     Extra additions to <memory>
//===----------------------------------------------------------------------===//

struct FreeDeleter {
  void operator()(void* v) {
    ::free(v);
  }
};

template<typename First, typename Second>
struct pair_hash {
  size_t operator()(const std::pair<First, Second> &P) const {
    return std::hash<First>()(P.first) * 31 + std::hash<Second>()(P.second);
  }
};

/// Binary functor that adapts to any other binary functor after dereferencing
/// operands.
template <typename T> struct deref {
  T func;

  // Could be further improved to cope with non-derivable functors and
  // non-binary functors (should be a variadic template member function
  // operator()).
  template <typename A, typename B> auto operator()(A &lhs, B &rhs) const {
    assert(lhs);
    assert(rhs);
    return func(*lhs, *rhs);
  }
};

namespace detail {

template <typename R> class enumerator_iter;

template <typename R> struct result_pair {
  using value_reference =
      typename std::iterator_traits<IterOfRange<R>>::reference;

  friend class enumerator_iter<R>;

  result_pair() = default;
  result_pair(std::size_t Index, IterOfRange<R> Iter)
      : Index(Index), Iter(Iter) {}

  result_pair(const result_pair<R> &Other)
      : Index(Other.Index), Iter(Other.Iter) {}
  result_pair &operator=(const result_pair &Other) {
    Index = Other.Index;
    Iter = Other.Iter;
    return *this;
  }

  std::size_t index() const { return Index; }
  const value_reference value() const { return *Iter; }
  value_reference value() { return *Iter; }

private:
  std::size_t Index = std::numeric_limits<std::size_t>::max();
  IterOfRange<R> Iter;
};

template <typename R>
class enumerator_iter
    : public iterator_facade_base<
          enumerator_iter<R>, std::forward_iterator_tag, result_pair<R>,
          typename std::iterator_traits<IterOfRange<R>>::difference_type,
          typename std::iterator_traits<IterOfRange<R>>::pointer,
          typename std::iterator_traits<IterOfRange<R>>::reference> {
  using result_type = result_pair<R>;

public:
  explicit enumerator_iter(IterOfRange<R> EndIter)
      : Result(std::numeric_limits<size_t>::max(), EndIter) {}

  enumerator_iter(std::size_t Index, IterOfRange<R> Iter)
      : Result(Index, Iter) {}

  result_type &operator*() { return Result; }
  const result_type &operator*() const { return Result; }

  enumerator_iter &operator++() {
    assert(Result.Index != std::numeric_limits<size_t>::max());
    ++Result.Iter;
    ++Result.Index;
    return *this;
  }

  bool operator==(const enumerator_iter &RHS) const {
    // Don't compare indices here, only iterators.  It's possible for an end
    // iterator to have different indices depending on whether it was created
    // by calling std::end() versus incrementing a valid iterator.
    return Result.Iter == RHS.Result.Iter;
  }

  enumerator_iter(const enumerator_iter &Other) : Result(Other.Result) {}
  enumerator_iter &operator=(const enumerator_iter &Other) {
    Result = Other.Result;
    return *this;
  }

private:
  result_type Result;
};

template <typename R> class enumerator {
public:
  explicit enumerator(R &&Range) : TheRange(std::forward<R>(Range)) {}

  enumerator_iter<R> begin() {
    return enumerator_iter<R>(0, std::begin(TheRange));
  }

  enumerator_iter<R> end() {
    return enumerator_iter<R>(std::end(TheRange));
  }

private:
  R TheRange;
};

} // end namespace detail

/// Given an input range, returns a new range whose values are are pair (A,B)
/// such that A is the 0-based index of the item in the sequence, and B is
/// the value from the original sequence.  Example:
///
/// std::vector<char> Items = {'A', 'B', 'C', 'D'};
/// for (auto X : enumerate(Items)) {
///   printf("Item %d - %c\n", X.index(), X.value());
/// }
///
/// Output:
///   Item 0 - A
///   Item 1 - B
///   Item 2 - C
///   Item 3 - D
///
template <typename R> detail::enumerator<R> enumerate(R &&TheRange) {
  return detail::enumerator<R>(std::forward<R>(TheRange));
}

namespace detail {

template <typename F, typename Tuple, std::size_t... I>
decltype(auto) apply_tuple_impl(F &&f, Tuple &&t, std::index_sequence<I...>) {
  return std::forward<F>(f)(std::get<I>(std::forward<Tuple>(t))...);
}

} // end namespace detail

/// Given an input tuple (a1, a2, ..., an), pass the arguments of the
/// tuple variadically to f as if by calling f(a1, a2, ..., an) and
/// return the result.
template <typename F, typename Tuple>
decltype(auto) apply_tuple(F &&f, Tuple &&t) {
  using Indices = std::make_index_sequence<
      std::tuple_size<typename std::decay<Tuple>::type>::value>;

  return detail::apply_tuple_impl(std::forward<F>(f), std::forward<Tuple>(t),
                                  Indices{});
}

/// Return true if the sequence [Begin, End) has exactly N items. Runs in O(N)
/// time. Not meant for use with random-access iterators.
/// Can optionally take a predicate to filter lazily some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItems(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted =
        [](const decltype(*std::declval<IterTy>()) &) { return true; },
    std::enable_if_t<
        !std::is_base_of<std::random_access_iterator_tag,
                         typename std::iterator_traits<std::remove_reference_t<
                             decltype(Begin)>>::iterator_category>::value,
        void> * = nullptr) {
  for (; N; ++Begin) {
    if (Begin == End)
      return false; // Too few.
    N -= ShouldBeCounted(*Begin);
  }
  for (; Begin != End; ++Begin)
    if (ShouldBeCounted(*Begin))
      return false; // Too many.
  return true;
}

/// Return true if the sequence [Begin, End) has N or more items. Runs in O(N)
/// time. Not meant for use with random-access iterators.
/// Can optionally take a predicate to lazily filter some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItemsOrMore(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted =
        [](const decltype(*std::declval<IterTy>()) &) { return true; },
    std::enable_if_t<
        !std::is_base_of<std::random_access_iterator_tag,
                         typename std::iterator_traits<std::remove_reference_t<
                             decltype(Begin)>>::iterator_category>::value,
        void> * = nullptr) {
  for (; N; ++Begin) {
    if (Begin == End)
      return false; // Too few.
    N -= ShouldBeCounted(*Begin);
  }
  return true;
}

/// Returns true if the sequence [Begin, End) has N or less items. Can
/// optionally take a predicate to lazily filter some items.
template <typename IterTy,
          typename Pred = bool (*)(const decltype(*std::declval<IterTy>()) &)>
bool hasNItemsOrLess(
    IterTy &&Begin, IterTy &&End, unsigned N,
    Pred &&ShouldBeCounted = [](const decltype(*std::declval<IterTy>()) &) {
      return true;
    }) {
  assert(N != std::numeric_limits<unsigned>::max());
  return !hasNItemsOrMore(Begin, End, N + 1, ShouldBeCounted);
}

/// Returns true if the given container has exactly N items
template <typename ContainerTy> bool hasNItems(ContainerTy &&C, unsigned N) {
  return hasNItems(std::begin(C), std::end(C), N);
}

/// Returns true if the given container has N or more items
template <typename ContainerTy>
bool hasNItemsOrMore(ContainerTy &&C, unsigned N) {
  return hasNItemsOrMore(std::begin(C), std::end(C), N);
}

/// Returns true if the given container has N or less items
template <typename ContainerTy>
bool hasNItemsOrLess(ContainerTy &&C, unsigned N) {
  return hasNItemsOrLess(std::begin(C), std::end(C), N);
}

/// Returns a raw pointer that represents the same address as the argument.
///
/// This implementation can be removed once we move to C++20 where it's defined
/// as std::to_address().
///
/// The std::pointer_traits<>::to_address(p) variations of these overloads has
/// not been implemented.
template <class Ptr> auto to_address(const Ptr &P) { return P.operator->(); }
template <class T> constexpr T *to_address(T *P) { return P; }

} // end namespace llvm

#endif // LLVM_ADT_STLEXTRAS_H
