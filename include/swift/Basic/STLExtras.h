//===- STLExtras.h - additions to the STL -----------------------*- C++ -*-===//
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
/// \file Provides STL-style algorithms for convenience.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_INTERLEAVE_H
#define SWIFT_BASIC_INTERLEAVE_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/Optional.h"
#include "llvm/Support/Casting.h"
#include <cassert>
#include <functional>
#include <iterator>
#include <type_traits>
#include <algorithm>

namespace swift {

/// @{

/// An STL-style algorithm similar to std::for_each that applies a second
/// functor between every pair of elements.
///
/// This provides the control flow logic to, for example, print a
/// comma-separated list:
/// \code
///   interleave(names.begin(), names.end(),
///              [&](StringRef name) { OS << name; },
///              [&] { OS << ", "; });
/// \endcode
template <typename ForwardIterator, typename UnaryFunctor,
          typename NullaryFunctor>
inline void interleave(ForwardIterator begin, ForwardIterator end,
                       UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  if (begin == end)
    return;
  each_fn(*begin);
  ++begin;
  for (; begin != end; ++begin) {
    between_fn();
    each_fn(*begin);
  }
}

template <typename Container, typename UnaryFunctor, typename NullaryFunctor>
inline void interleave(const Container &c, UnaryFunctor each_fn,
                       NullaryFunctor between_fn) {
  interleave(c.begin(), c.end(), each_fn, between_fn);
}

/// @}
/// @{

/// The equivalent of std::for_each, but for two lists at once.
template <typename InputIt1, typename InputIt2, typename BinaryFunction>
inline void for_each(InputIt1 I1, InputIt1 E1, InputIt2 I2, BinaryFunction f) {
  while (I1 != E1) {
    f(*I1, *I2);
    ++I1; ++I2;
  }
}

template <typename Container1, typename Container2, typename BinaryFunction>
inline void for_each(const Container1 &c1, const Container2 &c2,
                     BinaryFunction f) {
  assert(c1.size() == c2.size());
  for_each(c1.begin(), c1.end(), c2.begin(), f);
}

/// The equivalent of std::for_each, but for three lists at once.
template <typename InputIt1, typename InputIt2, typename InputIt3,
          typename TernaryFunction>
inline void for_each3(InputIt1 I1, InputIt1 E1, InputIt2 I2, InputIt3 I3,
                      TernaryFunction f) {
  while (I1 != E1) {
    f(*I1, *I2, *I3);
    ++I1; ++I2; ++I3;
  }
}

template <typename Container1, typename Container2, typename Container3,
          typename TernaryFunction>
inline void for_each3(const Container1 &c1, const Container2 &c2,
                      const Container3 &c3,
                     TernaryFunction f) {
  assert(c1.size() == c2.size());
  assert(c2.size() == c3.size());
  for_each3(c1.begin(), c1.end(), c2.begin(), c3.begin(), f);
}

/// @}

/// A range of iterators.
template<typename Iterator>
class IteratorRange {
  Iterator First, Last;

public:
  typedef Iterator iterator;

  IteratorRange(Iterator first, Iterator last) : First(first), Last(last) { }
  iterator begin() const { return First; }
  iterator end() const { return Last; }
  bool empty() const { return First == Last; }

  typename std::iterator_traits<iterator>::value_type front() const { 
    assert(!empty() && "Front of empty range");
    return *begin(); 
  }
};

/// Create a new iterator range.
template<typename Iterator>
inline IteratorRange<Iterator> 
makeIteratorRange(Iterator first, Iterator last) {
  return IteratorRange<Iterator>(first, last);
}

/// An iterator that filters the results of an underlying forward
/// iterator, only passing through those values that satisfy a predicate.
///
/// \tparam Iterator the underlying iterator.
///
/// \tparam Predicate A predicate that determines whether a value of the
/// underlying iterator is available in the resulting sequence.
template<typename Iterator, typename Predicate>
class FilterIterator {
  Iterator Current, End;

  /// FIXME: Could optimize away this storage with EBCO tricks.
  Predicate Pred;

  /// Skip any non-matching elements.
  void skipNonMatching() {
    while (Current != End && !Pred(*Current))
      ++Current;
  }

public:
  /// Used to indicate when the current iterator has already been
  /// "primed", meaning that it's at the end or points to a value that
  /// satisfies the predicate.
  enum PrimedT { Primed };

  typedef std::forward_iterator_tag iterator_category;
  typedef typename std::iterator_traits<Iterator>::value_type value_type;
  typedef typename std::iterator_traits<Iterator>::reference  reference;
  typedef typename std::iterator_traits<Iterator>::pointer    pointer;
  typedef typename std::iterator_traits<Iterator>::difference_type
    difference_type;

  /// Construct a new filtering iterator for the given iterator range
  /// and predicate.
  FilterIterator(Iterator current, Iterator end, Predicate pred)
    : Current(current), End(end), Pred(pred) 
  {
    // Prime the iterator.
    skipNonMatching();
  }

  /// Construct a new filtering iterator for the given iterator range
  /// and predicate, where the iterator range has already been
  /// "primed" by ensuring that it is empty or the current iterator
  /// points to something that matches the predicate.
  FilterIterator(Iterator current, Iterator end, Predicate pred, PrimedT)
    : Current(current), End(end), Pred(pred) 
  { 
    // Assert that the iterators have already been primed.
    assert(Current == End || Pred(*Current) && "Not primed!");
  }

  reference operator*() const {
    return *Current;
  }

  pointer operator->() const {
    return Current.operator->();
  }

  FilterIterator &operator++() {
    ++Current;
    skipNonMatching();
    return *this;
  }

  FilterIterator operator++(int) {
    FilterIterator old = *this;
    ++*this;
    return old;
  }

  friend bool operator==(FilterIterator lhs, FilterIterator rhs) {
    return lhs.Current == rhs.Current;
  }
  friend bool operator!=(FilterIterator lhs, FilterIterator rhs) {
    return !(lhs == rhs);
  }
};

/// Create a new filter iterator.
template<typename Iterator, typename Predicate>
inline FilterIterator<Iterator, Predicate> 
makeFilterIterator(Iterator current, Iterator end, Predicate pred) {
  return FilterIterator<Iterator, Predicate>(current, end, pred);
}

/// A range filtered by a specific predicate.
template<typename Range, typename Predicate>
class FilterRange {
  typedef typename Range::iterator Iterator;

  Iterator First, Last;
  Predicate Pred;

public:
  typedef FilterIterator<Iterator, Predicate> iterator;

  FilterRange(Range range, Predicate pred)
    : First(range.begin()), Last(range.end()), Pred(pred) 
  { 
    // Prime the sequence.
    while (First != Last && !Pred(*First))
      ++First;
  }

  iterator begin() const { 
    return iterator(First, Last, Pred, iterator::Primed); 
  }

  iterator end() const { 
    return iterator(Last, Last, Pred, iterator::Primed); 
  }

  bool empty() const { return First == Last; }

  typename std::iterator_traits<iterator>::value_type front() const { 
    assert(!empty() && "Front of empty range");
    return *begin(); 
  }
};

/// Create a new filter range.
template<typename Range, typename Predicate>
inline FilterRange<Range, Predicate> 
makeFilterRange(Range range, Predicate pred) {
  return FilterRange<Range, Predicate>(range, pred);
}

/// An iterator that transforms the result of an underlying forward
/// iterator with a given operation.
///
/// \tparam Iterator the underlying iterator.
///
/// \tparam Operation A function object that transforms the underlying
/// sequence's values into the new sequence's values.
template<typename Iterator, typename Operation>
class TransformIterator {
  Iterator Current;

  /// FIXME: Could optimize away this storage with EBCO tricks.
  Operation Op;

  /// The underlying reference type, which will be passed to the
  /// operation.
  typedef typename std::iterator_traits<Iterator>::reference
    UnderlyingReference;
 
public:
  typedef std::forward_iterator_tag iterator_category;
  typedef typename std::result_of<Operation(UnderlyingReference)>::type
    value_type;
  typedef value_type reference;
  typedef void pointer; // FIXME: Should provide a pointer proxy.
  typedef typename std::iterator_traits<Iterator>::difference_type 
    difference_type;

  /// Construct a new transforming iterator for the given iterator 
  /// and operation.
  TransformIterator(Iterator current, Operation op) 
    : Current(current), Op(op) { }

  reference operator*() const {
    return Op(*Current);
  }

  TransformIterator &operator++() {
    ++Current;
    return *this;
  }

  TransformIterator operator++(int) {
    TransformIterator old = *this;
    ++*this;
    return old;
  }

  friend bool operator==(TransformIterator lhs, TransformIterator rhs) {
    return lhs.Current == rhs.Current;
  }
  friend bool operator!=(TransformIterator lhs, TransformIterator rhs) {
    return !(lhs == rhs);
  }
};

/// Create a new transform iterator.
template<typename Iterator, typename Operation>
inline TransformIterator<Iterator, Operation> 
makeTransformIterator(Iterator current, Operation op) {
  return TransformIterator<Iterator, Operation>(current, op);
}

/// A range transformed by a specific predicate.
template<typename Range, typename Operation>
class TransformRange {
  Range Rng;
  Operation Op;

public:
  typedef TransformIterator<typename Range::iterator, Operation> iterator;

  TransformRange(Range range, Operation op)
    : Rng(range), Op(op) { }

  iterator begin() const { return iterator(Rng.begin(), Op); }
  iterator end() const { return iterator(Rng.end(), Op); }
  bool empty() const { return begin() == end(); }

  typename std::iterator_traits<iterator>::value_type front() const { 
    assert(!empty() && "Front of empty range");
    return *begin(); 
  }
};

/// Create a new transform range.
template<typename Range, typename Operation>
inline TransformRange<Range, Operation> 
makeTransformRange(Range range, Operation op) {
  return TransformRange<Range, Operation>(range, op);
}

/// An iterator that filters and transforms the results of an
/// underlying forward iterator based on an transformation from the underlying
/// value type to an optional result type.
///
/// \tparam Iterator the underlying iterator.
///
/// \tparam OptionalTransform A function object that maps a value of
/// the underlying iterator type to an optional containing a value of
/// the resulting sequence, or an empty optional if this item should
/// be skipped.
template<typename Iterator, typename OptionalTransform>
class OptionalTransformIterator {
  Iterator Current, End;

  /// FIXME: Could optimize away this storage with EBCO tricks.
  OptionalTransform Op;

  /// Skip any non-matching elements.
  void skipNonMatching() {
    while (Current != End && !Op(*Current))
      ++Current;
  }

  typedef typename std::iterator_traits<Iterator>::reference
    UnderlyingReference;
  
  typedef typename std::result_of<OptionalTransform(UnderlyingReference)>::type 
    ResultReference;

public:
  /// Used to indicate when the current iterator has already been
  /// "primed", meaning that it's at the end or points to a value that
  /// satisfies the transform.
  enum PrimedT { Primed };

  typedef std::forward_iterator_tag iterator_category;
  typedef typename ResultReference::value_type reference;
  typedef typename ResultReference::value_type value_type;
  typedef void pointer; // FIXME: should add a proxy here.
  typedef typename std::iterator_traits<Iterator>::difference_type
    difference_type;

  /// Construct a new optional transform iterator for the given
  /// iterator range and operation.
  OptionalTransformIterator(Iterator current, Iterator end, 
                            OptionalTransform op)
    : Current(current), End(end), Op(op)
  {
    // Prime the iterator.
    skipNonMatching();
  }

  /// Construct a new optional transform iterator for the given iterator range
  /// and operation, where the iterator range has already been
  /// "primed" by ensuring that it is empty or the current iterator
  /// points to something that matches the operation.
  OptionalTransformIterator(Iterator current, Iterator end, 
                            OptionalTransform op, PrimedT)
    : Current(current), End(end), Op(op) 
  { 
    // Assert that the iterators have already been primed.
    assert((Current == End || Op(*Current)) && "Not primed!");
  }

  reference operator*() const {
    return *Op(*Current);
  }

  OptionalTransformIterator &operator++() {
    ++Current;
    skipNonMatching();
    return *this;
  }

  OptionalTransformIterator operator++(int) {
    OptionalTransformIterator old = *this;
    ++*this;
    return old;
  }

  friend bool operator==(OptionalTransformIterator lhs, 
                         OptionalTransformIterator rhs) {
    return lhs.Current == rhs.Current;
  }
  friend bool operator!=(OptionalTransformIterator lhs,
                         OptionalTransformIterator rhs) {
    return !(lhs == rhs);
  }
};

/// Create a new filter iterator.
template<typename Iterator, typename OptionalTransform>
inline OptionalTransformIterator<Iterator, OptionalTransform> 
makeOptionalTransformIterator(Iterator current, Iterator end, 
                              OptionalTransform op) {
  return OptionalTransformIterator<Iterator, OptionalTransform>(current, end,
                                                                op);
}

/// A range filtered and transformed by the optional transform.
template<typename Range, typename OptionalTransform>
class OptionalTransformRange {
  typedef typename Range::iterator Iterator;

  Iterator First, Last;
  OptionalTransform Op;

public:
  typedef OptionalTransformIterator<Iterator, OptionalTransform> iterator;

  OptionalTransformRange(Range range, OptionalTransform op)
    : First(range.begin()), Last(range.end()), Op(op) 
  { 
    // Prime the sequence.
    while (First != Last && !Op(*First))
      ++First;
  }

  iterator begin() const { 
    return iterator(First, Last, Op, iterator::Primed); 
  }

  iterator end() const { 
    return iterator(Last, Last, Op, iterator::Primed); 
  }

  bool empty() const { return First == Last; }

  typename std::iterator_traits<iterator>::value_type front() const { 
    assert(!empty() && "Front of empty range");
    return *begin(); 
  }
};

/// Create a new filter range.
template<typename Range, typename OptionalTransform>
inline OptionalTransformRange<Range, OptionalTransform> 
makeOptionalTransformRange(Range range, OptionalTransform op) {
  return OptionalTransformRange<Range, OptionalTransform>(range, op);
}

/// Function object that attempts a downcast to a subclass, wrapping
/// the result in an optional to indicate success or failure.
template<typename Subclass>
struct DowncastAsOptional {
  template<typename Superclass>
  auto operator()(Superclass &value) const
         -> Optional<decltype(llvm::cast<Subclass>(value))> {
    if (auto result = llvm::dyn_cast<Subclass>(value))
      return result;

    return None;
  }

  template<typename Superclass>
  auto operator()(const Superclass &value) const
         -> Optional<decltype(llvm::cast<Subclass>(value))>
  {
    if (auto result = llvm::dyn_cast<Subclass>(value))
      return result;

    return None;
  }
};

template<typename Subclass, typename Iterator>
using DowncastFilterIterator
        = OptionalTransformIterator<Iterator, DowncastAsOptional<Subclass>>;

template<typename Subclass, typename Iterator>
inline DowncastFilterIterator<Subclass, Iterator>
makeDowncastFilterIterator(Iterator current, Iterator end) {
  DowncastAsOptional<Subclass> op;
  return DowncastFilterIterator<Subclass, Iterator>(current, end, op);
}

template<typename Subclass, typename Range>
class DowncastFilterRange 
  : public OptionalTransformRange<Range, DowncastAsOptional<Subclass>> {

  typedef OptionalTransformRange<Range, DowncastAsOptional<Subclass>> Inherited;

public:
  DowncastFilterRange(Range range) 
    : Inherited(range, DowncastAsOptional<Subclass>()) { }
};
              
template<typename Subclass, typename Range>
DowncastFilterRange<Subclass, Range>
makeDowncastFilterRange(Range range) {
  return DowncastFilterRange<Subclass, Range>(range);
}

/// Sorts and then uniques a container with random access iterators and an erase
/// method that removes a range specified by random access iterators.
template <typename Container>
void sortUnique(
    Container &C,
    typename std::enable_if<
        std::is_same<typename std::iterator_traits<
                         typename Container::iterator>::iterator_category,
                     std::random_access_iterator_tag>::value,
        void>::type * = nullptr) {
  std::sort(C.begin(), C.end());
  C.erase(std::unique(C.begin(), C.end()), C.end());
}

} // end namespace swift

#endif
