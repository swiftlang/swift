//===- TaggedUnion.h - A tagged union ---------------------------*- C++ -*-===//
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
// This file provides a simple tagged union, like std::variant but without
// any effort to be exception-safe and therefore much simpler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_TAGGEDUNION_H
#define SWIFT_BASIC_TAGGEDUNION_H

#include "swift/Basic/ExternalUnion.h"

namespace swift {

namespace TaggedUnionImpl {
template <class T>
using simplify_member_type
  = typename std::remove_const<typename std::remove_reference<T>::type>::type;

/// Given a type `T` deduced from a `T &&` parameter, can it be used to
/// construct a member of the union?  If so, `simplify_member_type<T>`
/// will give the formal member type.
template <class Members, class T>
inline constexpr bool is_member_constructible() {
  return Members::template contains<simplify_member_type<T>>();
}

struct Empty {};
}

template <class KindHelper, class Members,
          bool NonTrivial = !Members::Info::is_trivially_copyable,
          bool HasVoid = Members::template contains<void>()>
class TaggedUnionBase;

/// The base partial specialization.  All the other partial specializations
/// will end up inheriting from this.
template <class KindHelper, class Members>
class TaggedUnionBase<KindHelper, Members,
                      /*NonTrivial*/ false,
                      /*HasVoid*/ false> {
protected:
  using StorageType = SimpleExternalUnionBase<KindHelper, Members>;
  using Kind = typename KindHelper::Kind;
  StorageType Storage;
  Kind TheKind;

  TaggedUnionBase(Kind theKind) : TheKind(theKind) {}

public:
  /// Construct the union with a value of the given type, which must
  /// (ignoring references) be one of the declared members of the union.
  template <class T>
  TaggedUnionBase(T &&value,
                  typename std::enable_if<
                    TaggedUnionImpl::is_member_constructible<Members, T>(),
                    TaggedUnionImpl::Empty>::type = {}) {
    using TargetType = TaggedUnionImpl::simplify_member_type<T>;
    TheKind = StorageType::template kindForMember<TargetType>();
    Storage.template emplace<TargetType>(TheKind, std::forward<T>(value));
  }

  template <class T>
  typename std::enable_if<TaggedUnionImpl::is_member_constructible<Members, T>(),
                          TaggedUnionBase &>::type
  operator=(T &&value) {
    using TargetType = TaggedUnionImpl::simplify_member_type<T>;
    TheKind = StorageType::template kindForMember<TargetType>();
    Storage.template emplace<TargetType>(TheKind, std::forward<T>(value));
    return *this;    
  }

  /// Replace the current value in the union with a value of the given
  /// type, which must be one of the declared members of the union.
  ///
  /// The value will be constructed from the arguments with an argument
  /// list (i.e. `new(ptr) T(...)`).  If aggregate initialization is required,
  /// use `emplaceAggregate`.
  template <class T, class... Args>
  T &emplace(Args &&... args) {
    Storage.destruct(TheKind);
    TheKind = StorageType::template kindForMember<T>();
    return Storage.template emplace<T>(TheKind, std::forward<Args>(args)...);
  }

  /// Replace the current value in the union with a value of the given
  /// type, which must be one of the declared members of the union.
  ///
  /// The value will be constructed from the arguments with a braced
  /// initializer list (i.e. `T{...}`) and therefore may use aggregate
  /// initialization.
  template <class T, class... Args>
  T &emplaceAggregate(Args &&... args) {
    Storage.destruct(TheKind);
    TheKind = StorageType::template kindForMember<T>();
    return Storage.template emplaceAggregate<T>(TheKind,
                                                std::forward<Args>(args)...);
  }

  /// Return true if the union is storing a value of the given type,
  /// which must be one of the declared members of the union.
  template <class T>
  bool isa() const {
    return TheKind == StorageType::template kindForMember<T>();
  }

  /// Return a pointer to the value if the union is storing a value of the
  /// given type, which must be one of the declared members of the union.
  template <class T>
  T *dyn_cast() {
    return Storage.template dyn_cast<T>(TheKind);
  }

  /// Return a pointer to the value if the union is storing a value of the
  /// given type, which must be one of the declared members of the union.
  template <class T>
  const T *dyn_cast() const {
    return Storage.template dyn_cast<T>(TheKind);
  }

  /// Assert that the union is storing a value of the given type and return
  /// a reference to it.
  template <class T>
  T &get() {
    return Storage.template get<T>(TheKind);
  }

  /// Assert that the union is storing a value of the given type and return
  /// a reference to it.
  template <class T>
  const T &get() const {
    return Storage.template get<T>(TheKind);
  }
};

/// The partial specialization for when the union isn't trivially-copyable.
template <class KindHelper, class Members>
class TaggedUnionBase<KindHelper, Members, /*NonTrivial*/ true, /*HasVoid*/ false>
       : public TaggedUnionBase<KindHelper, Members, false, false> {
  using super = TaggedUnionBase<KindHelper, Members, false, false>;

protected:
  using super::Storage;
  using super::TheKind;

  TaggedUnionBase(typename super::Kind kind) : super(kind) {}

public:
  template <class T>
  TaggedUnionBase(T &&value,
                  typename std::enable_if<
                    TaggedUnionImpl::is_member_constructible<Members, T>(),
                    TaggedUnionImpl::Empty>::type = {})
    : super(std::forward<T>(value)) {}

  TaggedUnionBase(const TaggedUnionBase &other) : super(other.TheKind) {
    Storage.copyConstruct(other.TheKind, other.Storage);
  }

  TaggedUnionBase(TaggedUnionBase &&other) : super(other.TheKind) {
    Storage.moveConstruct(other.TheKind, std::move(other.Storage));
  }

  TaggedUnionBase &operator=(const TaggedUnionBase &other) {
    Storage.copyAssign(TheKind, other.TheKind, other.Storage);
    TheKind = other.TheKind;
    return *this;
  }

  TaggedUnionBase &operator=(TaggedUnionBase &&other) {
    Storage.moveAssign(TheKind, other.TheKind, std::move(other.Storage));
    TheKind = other.TheKind;
    return *this;
  }

  ~TaggedUnionBase() {
    Storage.destruct(TheKind);
  }
};

/// The partial specialization for when `void` is a member of the union.
template <class KindHelper, class Members, bool NonTrivial>
class TaggedUnionBase<KindHelper, Members, NonTrivial, /*HasVoid*/ true>
       : public TaggedUnionBase<KindHelper, Members, NonTrivial, false> {
  using super = TaggedUnionBase<KindHelper, Members, NonTrivial, false>;

protected:
  using super::Storage;
  using super::TheKind;

  static constexpr typename super::Kind kindForVoid() {
    return super::StorageType::template kindForMember<void>();
  }

public:
  template <class T>
  TaggedUnionBase(T &&value,
                  typename std::enable_if<
                    TaggedUnionImpl::is_member_constructible<Members, T>(),
                    TaggedUnionImpl::Empty>::type = {})
    : super(std::forward<T>(value)) {}

  /// Construct the union in the empty state.
  TaggedUnionBase() : super(kindForVoid()) {}

  /// Test whether the union is in the empty state.
  bool empty() const {
    return TheKind == kindForVoid();
  }

  /// Reset the union to the empty state.
  void reset() {
    Storage.destruct(TheKind);
    TheKind = kindForVoid();
  }
};

/// A tagged union of the given types.
///
/// Non-trivial members are supported; they will make the union
/// non-trivial to copy, move, and destruct.
///
/// The union provides the following members, as described in the
/// documentation for the primary partial specialization of
/// TaggedUnionBase.  In the following, `M` must be a declared member
/// type of the union.
///
/// ```
///   TaggedUnion(M);
///   M &emplace<M>(T...);
///   M &emplaceAggregate<M>(T...);
///   bool isa<M>() const;
///   M *dyn_cast<M>();
///   const M *dyn_cast<M>() const;
///   M &get<M>();
///   const M &get<M>() const;
/// ```
///
/// Additionally, if `void` is one of the types, the union supports an
/// empty state and provides a default constructor as well as `empty()`
/// and `reset()` methods.
template <class... Members>
using TaggedUnion =
  TaggedUnionBase<ExternalUnionImpl::OptimalKindTypeHelper<sizeof...(Members)>,
                  ExternalUnionMembers<Members...>>;

}

#endif
