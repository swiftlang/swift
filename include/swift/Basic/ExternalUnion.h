//===- ExternalUnion.h - A union with an external discriminator -*- C++ -*-===//
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
// This file defines the ExternalUnion class, which allows clients to
// conveniently define unions of possibly non-trivial types whose
// discriminator will be provided externally.
//
// It's the client's responsibility to call the appropriate
// "special members" within its own special members.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_EXTERNALUNION_H
#define SWIFT_BASIC_EXTERNALUNION_H

#include "llvm/Support/ErrorHandling.h"
#include "swift/Basic/type_traits.h"
#include <utility>
#include <assert.h>

namespace swift {

namespace ExternalUnionImpl {

/// A helper class for finding the index of a type in a parameter pack.
/// 'indexOf<T, List...>::value' will either be the index or -1.
template <class T, class... Members>
struct indexOf;

template <class T>
struct indexOf<T> {
  constexpr static const int value = -1;
};

template <class T, class U, class... Members>
struct indexOf<T, U, Members...> {
private:
  constexpr static const int indexInTail = indexOf<T, Members...>::value;
public:
  constexpr static const int value =
    (std::is_same<T, U>::value ? 0 :
     indexInTail != -1 ? indexInTail + 1 : -1);
};

/// A helper class for deriving information and rule-of-five operations
/// for the storage of a union.
template <class... Members>
struct MembersHelper;

template <unsigned NumMembers>
struct OptimalKindTypeHelper;

} // end namespace ExternalUnionImpl

/// A class used to define the list of member types which need to be
/// stored in an ExternalUnion.  As a special case, you may use 'void'
/// to indicate that an empty state is required in the union.
template <class... Members>
struct ExternalUnionMembers {
  // (private to the implementation)
  using Info = ExternalUnionImpl::MembersHelper<Members...>;

  /// The type of indices into the union member type list.
  enum Index : unsigned {};

  /// Return the index for the given type, asserting that it is a member
  /// of the list.
  template <class T>
  static constexpr Index indexOf() {
    static_assert(ExternalUnionImpl::indexOf<T, Members...>::value != -1,
                  "type not registered in union");
    return Index(ExternalUnionImpl::indexOf<T, Members...>::value);
  }

  /// Return the index for the given type or -1 if it is not a member
  /// of the list.  If it is not -1, it may be safely converted to an Index.
  template <class T>
  static constexpr int maybeIndexOf() {
    return ExternalUnionImpl::indexOf<T, Members...>::value;
  }
};

/// An external union that uses the member-list index as the user-facing
/// discriminator kind.
///
/// This type can be used directly, but it's generally better to use
/// ExternalUnion instead.  If nothing else, it's not a good idea to
/// cement the assumption that you won't have two cases that need the
/// same storage.
///
/// The external union itself is a trivial type, and it is the
/// responsibility of the client to call the "special member functions"
/// at the appropriate time.
template <class Members>
class BasicExternalUnion {

  /// The value storage.
  alignas(Members::Info::alignment) char Storage[Members::Info::size];

  template <class T>
  static constexpr int maybeIndexOfMember() {
    return Members::template maybeIndexOf<T>();
  }

public:
  enum : bool {
    union_is_trivially_copyable = Members::Info::is_trivially_copyable
  };

  using Index = typename Members::Index;

  /// Construct a union member in-place.
  template <class T, class... Args>
  T &emplaceWithoutIndex(Args &&... args) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");

    return *(::new ((void*) &Storage) T(std::forward<Args>(args)...));
  }

  /// Construct a union member in-place.
  template <class T, class... Args>
  T &emplace(Index index, Args &&... args) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");
    assert(index == Index(typeIndex) && "current kind is wrong for value");

    return *(::new ((void*) &Storage) T(std::forward<Args>(args)...));
  }

  /// Construct a union member in-place using list-initialization ({}).
  template <class T, class... Args>
  T &emplaceAggregateWithoutIndex(Args &&... args) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");

    return *(::new ((void*) &Storage) T{std::forward<Args>(args)...});
  }

  /// Construct a union member in-place using list-initialization ({}).
  template <class T, class... Args>
  T &emplaceAggregate(Index index, Args &&... args) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");
    assert(index == Index(typeIndex) && "current kind is wrong for value");

    return *(::new ((void*) &Storage) T{std::forward<Args>(args)...});
  }

  /// Return a reference to a union member.
  template <class T>
  T &getWithoutIndex() {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");

    return reinterpret_cast<T &>(Storage);
  }

  /// Return a reference to a union member.
  template <class T>
  const T &getWithoutIndex() const {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");

    return reinterpret_cast<const T &>(Storage);
  }

  /// Return a reference to a union member, asserting that the current
  /// kind matches the type being extracted.
  template <class T>
  T &get(Index index) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");
    assert(index == Index(typeIndex) && "current kind is wrong for access");

    return reinterpret_cast<T &>(Storage);
  }

  /// Return a reference to a union member, asserting that the current
  /// kind matches the type being extracted.
  template <class T>
  const T &get(Index index) const {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");
    assert(index == Index(typeIndex) && "current kind is wrong for access");

    return reinterpret_cast<const T &>(Storage);
  }

  /// Destruct the current union member.
  template <class T>
  void resetToEmptyWithoutIndex() {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");

    reinterpret_cast<T&>(Storage).T::~T();
  }

  /// Destroy the current union member.
  template <class T>
  void resetToEmpty(Index oldIndex, Index newIndex) {
    constexpr int typeIndex = maybeIndexOfMember<T>();
    static_assert(typeIndex != -1, "type not in union");
    constexpr int voidTypeIndex = maybeIndexOfMember<void>();
    static_assert(voidTypeIndex != -1, "union has not empty storage");
    assert(oldIndex == Index(typeIndex) && "current kind is wrong for value");
    assert(newIndex == Index(voidTypeIndex) && "new kind is not in union");

    reinterpret_cast<T&>(Storage).T::~T();
  }

  /// Copy-construct the union from another union.
  void copyConstruct(Index index, const BasicExternalUnion &other) {
    Members::Info::copyConstruct(Storage, unsigned(index), other.Storage);
  }

  /// Move-construct the union from another union.
  void moveConstruct(Index index, BasicExternalUnion &&other) {
    Members::Info::moveConstruct(Storage, unsigned(index), other.Storage);
  }

  /// Copy-assign the union from another union.
  void copyAssign(Index thisIndex, Index otherIndex,
                  const BasicExternalUnion &other) {
    if (this == &other) {
      // do nothing
    } else if (thisIndex == otherIndex) {
      Members::Info::copyAssignSame(unsigned(thisIndex),
                                    Storage, other.Storage);
    } else {
      destruct(thisIndex, Storage);
      copyConstruct(otherIndex, other);
    }
  }

  /// Move-assign the union from another union.
  void moveAssign(Index thisIndex, Index otherIndex,
                  BasicExternalUnion &&other) {
    assert(this != &other && "move-constructing value into itself?");

    if (thisIndex == otherIndex) {
      Members::Info::moveAssignSame(unsigned(thisIndex),
                                    Storage, other.Storage);
    } else {
      destruct(thisIndex);
      moveConstruct(otherIndex, std::move(other));
    }
  }

  /// Destroy the union from another union.
  void destruct(Index index) {
    Members::Info::destruct(unsigned(index), Storage);
  }
};

/// An external union whose membership is determined by a kind type
/// whose members are not necessarily 1-1 with the members of the union.
///
/// Clients must provide a function which translates the kind type
/// into an index into the union's members list.  The expected pattern
/// here is something like this:
///
///   using Members = ExternalUnionMembers<void, int, std::string>;
///   static Members::Index getIndexForKind(Kind kind) {
///     switch (kind) {
///     case Kind::Nothing: return Members::indexOf<void>();
///     case Kind::Happy: return Members::indexOf<int>();
///     case Kind::Sad: return Members::indexOf<int>();
///     case Kind::Funny: return Members::indexOf<std::string>();
///     case Kind::Angry: return Members::indexOf<std::string>();
///     }
///     llvm_unreachable("bad kind");
///   }
///   ExternalUnion<Kind, Members, getIndexForKind> Storage;
///
template <class Kind, class Members,
          typename Members::Index (&GetIndexForKind)(Kind)>
class ExternalUnion {
  BasicExternalUnion<Members> Union;

public:
  enum : bool {
    union_is_trivially_copyable = decltype(Union)::union_is_trivially_copyable
  };

  /// Construct a union member in-place.
  template <class T, class... Args>
  T &emplace(Kind kind, Args &&... args) {
#ifndef NDEBUG
    return Union.template emplace<T>(GetIndexForKind(kind),
                                     std::forward<Args>(args)...);
#else
    return Union.template emplaceWithoutIndex<T>(std::forward<Args>(args)...);
#endif
  }

  /// Construct a union member in-place using list-initialization ({}).
  template <class T, class... Args>
  T &emplaceAggregate(Kind kind, Args &&... args) {
#ifndef NDEBUG
    return Union.template emplaceAggregate<T>(GetIndexForKind(kind),
                                     std::forward<Args>(args)...);
#else
    return Union.template emplaceAggregateWithoutIndex<T>(
                                     std::forward<Args>(args)...);
#endif
  }

  /// Destroy the current member of the union and switch to a member
  /// that has no storage.
  template <class T>
  void resetToEmpty(Kind curKind, Kind newKind) {
#ifndef NDEBUG
    return Union.template resetToEmpty<T>(GetIndexForKind(curKind),
                                          GetIndexForKind(newKind));
#else
    return Union.template resetToEmptyWithoutIndex<T>();
#endif
  }

  /// Return a reference to a union member, asserting that the current
  /// kind is right.
  template <class T>
  T &get(Kind kind) {
#ifndef NDEBUG
    return Union.template get<T>(GetIndexForKind(kind));
#else
    return Union.template getWithoutIndex<T>();
#endif
  }

  /// Return a reference to a union member, asserting that the current
  /// kind is right.
  template <class T>
  const T &get(Kind kind) const {
#ifndef NDEBUG
    return Union.template get<T>(GetIndexForKind(kind));
#else
    return Union.template getWithoutIndex<T>();
#endif
  }

  /// Copy-construct the union from another union.
  void copyConstruct(Kind kind, const ExternalUnion &other) {
    Union.copyConstruct(GetIndexForKind(kind), other.Union);
  }

  /// Move-construct the union from another union.
  void moveConstruct(Kind kind, ExternalUnion &&other) {
    Union.moveConstruct(GetIndexForKind(kind), std::move(other.Union));
  }

  /// Copy-assign the union from another union.
  void copyAssign(Kind thisKind, Kind otherKind, const ExternalUnion &other) {
    Union.copyAssign(GetIndexForKind(thisKind), GetIndexForKind(otherKind),
                     other.Union);
  }

  /// Move-assign the union from another union.
  void moveAssign(Kind thisKind, Kind otherKind, ExternalUnion &&other) {
    Union.moveAssign(GetIndexForKind(thisKind), GetIndexForKind(otherKind),
                     std::move(other.Union));
  }

  /// Destroy the union from another union.
  void destruct(Kind kind) {
    Union.destruct(GetIndexForKind(kind));
  }
};

template <class KindHelper, class Members>
class SimpleExternalUnionBase
  : public ExternalUnion<typename KindHelper::Kind, Members,
              KindHelper::template coerceKindToIndex<typename Members::Index>> {
public:
  using Kind = typename KindHelper::Kind;

  template <class T>
  static constexpr Kind kindForMember() {
    return KindHelper::coerceIndexToKind(Members::template indexOf<T>());
  }

  template <class T>
  T *dyn_cast(Kind kind) {
    return (kind == kindForMember<T>()
              ? &this->template get<T>(kind) : nullptr);
  }

  template <class T>
  const T *dyn_cast(Kind kind) const {
    return (kind == kindForMember<T>()
              ? &this->template get<T>(kind) : nullptr);
  }
};

/// A particularly simple form of ExternalUnion suitable for unions where
/// the kind only exists to distinguish between cases of the union.
///
/// Recommended usage:
///   using Union = SimpleExternalUnion<void, int, std::string>;
///   Union::Kind Kind : 2 = Union::kindForMember<void>();
///   ...
///   Union Storage;
template <class... Members>
class SimpleExternalUnion
    : public SimpleExternalUnionBase<
        ExternalUnionImpl::OptimalKindTypeHelper<sizeof...(Members)>,
        ExternalUnionMembers<Members...> > {
};

namespace ExternalUnionImpl {

/// The MembersHelper base case.
template <>
struct MembersHelper<> {
  enum : bool {
    is_trivially_copyable = true
  };

  enum : size_t {
    size = 1,
    alignment = 1
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, int index, const void *other) {
    assert(false && "bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, int index, void *other) {
    assert(false && "bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(int index, void *self, const void *other) {
    assert(false && "bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(int index, void *self, void *other) {
    assert(false && "bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(int index, void *self) {
    assert(false && "bad index");
  }
};

template <class T>
struct UnionMemberInfo;

/// The  helper class for defining special members.
template <class H, class... T>
struct MembersHelper<H, T...> {
private:
  using Member = UnionMemberInfo<H>;
  using Others = MembersHelper<T...>;

public:
  enum : bool {
    is_trivially_copyable =
      Member::is_trivially_copyable && Others::is_trivially_copyable
  };

  enum : size_t {
    size = Member::size > Others::size
         ? Member::size : Others::size,
    alignment = Member::alignment > Others::alignment
              ? Member::alignment : Others::alignment
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, unsigned index, const void *other) {
    if (index == 0) {
      Member::copyConstruct(self, other);
    } else {
      Others::copyConstruct(self, index - 1, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, unsigned index, void *other) {
    if (index == 0) {
      Member::moveConstruct(self, other);
    } else {
      Others::moveConstruct(self, index - 1, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(unsigned index, void *self, const void *other) {
    if (index == 0) {
      Member::copyAssignSame(self, other);
    } else {
      Others::copyAssignSame(index - 1, self, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(unsigned index, void *self, void *other) {
    if (index == 0) {
      Member::moveAssignSame(self, other);
    } else {
      Others::moveAssignSame(index - 1, self, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(int index, void *self) {
    if (index == 0) {
      Member::destruct(self);
    } else {
      Others::destruct(index - 1, self);
    }
  }
};

/// The standard implementation of UnionMemberInfo.
template <class T>
struct UnionMemberInfo {
  enum : bool {
    is_trivially_copyable = IsTriviallyCopyable<T>::value
  };

  enum : size_t {
    size = sizeof(T),
    alignment = alignof(T)
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, const void *other) {
    ::new (self) T(*static_cast<const T *>(other));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, void *other) {
    ::new (self) T(std::move(*static_cast<T *>(other)));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(void *self, const void *other) {
    *static_cast<T*>(self) = *static_cast<const T *>(other);
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(void *self, void *other) {
    *static_cast<T*>(self) = std::move(*static_cast<T *>(other));
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(void *self) {
    static_cast<T*>(self)->T::~T();
  }
};

/// An explicit specialization of UnionMemberInfo for 'void', which
/// represents the empty state.
template <>
struct UnionMemberInfo<void> {
  enum : bool {
    is_trivially_copyable = true
  };

  enum : size_t {
    size = 0,
    alignment = 1
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, const void *other) {}

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, void *other) {}

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(void *self, const void *other) {}

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(void *self, void *other) {}

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(void *self) {}
};

template <unsigned NumMembers,
          bool FitsInUInt8 = (NumMembers < (1 << sizeof(uint8_t))),
          bool FitsInUInt16 = (NumMembers < (1 << sizeof(uint16_t)))>
struct OptimalUnderlyingType;

template <unsigned NumMembers>
struct OptimalUnderlyingType<NumMembers, true, true> {
  using type = uint8_t;
};

template <unsigned NumMembers>
struct OptimalUnderlyingType<NumMembers, false, true> {
  using type = uint16_t;
};

template <unsigned NumMembers>
struct OptimalUnderlyingType<NumMembers, false, false> {
  using type = unsigned;
};

template <unsigned NumMembers>
struct OptimalKindTypeHelper {
private:
  using UnderlyingType = typename OptimalUnderlyingType<NumMembers>::type;
public:
  enum Kind : UnderlyingType {};

  template <class IndexType>
  static constexpr IndexType coerceKindToIndex(Kind kind) {
    return IndexType(UnderlyingType(kind));
  }

  template <class IndexType>
  static constexpr Kind coerceIndexToKind(IndexType index) {
    return Kind(UnderlyingType(index));
  }
};

} // end namespace ExternalUnionImpl
} // end namespace swift

#endif // SWIFT_BASIC_CLUSTEREDBITVECTOR_H
