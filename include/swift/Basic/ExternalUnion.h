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

#include "llvm/Support/Compiler.h"
#include <type_traits>
#include <utility>
#include <assert.h>

namespace swift {

template <size_t... Values>
struct static_max;

template <size_t Value>
struct static_max<Value> {
  static const size_t value = Value;
};

template <size_t Head, size_t... Tail>
struct static_max<Head, Tail...> {
  static const size_t value =
    (Head > static_max<Tail...>::value ? Head : static_max<Tail...>::value);
};

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

template <class... Members>
struct SpecialMembers;

/// An external union whose discriminator is just a position in
/// the template arguments.
///
/// The external union itself is a trivial type, and it is the
/// responsibility of the client to call the "special member functions"
/// at the appropriate time.
template <class... Members>
class BasicExternalUnion {
  /// The value storage.
  LLVM_ALIGNAS(static_max<alignof(Members)...>::value)
  char Storage[static_max<sizeof(Members)...>::value];

public:
  enum : bool {
    union_is_trivially_copyable =
      SpecialMembers<Members...>::is_trivially_copyable
  };

  /// Construct a union member in-place.
  template <class T, class... Args>
  T &emplaceWithoutIndex(Args &&... args) {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");

    return *(::new ((void*) &Storage) T(std::forward<Args>(args)...));
  }

  /// Construct a union member in-place.
  template <class T, class... Args>
  T &emplace(int index, Args &&... args) {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");
    assert(index == typeIndex && "current kind is wrong for value");

    return *(::new ((void*) &Storage) T(std::forward<Args>(args)...));
  }

  /// Construct a union member in-place using list-initialization ({}).
  template <class T, class... Args>
  T &emplaceAggregateWithoutIndex(Args &&... args) {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");

    return *(::new ((void*) &Storage) T{std::forward<Args>(args)...});
  }

  /// Construct a union member in-place using list-initialization ({}).
  template <class T, class... Args>
  T &emplaceAggregate(int index, Args &&... args) {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");
    assert(index == typeIndex && "current kind is wrong for value");

    return *(::new ((void*) &Storage) T{std::forward<Args>(args)...});
  }

  /// Return a reference to a union member.
  template <class T>
  T &getWithoutIndex() {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");

    return reinterpret_cast<T &>(Storage);
  }

  /// Return a reference to a union member.
  template <class T>
  const T &getWithoutIndex() const {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");

    return reinterpret_cast<const T &>(Storage);
  }

  /// Return a reference to a union member, asserting that the current
  /// kind matches the type being extracted.
  template <class T>
  T &get(int index) {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");
    assert(index == typeIndex && "current kind is wrong for access");

    return reinterpret_cast<T &>(Storage);
  }

  /// Return a reference to a union member, asserting that the current
  /// kind matches the type being extracted.
  template <class T>
  const T &get(int index) const {
    constexpr int typeIndex = indexOf<T, Members...>::value;
    static_assert(typeIndex != -1, "type not in union");
    assert(index == typeIndex && "current kind is wrong for access");

    return reinterpret_cast<const T &>(Storage);
  }

  /// Copy-construct the union from another union.
  void copyConstruct(int index, const BasicExternalUnion &other) {
    if (index != -1) {
      SpecialMembers<Members...>::copyConstruct(Storage, index, other.Storage);
    }
  }

  /// Move-construct the union from another union.
  void moveConstruct(int index, BasicExternalUnion &&other) {
    if (index != -1) {
      SpecialMembers<Members...>::moveConstruct(Storage, index, other.Storage);
    }
  }

  /// Copy-assign the union from another union.
  void copyAssign(int thisIndex, int otherIndex,
                  const BasicExternalUnion &other) {
    if (this == &other) {
      // do nothing
    } else if (thisIndex == otherIndex) {
      if (thisIndex != -1) {
        SpecialMembers<Members...>::copyAssignSame(thisIndex, Storage,
                                                   other.Storage);  
      }
    } else {
      destruct(thisIndex, Storage);
      copyConstruct(otherIndex, other);
    }
  }

  /// Move-assign the union from another union.
  void moveAssign(int thisIndex, int otherIndex,
                  BasicExternalUnion &&other) {
    assert(this != &other && "move-constructing value into itself?");

    if (thisIndex == otherIndex) {
      if (thisIndex != -1) {
        SpecialMembers<Members...>::moveAssignSame(thisIndex, Storage,
                                                   other.Storage);  
      }
    } else {
      destruct(thisIndex);
      moveConstruct(otherIndex, std::move(other));
    }
  }

  /// Destroy the union from another union.
  void destruct(int index) {
    if (index != -1) {
      SpecialMembers<Members...>::destruct(index, Storage);
    }    
  }
};

/// An external union whose membership is determined by a kind type
/// whose members are not necessarily 1-1 with the members of the union.
///
/// Clients must provide a function which translates the kind type
/// into an index into the union's Members list, or -1 for kind values
/// which do not require data in the union.
template <class Kind, int (&GetIndexForKind)(Kind), class... Members>
class ExternalUnion {
  BasicExternalUnion<Members...> Union;

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

/// A helper class for defining special members.
template <>
struct SpecialMembers<> {
  enum : bool {
    is_trivially_copyable = true
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, int index, const void *other) {
    llvm_unreachable("bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, int index, void *other) {
    llvm_unreachable("bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(int index, void *self, const void *other) {
    llvm_unreachable("bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(int index, void *self, void *other) {
    llvm_unreachable("bad index");
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(int index, void *self) {
    llvm_unreachable("bad index");
  }
};

template <class T, class... Others>
struct SpecialMembers<T, Others...> {
  enum : bool {
    is_trivially_copyable =
      std::is_trivially_copyable<T>::value &&
      SpecialMembers<Others...>::is_trivially_copyable
  };

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyConstruct(void *self, int index, const void *other) {
    if (index == 0) {
      ::new (self) T(*static_cast<const T *>(other));
    } else {
      SpecialMembers<Others...>::copyConstruct(self, index - 1, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveConstruct(void *self, int index, void *other) {
    if (index == 0) {
      ::new (self) T(std::move(*static_cast<T *>(other)));
    } else {
      SpecialMembers<Others...>::moveConstruct(self, index - 1, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void copyAssignSame(int index, void *self, const void *other) {
    if (index == 0) {
      *static_cast<T*>(self) = *static_cast<const T *>(other);
    } else {
      SpecialMembers<Others...>::copyAssignSame(index - 1, self, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void moveAssignSame(int index, void *self, void *other) {
    if (index == 0) {
      *static_cast<T*>(self) = std::move(*static_cast<T *>(other));
    } else {
      SpecialMembers<Others...>::moveAssignSame(index - 1, self, other);
    }
  }

  LLVM_ATTRIBUTE_ALWAYS_INLINE
  static void destruct(int index, void *self) {
    if (index == 0) {
      static_cast<T*>(self)->T::~T();
    } else {
      SpecialMembers<Others...>::destruct(index - 1, self);
    }
  }
};

} // end namespace swift

#endif // SWIFT_BASIC_CLUSTEREDBITVECTOR_H
