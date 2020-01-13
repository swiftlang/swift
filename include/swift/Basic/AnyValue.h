//===--- AnyValue.h - Any Value Existential ---------------------*- C++ -*-===//
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
//  This file defines the AnyValue class, which is used to store an
//  immutable value of any type.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_BASIC_ANYVALUE_H
#define SWIFT_BASIC_ANYVALUE_H

#include "swift/AST/SmallHolder.h"
#include "swift/Basic/SimpleDisplay.h"
#include "swift/Basic/TypeID.h"
#include "llvm/ADT/PointerUnion.h"  // to define hash_value
#include "llvm/ADT/TinyPtrVector.h"

namespace llvm {
  // FIXME: Belongs in LLVM itself
  template<typename PT1, typename PT2>
  hash_code hash_value(const llvm::PointerUnion<PT1, PT2> &ptr) {
    return hash_value(ptr.getOpaqueValue());
  }
}

namespace swift {

struct AnyValueVTable {
  template<typename Value>
  struct Impl {
    static void copy(AnyHolder input, void *output) {
      new (output) Value(input.get<Value>());
    }
    static void deleter(AnyHolder holder) {
      holder.getMutable<Value>().~Value();
    }
    static bool isEqual(AnyHolder lhs, AnyHolder rhs) {
      return lhs.get<Value>() == rhs.get<Value>();
    }
    static void display(AnyHolder holder, llvm::raw_ostream &out) {
      simple_display(out, holder.get<Value>());
    }
  };

  const uint64_t typeID;
  const std::function<void(AnyHolder, void *)> copy;
  const std::function<void(AnyHolder)> deleter;
  const std::function<bool(AnyHolder, AnyHolder)> isEqual;
  const std::function<void(AnyHolder, llvm::raw_ostream &)> display;

  template <typename Value>
  static const AnyValueVTable *get() {
    static const AnyValueVTable vtable = {
        TypeID<Value>::value,
        &Impl<Value>::copy,
        &Impl<Value>::deleter,
        &Impl<Value>::isEqual,
        &Impl<Value>::display,
    };
    return &vtable;
  }
};

/// Stores a value of any type that satisfies a small set of requirements.
///
/// Requirements on the values stored within an AnyValue:
///
///   - Copy constructor
///   - Equality operator (==)
///   - TypeID support (see swift/Basic/TypeID.h)
///   - Display support (free function):
///       void simple_display(llvm::raw_ostream &, const T &);
class AnyValue {
  /// The data stored in this value.
  using Storage = SmallHolder<8, alignof(void *), AnyValueVTable>;
  Storage stored;

public:
  /// Construct a new instance with the given value.
  template <typename T>
  AnyValue(T &&value) : stored(Storage(std::forward<T>(value))) {}

  /// Cast to a specific (known) type.
  template<typename T>
  const T &castTo() const {
    return *stored.get<T>();
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename T>
  const T *getAs() const {
    return stored.getAs<T>();
  }

  /// Compare two instances for equality.
  friend bool operator==(const AnyValue &lhs, const AnyValue &rhs) {
    return lhs.stored == rhs.stored;
  }

  friend bool operator!=(const AnyValue &lhs, const AnyValue &rhs) {
    return !(lhs == rhs);
  }

  friend void simple_display(llvm::raw_ostream &out, const AnyValue &value) {
    value.stored.getVTable()->display(value.stored, out);
  }

  /// Return the result of calling simple_display as a string.
  std::string getAsString() const;
};

} // end namespace swift

namespace llvm {
  template<typename T>
  bool operator==(const TinyPtrVector<T> &lhs, const TinyPtrVector<T> &rhs) {
    if (lhs.size() != rhs.size())
      return false;
    
    for (unsigned i = 0, n = lhs.size(); i != n; ++i) {
      if (lhs[i] != rhs[i])
        return false;
    }
    
    return true;
  }
  
  template<typename T>
  bool operator!=(const TinyPtrVector<T> &lhs, const TinyPtrVector<T> &rhs) {
    return !(lhs == rhs);
  }

  template<typename T>
  void simple_display(raw_ostream &out, const Optional<T> &opt) {
    if (opt) {
      simple_display(out, *opt);
    }
    out << "None";
  }
} // end namespace llvm

#endif //


