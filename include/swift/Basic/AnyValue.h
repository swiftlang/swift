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
  /// Abstract base class used to hold on to a value.
  class HolderBase {
  public:
    /// Type ID number.
    const uint64_t typeID;

    HolderBase() = delete;
    HolderBase(const HolderBase &) = delete;
    HolderBase(HolderBase &&) = delete;
    HolderBase &operator=(const HolderBase &) = delete;
    HolderBase &operator=(HolderBase &&) = delete;

    /// Initialize base with type ID.
    HolderBase(uint64_t typeID) : typeID(typeID) { }

    virtual ~HolderBase();

    /// Determine whether this value is equivalent to another.
    ///
    /// The caller guarantees that the type IDs are the same.
    virtual bool equals(const HolderBase &other) const = 0;

    /// Display.
    virtual void display(llvm::raw_ostream &out) const = 0;
  };

  /// Holds a value that can be used as a request input/output.
  template<typename T>
  class Holder final : public HolderBase {
  public:
    const T value;

    Holder(T &&value)
      : HolderBase(TypeID<T>::value),
        value(std::move(value)) { }

    Holder(const T &value) : HolderBase(TypeID<T>::value), value(value) { }

    virtual ~Holder() { }

    /// Determine whether this value is equivalent to another.
    ///
    /// The caller guarantees that the type IDs are the same.
    virtual bool equals(const HolderBase &other) const override {
      assert(typeID == other.typeID && "Caller should match type IDs");
      return value == static_cast<const Holder<T> &>(other).value;
    }

    /// Display.
    virtual void display(llvm::raw_ostream &out) const override {
      simple_display(out, value);
    }
  };

  /// The data stored in this value.
  std::shared_ptr<HolderBase> stored;

public:
  /// Construct a new instance with the given value.
  template<typename T>
  AnyValue(T&& value) {
    using ValueType = typename std::remove_reference<T>::type;
    stored.reset(new Holder<ValueType>(std::forward<T>(value)));
  }

  /// Cast to a specific (known) type.
  template<typename T>
  const T &castTo() const {
    assert(stored->typeID == TypeID<T>::value);
    return static_cast<const Holder<T> *>(stored.get())->value;
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename T>
  const T *getAs() const {
    if (stored->typeID != TypeID<T>::value)
      return nullptr;

    return &static_cast<const Holder<T> *>(stored.get())->value;
  }

  /// Compare two instances for equality.
  friend bool operator==(const AnyValue &lhs, const AnyValue &rhs) {
    if (lhs.stored->typeID != rhs.stored->typeID)
      return false;

    return lhs.stored->equals(*rhs.stored);
  }

  friend bool operator!=(const AnyValue &lhs, const AnyValue &rhs) {
    return !(lhs == rhs);
  }

  friend void simple_display(llvm::raw_ostream &out, const AnyValue &value) {
    value.stored->display(out);
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


