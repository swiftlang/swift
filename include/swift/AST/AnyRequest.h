//===--- AnyRequest.h - Requests Instances ----------------------*- C++ -*-===//
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
//  This file defines the AnyRequest class, which describes a stored request.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ANYREQUEST_H
#define SWIFT_AST_ANYREQUEST_H

#include "swift/Basic/TypeID.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include <string>

namespace llvm {
class raw_ostream;
}

namespace swift {

using llvm::hash_code;
using llvm::hash_value;

class DiagnosticEngine;

/// Stores a request (for the \c Evaluator class) of any kind.
///
/// Requests must be value types and provide the following API to be stored in
/// an \c AnyRequest instance:
///
///   - Copy constructor
///   - Equality operator (==)
///   - Hashing support (hash_value)
///   - TypeID support (see swift/Basic/TypeID.h)
///   - Display support (free function):
///       void simple_display(llvm::raw_ostream &, const T &);
///   - Cycle diagnostics operations:
///       void diagnoseCycle(DiagnosticEngine &diags) const;
///       void noteCycleStep(DiagnosticEngine &diags) const;
///
class AnyRequest {
  /// Abstract base class used to hold the specific request kind.
  class HolderBase {
  public:
    /// The type ID of the request being stored.
    const uint64_t typeID;

    /// Hash value for the request itself.
    const hash_code hash;

  protected:
    /// Initialize base with type ID and hash code.
    HolderBase(uint64_t typeID, hash_code hash)
      : typeID(typeID), hash(hash_combine(hash_value(typeID), hash)) { }

  public:
    virtual ~HolderBase();

    /// Determine whether this request is equivalent to the \c other
    /// request.
    virtual bool equals(const HolderBase &other) const = 0;

    /// Display.
    virtual void display(llvm::raw_ostream &out) const = 0;

    /// Diagnose a cycle detected for this request.
    virtual void diagnoseCycle(DiagnosticEngine &diags) const = 0;

    /// Note that this request is part of a cycle.
    virtual void noteCycleStep(DiagnosticEngine &diags) const = 0;
  };

  /// Holds a value that can be used as a request input/output.
  template<typename Request>
  class Holder final : public HolderBase {
  public:
    const Request request;

    Holder(const Request &request)
      : HolderBase(TypeID<Request>::value, hash_value(request)),
        request(request) { }

    Holder(Request &&request)
      : HolderBase(TypeID<Request>::value, hash_value(request)),
        request(std::move(request)) { }

    virtual ~Holder() { }

    /// Determine whether this request is equivalent to another.
    ///
    /// The caller guarantees that the typeIDs are the same.
    virtual bool equals(const HolderBase &other) const override {
      assert(typeID == other.typeID && "Caller should match typeIDs");
      return request == static_cast<const Holder<Request> &>(other).request;
    }

    /// Display.
    virtual void display(llvm::raw_ostream &out) const override {
      simple_display(out, request);
    }

    /// Diagnose a cycle detected for this request.
    virtual void diagnoseCycle(DiagnosticEngine &diags) const override {
      request.diagnoseCycle(diags);
    }

    /// Note that this request is part of a cycle.
    virtual void noteCycleStep(DiagnosticEngine &diags) const override {
      request.noteCycleStep(diags);
    }
  };

  /// FIXME: Inefficient. Use the low bits.
  enum class StorageKind {
    Normal,
    Empty,
    Tombstone,
  } storageKind = StorageKind::Normal;

  /// The data stored in this value.
  std::shared_ptr<HolderBase> stored;

  AnyRequest(StorageKind storageKind) : storageKind(storageKind) {
    assert(storageKind != StorageKind::Normal);
  }

public:
  AnyRequest(const AnyRequest &other) = default;
  AnyRequest &operator=(const AnyRequest &other) = default;

  AnyRequest(AnyRequest &&other)
      : storageKind(other.storageKind), stored(std::move(other.stored)) {
    other.storageKind = StorageKind::Empty;
  }

  AnyRequest &operator=(AnyRequest &&other) {
    storageKind = other.storageKind;
    stored = std::move(other.stored);
    other.storageKind = StorageKind::Empty;
    other.stored = nullptr;
    return *this;
  }

  AnyRequest(AnyRequest &other)
    : storageKind(other.storageKind), stored(other.stored) { }

  AnyRequest(const AnyRequest &&other)
    : storageKind(other.storageKind), stored(other.stored) { }

  /// Construct a new instance with the given value.
  template<typename T>
  AnyRequest(T&& value) : storageKind(StorageKind::Normal) {
    using ValueType =
      typename std::remove_cv<typename std::remove_reference<T>::type>::type;
    stored.reset(new Holder<ValueType>(std::forward<T>(value)));
  }

  /// Cast to a specific (known) type.
  template<typename Request>
  const Request &castTo() const {
    assert(stored->typeID == TypeID<Request>::value && "wrong type in cast");
    return static_cast<const Holder<Request> *>(stored.get())->value;
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename Request>
  const Request *getAs() const {
    if (stored->typeID != TypeID<Request>::value)
      return nullptr;

    return &static_cast<const Holder<Request> *>(stored.get())->value;
  }

  /// Diagnose a cycle detected for this request.
  void diagnoseCycle(DiagnosticEngine &diags) const {
    stored->diagnoseCycle(diags);
  }

  /// Note that this request is part of a cycle.
  void noteCycleStep(DiagnosticEngine &diags) const {
    stored->noteCycleStep(diags);
  }

  /// Compare two instances for equality.
  friend bool operator==(const AnyRequest &lhs, const AnyRequest &rhs) {
    if (lhs.storageKind != rhs.storageKind) {
      return false;
    }

    if (lhs.storageKind != StorageKind::Normal)
      return true;

    if (lhs.stored->typeID != rhs.stored->typeID)
      return false;

    return lhs.stored->equals(*rhs.stored);
  }

  friend bool operator!=(const AnyRequest &lhs, const AnyRequest &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const AnyRequest &any) {
    if (any.storageKind != StorageKind::Normal)
      return 1;

    return any.stored->hash;
  }

  friend void simple_display(llvm::raw_ostream &out, const AnyRequest &any) {
    any.stored->display(out);
  }

  /// Return the result of calling simple_display as a string.
  std::string getAsString() const;

  static AnyRequest getEmptyKey() {
    return AnyRequest(StorageKind::Empty);
  }

  static AnyRequest getTombstoneKey() {
    return AnyRequest(StorageKind::Tombstone);
  }
};

} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::AnyRequest> {
    static inline swift::AnyRequest getEmptyKey() {
      return swift::AnyRequest::getEmptyKey();
    }
    static inline swift::AnyRequest getTombstoneKey() {
      return swift::AnyRequest::getTombstoneKey();
    }
    static unsigned getHashValue(const swift::AnyRequest &request) {
      return hash_value(request);
    }
    static bool isEqual(const swift::AnyRequest &lhs,
                        const swift::AnyRequest &rhs) {
      return lhs == rhs;
    }
  };

} // end namespace llvm

#endif // SWIFT_AST_ANYREQUEST_H
