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
//  This file defines type-erasing wrappers for requests used by the Evaluator
//  class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_ANYREQUEST_H
#define SWIFT_AST_ANYREQUEST_H

#include "swift/Basic/SourceLoc.h"
#include "swift/Basic/TypeID.h"
#include "llvm/ADT/DenseMapInfo.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/PointerIntPair.h"
#include <string>

namespace llvm {
class raw_ostream;
}

namespace swift {

using llvm::hash_code;
using llvm::hash_value;

class DiagnosticEngine;

/// A collection of functions that describe how to perform operations for a
/// specific concrete request, obtained using
/// \c AnyRequestVTable::get<ConcreteRequest>().
struct AnyRequestVTable {
  template<typename Request>
  struct Impl {
    static void copy(const void *input, void *output) {
      new (output) Request(*static_cast<const Request *>(input));
    }
    static hash_code getHash(const void *ptr) {
      return hash_value(*static_cast<const Request *>(ptr));
    }
    static void deleter(void *ptr) {
      static_cast<Request *>(ptr)->~Request();
    }
    static bool isEqual(const void *lhs, const void *rhs) {
      return *static_cast<const Request *>(lhs) ==
             *static_cast<const Request *>(rhs);
    }
    static void simpleDisplay(const void *ptr, llvm::raw_ostream &out) {
      simple_display(out, *static_cast<const Request *>(ptr));
    }
    static void diagnoseCycle(const void *ptr, DiagnosticEngine &diags) {
      static_cast<const Request *>(ptr)->diagnoseCycle(diags);
    }
    static void noteCycleStep(const void *ptr, DiagnosticEngine &diags) {
      static_cast<const Request *>(ptr)->noteCycleStep(diags);
    }
    static SourceLoc getNearestLoc(const void *ptr) {
      return static_cast<const Request *>(ptr)->getNearestLoc();
    }
    static bool isCached(const void *ptr) {
      return static_cast<const Request *>(ptr)->isCached();
    }
  };

  const uint64_t typeID;
  const size_t requestSize;
  const std::function<void(const void *, void *)> copy;
  const std::function<hash_code(const void *)> getHash;
  const std::function<void(void *)> deleter;
  const std::function<bool(const void *, const void *)> isEqual;
  const std::function<void(const void *, llvm::raw_ostream &)> simpleDisplay;
  const std::function<void(const void *, DiagnosticEngine &)> diagnoseCycle;
  const std::function<void(const void *, DiagnosticEngine &)> noteCycleStep;
  const std::function<SourceLoc(const void *)> getNearestLoc;
  const std::function<bool(const void *)> isCached;

  template <typename Request,
            typename std::enable_if<Request::isEverCached>::type * = nullptr>
  static const AnyRequestVTable *get() {
    static const AnyRequestVTable vtable = {
        TypeID<Request>::value,
        sizeof(Request),
        &Impl<Request>::copy,
        &Impl<Request>::getHash,
        &Impl<Request>::deleter,
        &Impl<Request>::isEqual,
        &Impl<Request>::simpleDisplay,
        &Impl<Request>::diagnoseCycle,
        &Impl<Request>::noteCycleStep,
        &Impl<Request>::getNearestLoc,
        &Impl<Request>::isCached,
    };
    return &vtable;
  }

  template <typename Request,
            typename std::enable_if<!Request::isEverCached>::type * = nullptr>
  static const AnyRequestVTable *get() {
    static const AnyRequestVTable vtable = {
        TypeID<Request>::value,
        sizeof(Request),
        &Impl<Request>::copy,
        &Impl<Request>::getHash,
        &Impl<Request>::deleter,
        &Impl<Request>::isEqual,
        &Impl<Request>::simpleDisplay,
        &Impl<Request>::diagnoseCycle,
        &Impl<Request>::noteCycleStep,
        &Impl<Request>::getNearestLoc,
        [](auto){ return false; },
    };
    return &vtable;
  }
};

/// Base class for request type-erasing wrappers.
template <typename Derived>
class AnyRequestBase {
  friend llvm::DenseMapInfo<Derived>;

protected:
  static hash_code hashForHolder(uint64_t typeID, hash_code requestHash) {
    return hash_combine(typeID, requestHash);
  }

  enum class StorageKind : uint8_t {
    Normal,
    Empty,
    Tombstone,
  };

  /// The vtable and storage kind.
  llvm::PointerIntPair<const AnyRequestVTable *, 2, StorageKind> vtableAndKind;

  StorageKind getStorageKind() const { return vtableAndKind.getInt(); }

  /// Whether this object is storing a value, and is not empty or a tombstone.
  bool hasStorage() const {
    switch (getStorageKind()) {
    case StorageKind::Empty:
    case StorageKind::Tombstone:
      return false;
    case StorageKind::Normal:
      return true;
    }
    llvm_unreachable("Unhandled case in switch");
  }

  /// Retrieve the vtable to perform operations on the type-erased request.
  const AnyRequestVTable *getVTable() const {
    assert(hasStorage() && "Shouldn't be querying empty or tombstone");
    return vtableAndKind.getPointer();
  }

  AnyRequestBase(const AnyRequestVTable *vtable, StorageKind storageKind) {
    vtableAndKind.setPointer(vtable);
    vtableAndKind.setInt(storageKind);
    assert((bool)vtable == hasStorage() && "Must have a vtable with storage");
  }

  AnyRequestBase(const AnyRequestBase &other) {
    vtableAndKind = other.vtableAndKind;
  }
  AnyRequestBase &operator=(const AnyRequestBase &other) {
    vtableAndKind = other.vtableAndKind;
    return *this;
  }

private:
  Derived &asDerived() {
    return *static_cast<Derived *>(this);
  }
  const Derived &asDerived() const {
    return *static_cast<const Derived *>(this);
  }

  const void *getRawStorage() const { return asDerived().getRawStorage(); }

public:
  /// Cast to a specific (known) type.
  template<typename Request>
  const Request &castTo() const {
    assert(getVTable()->typeID == TypeID<Request>::value &&
           "Wrong type in cast");
    return *static_cast<const Request *>(getRawStorage());
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename Request>
  const Request *getAs() const {
    if (getVTable()->typeID != TypeID<Request>::value)
      return nullptr;

    return static_cast<const Request *>(getRawStorage());
  }

  /// Diagnose a cycle detected for this request.
  void diagnoseCycle(DiagnosticEngine &diags) const {
    getVTable()->diagnoseCycle(getRawStorage(), diags);
  }

  /// Note that this request is part of a cycle.
  void noteCycleStep(DiagnosticEngine &diags) const {
    getVTable()->noteCycleStep(getRawStorage(), diags);
  }

  /// Retrieve the nearest source location to which this request applies.
  SourceLoc getNearestLoc() const {
    return getVTable()->getNearestLoc(getRawStorage());
  }

  bool isCached() const {
    return getVTable()->isCached(getRawStorage());
  }

  /// Compare two instances for equality.
  friend bool operator==(const AnyRequestBase<Derived> &lhs,
                         const AnyRequestBase<Derived> &rhs) {
    // If the storage kinds don't match, we're done.
    if (lhs.getStorageKind() != rhs.getStorageKind())
      return false;

    // If the storage kinds do match, but there's no storage, they're trivially
    // equal.
    if (!lhs.hasStorage())
      return true;

    // Must be storing the same kind of request.
    if (lhs.getVTable()->typeID != rhs.getVTable()->typeID)
      return false;

    return lhs.getVTable()->isEqual(lhs.getRawStorage(), rhs.getRawStorage());
  }

  friend bool operator!=(const Derived &lhs, const Derived &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const AnyRequestBase<Derived> &req) {
    // If there's no storage, return a trivial hash value.
    if (!req.hasStorage())
      return 1;

    auto reqHash = req.getVTable()->getHash(req.getRawStorage());
    return hashForHolder(req.getVTable()->typeID, reqHash);
  }

  friend void simple_display(llvm::raw_ostream &out,
                             const AnyRequestBase<Derived> &req) {
    req.getVTable()->simpleDisplay(req.getRawStorage(), out);
  }
};

/// Provides a view onto a request that is stored on the stack. Objects of this
/// class must not outlive the request they reference.
///
/// Requests must be value types and provide the following API:
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
class ActiveRequest final : public AnyRequestBase<ActiveRequest> {
  template <typename T>
  friend class AnyRequestBase;

  friend class AnyRequest;
  friend llvm::DenseMapInfo<ActiveRequest>;

  /// Pointer to the request stored on the stack.
  const void *storage;

  /// Creates an \c ActiveRequest without storage.
  explicit ActiveRequest(StorageKind storageKind)
      : AnyRequestBase(/*vtable*/ nullptr, storageKind) {}

  const void *getRawStorage() const { return storage; }

public:
  /// Creates a new \c ActiveRequest referencing a concrete request on the
  /// stack.
  template <typename Request>
  explicit ActiveRequest(const Request &request)
      : AnyRequestBase(AnyRequestVTable::get<Request>(), StorageKind::Normal) {
    storage = &request;
  }
};

/// Stores a request (for the \c Evaluator class) of any kind. Unlike
/// \c ActiveRequest, this wrapper has ownership of the underlying request.
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
class AnyRequest final : public AnyRequestBase<AnyRequest> {
  template <typename T>
  friend class AnyRequestBase;

  friend llvm::DenseMapInfo<AnyRequest>;

  /// Pointer to the request on the heap.
  void *storage;

  /// Creates an \c AnyRequest without storage.
  explicit AnyRequest(StorageKind storageKind)
      : AnyRequestBase(/*vtable*/ nullptr, storageKind) {}

  const void *getRawStorage() const { return storage; }

  /// Whether this wrapper is storing the same underlying request as an
  /// \c ActiveRequest.
  bool isStorageEqual(const ActiveRequest &other) const {
    // If either wrapper isn't storing anything, just return false.
    if (!hasStorage() || !other.hasStorage())
      return false;

    if (getVTable()->typeID != other.getVTable()->typeID)
      return false;

    return getVTable()->isEqual(getRawStorage(), other.getRawStorage());
  }

public:
  AnyRequest(const AnyRequest &other) : AnyRequestBase(other) {
    if (hasStorage()) {
      // For now, just allocate a new buffer and copy across the request. This
      // should only happen when performing operations on the (disabled by
      // default) dependency graph.
      storage = llvm::safe_malloc(getVTable()->requestSize);
      getVTable()->copy(other.storage, storage);
    }
  }
  AnyRequest &operator=(const AnyRequest &other) {
    if (&other != this) {
      this->~AnyRequest();
      new (this) AnyRequest(other);
    }
    return *this;
  }

  AnyRequest(AnyRequest &&other) : AnyRequestBase(other),
        storage(other.storage) {
    new (&other) AnyRequest(StorageKind::Empty);
  }

  AnyRequest &operator=(AnyRequest &&other) {
    if (&other != this) {
      this->~AnyRequest();
      new (this) AnyRequest(std::move(other));
    }
    return *this;
  }

  // Create a local template typename `Request` in the template specialization
  // so that we can refer to it in the SFINAE condition as well as the body of
  // the template itself.  The SFINAE condition allows us to remove this
  // constructor from candidacy when evaluating explicit construction with an
  // instance of `AnyRequest`.  If we do not do so, we will find ourselves with
  // ambiguity with this constructor and the defined move constructor above.
  /// Construct a new instance with the given value.
  template <typename T, typename Request = std::decay_t<T>,
            typename = typename std::enable_if<
                !std::is_same<Request, AnyRequest>::value>::type>
  explicit AnyRequest(T &&request)
      : AnyRequestBase(AnyRequestVTable::get<Request>(), StorageKind::Normal) {
    storage = llvm::safe_malloc(sizeof(Request));
    new (storage) Request(std::forward<T>(request));
  }

  /// Construct an \c AnyRequest from an \c ActiveRequest, allowing the
  /// underlying request to persist.
  explicit AnyRequest(const ActiveRequest &req)
      : AnyRequestBase(req.getVTable(), StorageKind::Normal) {
    assert(req.hasStorage());
    storage = llvm::safe_malloc(getVTable()->requestSize);
    getVTable()->copy(req.storage, storage);
  }

  ~AnyRequest() {
    if (hasStorage()) {
      getVTable()->deleter(storage);
      std::free(storage);
    }
  }

  /// Return the result of calling simple_display as a string.
  std::string getAsString() const;
};

} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::ActiveRequest> {
    using ActiveRequest = swift::ActiveRequest;
    static inline ActiveRequest getEmptyKey() {
      return ActiveRequest(ActiveRequest::StorageKind::Empty);
    }
    static inline ActiveRequest getTombstoneKey() {
      return ActiveRequest(ActiveRequest::StorageKind::Tombstone);
    }
    static unsigned getHashValue(const ActiveRequest &request) {
      return hash_value(request);
    }
    static bool isEqual(const ActiveRequest &lhs, const ActiveRequest &rhs) {
      return lhs == rhs;
    }
  };

  template<>
  struct DenseMapInfo<swift::AnyRequest> {
    using AnyRequest = swift::AnyRequest;
    using ActiveRequest = swift::ActiveRequest;

    static inline AnyRequest getEmptyKey() {
      return AnyRequest(AnyRequest::StorageKind::Empty);
    }
    static inline swift::AnyRequest getTombstoneKey() {
      return AnyRequest(AnyRequest::StorageKind::Tombstone);
    }
    static unsigned getHashValue(const AnyRequest &request) {
      return hash_value(request);
    }
    template <typename Request>
    static unsigned getHashValue(const Request &request) {
      return AnyRequest::hashForHolder(swift::TypeID<Request>::value,
                                       hash_value(request));
    }
    static unsigned getHashValue(const ActiveRequest &request) {
      return hash_value(request);
    }
    static bool isEqual(const AnyRequest &lhs, const AnyRequest &rhs) {
      return lhs == rhs;
    }
    template <typename Request>
    static bool isEqual(const Request &lhs, const AnyRequest &rhs) {
      if (!rhs.hasStorage())
        return false;

      auto *rhsRequest = rhs.getAs<Request>();
      return rhsRequest && lhs == *rhsRequest;
    }
    static bool isEqual(const ActiveRequest &lhs, const AnyRequest &rhs) {
      return rhs.isStorageEqual(lhs);
    }
  };

} // end namespace llvm

#endif // SWIFT_AST_ANYREQUEST_H
