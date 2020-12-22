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

#include "swift/Basic/SmallHolder.h"
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
    static void copy(AnyHolder input, void *output) {
      new (output) Request(input.get<Request>());
    }
    static hash_code getHash(AnyHolder holder) {
      return hash_value(holder.get<Request>());
    }
    static void deleter(AnyHolder holder) {
      holder.getMutable<Request>().~Request();
    }
    static bool isEqual(AnyHolder lhs, AnyHolder rhs) {
      return lhs.get<Request>() == rhs.get<Request>();
    }
    static void simpleDisplay(AnyHolder holder, llvm::raw_ostream &out) {
      simple_display(out, holder.get<Request>());
    }
    static void diagnoseCycle(AnyHolder holder, DiagnosticEngine &diags) {
      holder.get<Request>().diagnoseCycle(diags);
    }
    static void noteCycleStep(AnyHolder holder, DiagnosticEngine &diags) {
      holder.get<Request>().noteCycleStep(diags);
    }
    static SourceLoc getNearestLoc(AnyHolder holder) {
      return holder.get<Request>().getNearestLoc();
    }

    template <typename Req,
              typename std::enable_if<Req::isEverCached>::type * = nullptr>
    static bool isCached(AnyHolder holder) {
      return holder.get<Request>().isCached();
    }
    template <typename Req,
              typename std::enable_if<!Req::isEverCached>::type * = nullptr>
    static bool isCached(AnyHolder holder) {
      return false;
    }
  };

  const uint64_t typeID;
  const size_t size;
  const std::function<void(AnyHolder, void *)> copy;
  const std::function<hash_code(AnyHolder)> getHash;
  const std::function<void(AnyHolder)> deleter;
  const std::function<bool(AnyHolder, AnyHolder)> isEqual;
  const std::function<void(AnyHolder, llvm::raw_ostream &)> simpleDisplay;
  const std::function<void(AnyHolder, DiagnosticEngine &)> diagnoseCycle;
  const std::function<void(AnyHolder, DiagnosticEngine &)> noteCycleStep;
  const std::function<SourceLoc(AnyHolder)> getNearestLoc;
  const std::function<bool(AnyHolder)> isCached;

  template <typename Request>
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
        &Impl<Request>::template isCached<Request>,
    };
    return &vtable;
  }
};

/// Base class for request type-erasing wrappers.
template <typename Derived>
class AnyRequestBase {
  template<typename T>
  friend class AnyRequestBase;

  friend llvm::DenseMapInfo<Derived>;

protected:
  static hash_code hashForHolder(uint64_t typeID, hash_code requestHash) {
    return hash_combine(typeID, requestHash);
  }

private:
  Derived &asDerived() {
    return *static_cast<Derived *>(this);
  }
  const Derived &asDerived() const {
    return *static_cast<const Derived *>(this);
  }

  const auto &getStorage() const { return asDerived().getStorage(); }

  const AnyRequestVTable *getVTable() const {
    return getStorage().getVTable();
  }

public:
  /// Cast to a specific (known) type.
  template<typename Request>
  const Request &castTo() const {
    return getStorage().template get<Request>();
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename Request>
  const Request *getAs() const {
    return getStorage().template getAs<Request>();
  }

  /// Diagnose a cycle detected for this request.
  void diagnoseCycle(DiagnosticEngine &diags) const {
    getVTable()->diagnoseCycle(getStorage(), diags);
  }

  /// Note that this request is part of a cycle.
  void noteCycleStep(DiagnosticEngine &diags) const {
    getVTable()->noteCycleStep(getStorage(), diags);
  }

  /// Retrieve the nearest source location to which this request applies.
  SourceLoc getNearestLoc() const {
    return getVTable()->getNearestLoc(getStorage());
  }

  bool isCached() const {
    return getVTable()->isCached(getStorage());
  }

  template <typename OtherDerived>
  bool isStorageEqual(const AnyRequestBase<OtherDerived> &other) const {
    return getStorage().isStorageEqual(other.getStorage());
  }

  /// Compare two instances for equality.
  friend bool operator==(const Derived &lhs, const Derived &rhs) {
    return lhs.getStorage() == rhs.getStorage();
  }

  friend bool operator!=(const Derived &lhs, const Derived &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const Derived &req) {
    return hash_value(req.getStorage());
  }

  friend void simple_display(llvm::raw_ostream &out, const Derived &req) {
    req.getVTable()->simpleDisplay(req.getStorage(), out);
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

  using Storage = SmallHolder<AnyRequestVTable>;
  Storage storage;

  explicit ActiveRequest(Storage &&empty) : storage(std::move(empty)) {
    assert(!storage.hasStorage());
  }
  const Storage &getStorage() const { return storage; }

public:
  /// Creates a new \c ActiveRequest referencing a concrete request on the
  /// stack.
  template <typename Request>
  explicit ActiveRequest(const Request &request)
      : storage(Storage::withUnownedRef(request)) {}

  static ActiveRequest getEmptyKey() {
    return ActiveRequest(Storage::getEmptyKey());
  }

  static ActiveRequest getTombstoneKey() {
    return ActiveRequest(Storage::getTombstoneKey());
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

  using Storage = SmallHolder<AnyRequestVTable>;
  Storage storage;

  explicit AnyRequest(Storage storage) : storage(std::move(storage)) {}

  bool hasStorage() const { return storage.hasStorage(); }
  const Storage &getStorage() const { return storage; }

public:
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
  explicit AnyRequest(T &&request) : storage(std::forward<T>(request)) {}

  /// Construct an \c AnyRequest from an \c ActiveRequest, allowing the
  /// underlying request to persist.
  explicit AnyRequest(const ActiveRequest &req)
      : storage(req.storage.makePersistant()) {}

  /// Return the result of calling simple_display as a string.
  std::string getAsString() const;

  static AnyRequest getEmptyKey() {
    return AnyRequest(Storage::getEmptyKey());
  }

  static AnyRequest getTombstoneKey() {
    return AnyRequest(Storage::getTombstoneKey());
  }
};

} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::ActiveRequest> {
    using ActiveRequest = swift::ActiveRequest;
    static inline ActiveRequest getEmptyKey() {
      return ActiveRequest::getEmptyKey();
    }
    static inline ActiveRequest getTombstoneKey() {
      return ActiveRequest::getTombstoneKey();
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
      return AnyRequest::getEmptyKey();
    }
    static inline swift::AnyRequest getTombstoneKey() {
      return AnyRequest::getTombstoneKey();
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
      if (!rhs.hasStorage())
        return false;

      return rhs.isStorageEqual(lhs);
    }
  };

} // end namespace llvm

#endif // SWIFT_AST_ANYREQUEST_H
