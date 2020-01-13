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

#include "swift/AST/SmallHolder.h"
#include "swift/Basic/SourceLoc.h"
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
  };

  const uint64_t typeID;
  const std::function<void(AnyHolder, void *)> copy;
  const std::function<hash_code(AnyHolder)> getHash;
  const std::function<void(AnyHolder)> deleter;
  const std::function<bool(AnyHolder, const AnyHolder &)> isEqual;
  const std::function<void(AnyHolder, llvm::raw_ostream &)> simpleDisplay;
  const std::function<void(AnyHolder, DiagnosticEngine &)> diagnoseCycle;
  const std::function<void(AnyHolder, DiagnosticEngine &)> noteCycleStep;
  const std::function<SourceLoc(AnyHolder)> getNearestLoc;

  template <typename Request>
  static const AnyRequestVTable *get() {
    static const AnyRequestVTable vtable = {
        TypeID<Request>::value,
        &Impl<Request>::copy,
        &Impl<Request>::getHash,
        &Impl<Request>::deleter,
        &Impl<Request>::isEqual,
        &Impl<Request>::simpleDisplay,
        &Impl<Request>::diagnoseCycle,
        &Impl<Request>::noteCycleStep,
        &Impl<Request>::getNearestLoc,
    };
    return &vtable;
  }
};

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
  friend llvm::DenseMapInfo<AnyRequest>;

  /// The data stored in this value.
  using Storage = SmallHolder<8, alignof(void *), AnyRequestVTable>;
  Storage stored;

  explicit AnyRequest(Storage stored) : stored(stored) {}

public:
  AnyRequest(const AnyRequest &other) = default;
  AnyRequest &operator=(const AnyRequest &other) = default;
  AnyRequest(AnyRequest &&other) = default;
  AnyRequest &operator=(AnyRequest &&other) = default;

  // Create a local template typename `ValueType` in the template specialization
  // so that we can refer to it in the SFINAE condition as well as the body of
  // the template itself.  The SFINAE condition allows us to remove this
  // constructor from candidacy when evaluating explicit construction with an
  // instance of `AnyRequest`.  If we do not do so, we will find ourselves with
  // ambiguity with this constructor and the defined move constructor above.
  /// Construct a new instance with the given value.
  template <typename T, typename ValueType = std::decay_t<T>,
            typename = typename std::enable_if<
                !std::is_same<ValueType, AnyRequest>::value>::type>
  explicit AnyRequest(T &&value) : stored(Storage(std::forward<T>(value))) {}

  /// Cast to a specific (known) type.
  template<typename Request>
  const Request &castTo() const {
    return *stored.get<Request>();
  }

  /// Try casting to a specific (known) type, returning \c nullptr on
  /// failure.
  template<typename Request>
  const Request *getAs() const {
    return stored.getAs<Request>();
  }

  /// Diagnose a cycle detected for this request.
  void diagnoseCycle(DiagnosticEngine &diags) const {
    stored.getVTable()->diagnoseCycle(stored, diags);
  }

  /// Note that this request is part of a cycle.
  void noteCycleStep(DiagnosticEngine &diags) const {
    stored.getVTable()->noteCycleStep(stored, diags);
  }

  /// Retrieve the nearest source location to which this request applies.
  SourceLoc getNearestLoc() const {
    return stored.getVTable()->getNearestLoc(stored);
  }

  /// Compare two instances for equality.
  friend bool operator==(const AnyRequest &lhs, const AnyRequest &rhs) {
    return lhs.stored == rhs.stored;
  }

  friend bool operator!=(const AnyRequest &lhs, const AnyRequest &rhs) {
    return !(lhs == rhs);
  }

  friend hash_code hash_value(const AnyRequest &req) {
    return hash_value(req.stored);
  }

  friend void simple_display(llvm::raw_ostream &out, const AnyRequest &req) {
    req.stored.getVTable()->simpleDisplay(req.stored, out);
  }

  /// Return the result of calling simple_display as a string.
  std::string getAsString() const;
};

} // end namespace swift

namespace llvm {
  template<>
  struct DenseMapInfo<swift::AnyRequest> {
    using AnyRequest = swift::AnyRequest;
    static inline AnyRequest getEmptyKey() {
      return AnyRequest(AnyRequest::Storage::getEmptyKey());
    }
    static inline AnyRequest getTombstoneKey() {
      return AnyRequest(AnyRequest::Storage::getTombstoneKey());
    }
    static unsigned getHashValue(const AnyRequest &request) {
      return hash_value(request);
    }
    template <typename Request>
    static unsigned getHashValue(const Request &request) {
      return DenseMapInfo<AnyRequest::Storage>::getHashValue(request);
    }
    static bool isEqual(const AnyRequest &lhs, const AnyRequest &rhs) {
      return lhs == rhs;
    }
    template <typename Request>
    static bool isEqual(const Request &lhs, const AnyRequest &rhs) {
      return DenseMapInfo<AnyRequest::Storage>::isEqual(lhs, rhs.stored);
    }
  };

} // end namespace llvm

#endif // SWIFT_AST_ANYREQUEST_H
