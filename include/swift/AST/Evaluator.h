//===--- Evaluator.h - Request Evaluator ------------------------*- C++ -*-===//
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
// This file defines the Evaluator class that evaluates and caches
// requests.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_EVALUATOR_H
#define SWIFT_AST_EVALUATOR_H

#include "swift/AST/AnyRequest.h"
#include "swift/Basic/AnyValue.h"
#include "swift/Basic/Defer.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include <tuple>
#include <type_traits>
#include <vector>

namespace swift {

using llvm::Optional;
using llvm::None;

class DiagnosticEngine;

/// Evaluation engine that evaluates and caches "requests", checking for cyclic
/// dependencies along the way.
///
/// Each request is a function object that accepts a reference to the evaluator
/// itself (through which it can request other values) and produces a
/// value. That value can then be cached by the evaluator for subsequent access,
/// using a policy dictated by the request itself.
///
/// The evaluator keeps track of all in-flight requests so that it can detect
/// and diagnose cyclic dependencies.
///
/// Each request should be its own function object, supporting the following
/// API:
///
///   - Copy constructor
///   - Equality operator (==)
///   - Hashing support (hash_value)
///   - TypeID support (see swift/Basic/TypeID.h)
///   - The output type (described via a nested type OutputType), which
///     must itself by a value type that supports TypeID.
///   - Evaluation via the function call operator:
///       OutputType operator()(Evaluator &evaluator) const;
///   - Cycle breaking and diagnostics operations:
///
///       void diagnoseCycle(DiagnosticEngine &diags) const;
///       void noteCycleStep(DiagnosticEngine &diags) const;
///       OutputType breakCycle() const;
///   - Caching policy:
///
///     static const bool isEverCached;
///
///       When false, the request's result will never be cached. Note that this
///       also precludes cycle detection.  When true, the result will be
///       cached on completion. How it is cached depends on the following.
///
///     bool isCached() const;
///
///       Dynamically indicates whether to cache this particular instance of the
///       request, so that (for example) requests for which a quick check
///       usually suffices can avoid caching a trivial result.
///
///     static const bool hasExternalCache;
///
///       When false, the results will be cached within the evaluator and
///       cannot be accessed except through the evaluator. This is the
///       best approach, because it ensures that all accesses to the result
///       are tracked.
///
///       When true, the request itself must provide an way to cache the
///       results, e.g., in some external data structure. External caching
///       should only be used when staging in the use of the evaluator into
///       existing mutable data structures; new computations should not depend
///       on it. Externally-cached requests must provide additional API:
///
///         Optional<OutputType> getCachedResult() const;
///
///           Retrieve the cached result, or \c None if there is no such
///           result.
///
///         void cacheResult(OutputType value) const;
///
///            Cache the given result.
///
///         bool isInFlight() const;
///
///           Whether we're already computing this value.
///
///         void setInFlight() const;
///
///           Indicate that we're about to compute this value. After this,
///           \c isInFlight() should return \c true until \c cacheResult()
///           is called.
class Evaluator {
  /// The diagnostics engine through which any cyclic-dependency
  /// diagnostics will be emitted.
  DiagnosticEngine &diags;

  /// A vector containing all of the active evaluation requests, which
  /// is treated as a stack.
  std::vector<AnyRequest> activeRequests;

  /// A cache that stores the results of requests.
  llvm::DenseMap<AnyRequest, Optional<AnyValue>> cache;

public:
  /// Construct a new evaluator that can emit cyclic-dependency
  /// diagnostics through the given diagnostics engine.
  Evaluator(DiagnosticEngine &diags);

  /// Evaluate the given request and produce its result,
  /// consulting/populating the cache as required.
  template<typename Request>
  typename Request::OutputType operator()(const Request &request) {
    return getResult(request);
  }

  /// Evaluate a set of requests and return their results as a tuple.
  ///
  /// Use this to describe cases where there are multiple (known)
  /// requests that all need to be satisfied.
  template<typename ...Requests>
  std::tuple<typename Requests::OutputType...>
  operator()(const Requests &...requests) {
    return std::tuple<typename Requests::OutputType...>((*this)(requests)...);
  }

private:
  /// Diagnose a cycle detected in the evaluation of the given
  /// request.
  void diagnoseCycle(const AnyRequest &request);

  /// Retrieve the result produced by evaluating a request that can
  /// be cached.
  template<typename Request,
           typename std::enable_if<Request::isEverCached>::type * = nullptr>
  typename Request::OutputType getResult(const Request &request) {
    // The request can be cached, but check a predicate to determine
    // whether this particular instance is cached. This allows more
    // fine-grained control over which instances get cache.
    if (request.isCached())
      return getResultCached(request);

    return getResultUncached(request);
  }

  /// Retrieve the result produced by evaluating a request that
  /// will never be cached.
  template<typename Request,
           typename std::enable_if<!Request::isEverCached>::type * = nullptr>
  typename Request::OutputType getResult(const Request &request) {
    return getResultUncached(request);
  }

  /// Produce the result of the request without caching.
  template<typename Request>
  typename Request::OutputType getResultUncached(const Request &request) {
    // Push this request onto the active-requests stack while we
    // evaluate it.
    activeRequests.push_back(request);
    SWIFT_DEFER {
      activeRequests.pop_back();
    };

    return request(*this);
  }

  /// Get the result of a request, consulting an external cache
  /// provided by the request to retrieve previously-computed results
  /// and detect recursion.
  template<typename Request,
           typename std::enable_if<Request::hasExternalCache>::type * = nullptr>
  typename Request::OutputType getResultCached(const Request &request) {
    // If the request is still in flight, diagnose it.
    if (request.isInFlight()) {
      diagnoseCycle(request);

      // Break the cache and cache the result, so we don't attempt
      // this operation again.
      request.cacheResult(request.breakCycle());
    }

    // If there is a cached result, return it.
    if (auto cached = request.getCachedResult())
      return *cached;

    // Push this request onto the active-requests stack while we
    // evaluate it.
    activeRequests.push_back(request);
    SWIFT_DEFER {
      activeRequests.pop_back();
    };

    // Compute the result.
    request.setInFlight();
    auto result = request(*this);

    // Cache it.
    request.cacheResult(result);

    // Return it.
    return result;
  }

  /// Get the result of a request, consulting the general cache to
  /// retrieve previously-computed results and detect recursion.
  template<
      typename Request,
      typename std::enable_if<!Request::hasExternalCache>::type * = nullptr>
  typename Request::OutputType getResultCached(const Request &request) {
    AnyRequest anyRequest{request};

    // Find an existing entry for this request in the cache, or create
    // an empty entry if none exists yet.
    auto cached = cache.insert({anyRequest, None});
    if (!cached.second) {
      // There was already an entry in the cache.

      // If the existing cache entry had no value, we have a
      // cycle. Diagnose the cycle and have the request break the
      // cycle by providing a placeholder value.
      if (!cached.first->second) {
        diagnoseCycle(anyRequest);
        cached.first->second = request.breakCycle();
      }

      // Return the cached value, which might be the placeholder above.
      return cached.first->second->castTo<typename Request::OutputType>();
    }

    // Push this request onto the active-requests stack while we
    // evaluate it.
    activeRequests.push_back(anyRequest);
    SWIFT_DEFER {
      activeRequests.pop_back();
    };

    // Evaluate the request.
    auto result = request(*this);

    // Cache the result.
    cache[anyRequest] = result;
    return result;
  }
};

} // end namespace evaluator

#endif // SWIFT_AST_EVALUATOR_H
