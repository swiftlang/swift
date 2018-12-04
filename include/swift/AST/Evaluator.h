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
#include "swift/Basic/CycleDiagnosticKind.h"
#include "swift/Basic/Defer.h"
#include "swift/Basic/Statistic.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/Optional.h"
#include "llvm/ADT/SetVector.h"
#include "llvm/Support/Error.h"
#include "llvm/Support/PrettyStackTrace.h"
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

namespace llvm {
class raw_ostream;
}

namespace swift {

using llvm::ArrayRef;
using llvm::Optional;
using llvm::None;

class DiagnosticEngine;
class Evaluator;
class UnifiedStatsReporter;

/// An "abstract" request function pointer, which is the storage type
/// used for each of the
using AbstractRequestFunction = void(void);

/// Form the specific request function for the given request type.
template<typename Request>
using RequestFunction =
  llvm::Expected<typename Request::OutputType>(const Request &, Evaluator &);

/// Pretty stack trace handler for an arbitrary request.
template<typename Request>
class PrettyStackTraceRequest : public llvm::PrettyStackTraceEntry {
  const Request &request;

public:
  PrettyStackTraceRequest(const Request &request) : request(request) { }

  void print(llvm::raw_ostream &out) const {
    out << "While evaluating request ";
    simple_display(out, request);
    out << "\n";
  }
};

/// An llvm::ErrorInfo container for a request in which a cycle was detected
/// and diagnosed.
template <typename Request>
struct CyclicalRequestError : 
  public llvm::ErrorInfo<CyclicalRequestError<Request>> {
public:
  static char ID;
  const Request &request;
  const Evaluator &evaluator;

  CyclicalRequestError(const Request &request, const Evaluator &evaluator)
    : request(request), evaluator(evaluator) {}

  virtual void log(llvm::raw_ostream &out) const;

  virtual std::error_code convertToErrorCode() const {
    // This is essentially unused, but is a temporary requirement for
    // llvm::ErrorInfo subclasses.
    llvm_unreachable("shouldn't get std::error_code from CyclicalRequestError");
  }
};

template <typename Request>
char CyclicalRequestError<Request>::ID = '\0';

/// Evaluates a given request or returns a default value if a cycle is detected.
template <typename Request>
typename Request::OutputType
evaluateOrDefault(
  Evaluator &eval, Request req, typename Request::OutputType def) {
  auto result = eval(req);
  if (auto err = result.takeError()) {
    llvm::handleAllErrors(std::move(err),
      [](const CyclicalRequestError<Request> &E) {
        // cycle detected
      });
    return def;
  }
  return *result;
}

/// Report that a request of the given kind is being evaluated, so it
/// can be recorded by the stats reporter.
template<typename Request>
void reportEvaluatedRequest(UnifiedStatsReporter &stats,
                            const Request &request) { }

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
///   - Caching policy:
///
///     static const bool isEverCached;
///
///       When false, the request's result will never be cached. When true,
///       the result will be cached on completion. How it is cached depends on
///       the following.
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
class Evaluator {
  /// The diagnostics engine through which any cyclic-dependency
  /// diagnostics will be emitted.
  DiagnosticEngine &diags;

  /// Whether to diagnose cycles or ignore them completely.
  CycleDiagnosticKind shouldDiagnoseCycles;

  /// Used to report statistics about which requests were evaluated, if
  /// non-null.
  UnifiedStatsReporter *stats = nullptr;

  /// A vector containing the abstract request functions that can compute
  /// the result of a particular request within a given zone. The
  /// \c uint8_t is the zone number of the request, and the array is
  /// indexed by the index of the request type within that zone. Each
  /// entry is a function pointer that will be reinterpret_cast'd to
  ///
  ///   RequestType::OutputType (*)(const RequestType &request,
  ///                               Evaluator &evaluator);
  /// and called to satisfy the request.
  std::vector<std::pair<uint8_t, ArrayRef<AbstractRequestFunction *>>>
    requestFunctionsByZone;

  /// A vector containing all of the active evaluation requests, which
  /// is treated as a stack and is used to detect cycles.
  llvm::SetVector<AnyRequest> activeRequests;

  /// A cache that stores the results of requests.
  llvm::DenseMap<AnyRequest, AnyValue> cache;

  /// Track the dependencies of each request.
  ///
  /// This is an adjacency-list representation expressing, for each known
  /// request, the requests that it directly depends on. It is populated
  /// lazily while the request is being evaluated.
  ///
  /// In a well-formed program, the graph should be a directed acycle graph
  /// (DAG). However, cyclic dependencies will be recorded within this graph,
  /// so all clients must cope with cycles.
  llvm::DenseMap<AnyRequest, std::vector<AnyRequest>> dependencies;

  /// Retrieve the request function for the given zone and request IDs.
  AbstractRequestFunction *getAbstractRequestFunction(uint8_t zoneID,
                                                      uint8_t requestID) const;

  /// Retrieve the request function for the given request type.
  template<typename Request>
  auto getRequestFunction() const -> RequestFunction<Request> * {
    auto abstractFn = getAbstractRequestFunction(TypeID<Request>::zoneID,
                                                 TypeID<Request>::localID);
    assert(abstractFn && "No request function for request");
    return reinterpret_cast<RequestFunction<Request> *>(abstractFn);
  }

public:
  /// Construct a new evaluator that can emit cyclic-dependency
  /// diagnostics through the given diagnostics engine.
  Evaluator(DiagnosticEngine &diags, CycleDiagnosticKind shouldDiagnoseCycles);

  /// Emit GraphViz output visualizing the request graph.
  void emitRequestEvaluatorGraphViz(llvm::StringRef graphVizPath);

  /// Set the unified stats reporter through which evaluated-request
  /// statistics will be recorded.
  void setStatsReporter(UnifiedStatsReporter *stats) { this->stats = stats; }

  /// Register the set of request functions for the given zone.
  ///
  /// These functions will be called to evaluate any requests within that
  /// zone.
  void registerRequestFunctions(uint8_t zoneID,
                                ArrayRef<AbstractRequestFunction *> functions);

  /// Evaluate the given request and produce its result,
  /// consulting/populating the cache as required.
  template<typename Request>
  llvm::Expected<typename Request::OutputType>
  operator()(const Request &request) {
    // Check for a cycle.
    if (checkDependency(getCanonicalRequest(request))) {
      return llvm::Error(
        llvm::make_unique<CyclicalRequestError<Request>>(request, *this));
    }

    // Make sure we remove this from the set of active requests once we're
    // done.
    SWIFT_DEFER {
      assert(activeRequests.back().castTo<Request>() == request);
      activeRequests.pop_back();
    };

    // Get the result.
    return getResult(request);
  }

  /// Evaluate a set of requests and return their results as a tuple.
  ///
  /// Use this to describe cases where there are multiple (known)
  /// requests that all need to be satisfied.
  template<typename ...Requests>
  std::tuple<llvm::Expected<typename Requests::OutputType>...>
  operator()(const Requests &...requests) {
    return std::tuple<llvm::Expected<typename Requests::OutputType>...>(
      (*this)(requests)...);
  }

  /// Clear the cache stored within this evaluator.
  ///
  /// Note that this does not clear the caches of requests that use external
  /// caching.
  void clearCache() { cache.clear(); }

private:
  template <typename Request>
  const AnyRequest &getCanonicalRequest(const Request &request) {
    // FIXME: DenseMap ought to let us do this with one hash lookup.
    auto iter = dependencies.find_as(request);
    if (iter != dependencies.end())
      return iter->first;
    auto insertResult = dependencies.insert({AnyRequest(request), {}});
    assert(insertResult.second && "just checked if the key was already there");
    return insertResult.first->first;
  }

  /// Diagnose a cycle detected in the evaluation of the given
  /// request.
  void diagnoseCycle(const AnyRequest &request);

  /// Check the dependency from the current top of the stack to
  /// the given request, including cycle detection and diagnostics.
  ///
  /// \returns true if a cycle was detected, in which case this function has
  /// already diagnosed the cycle. Otherwise, returns \c false and adds this
  /// request to the \c activeRequests stack.
  bool checkDependency(const AnyRequest &request);

  /// Retrieve the result produced by evaluating a request that can
  /// be cached.
  template<typename Request,
           typename std::enable_if<Request::isEverCached>::type * = nullptr>
  llvm::Expected<typename Request::OutputType>
  getResult(const Request &request) {
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
  llvm::Expected<typename Request::OutputType>
  getResult(const Request &request) {
    return getResultUncached(request);
  }

  /// Produce the result of the request without caching.
  template<typename Request>
  llvm::Expected<typename Request::OutputType>
  getResultUncached(const Request &request) {
    // Clear out the dependencies on this request; we're going to recompute
    // them now anyway.
    dependencies.find_as(request)->second.clear();

    PrettyStackTraceRequest<Request> prettyStackTrace(request);

    // Trace and/or count statistics.
    FrontendStatsTracer statsTracer = make_tracer(stats, request);
    if (stats) reportEvaluatedRequest(*stats, request);

    return getRequestFunction<Request>()(request, *this);
  }

  /// Get the result of a request, consulting an external cache
  /// provided by the request to retrieve previously-computed results
  /// and detect recursion.
  template<typename Request,
           typename std::enable_if<Request::hasExternalCache>::type * = nullptr>
  llvm::Expected<typename Request::OutputType>
  getResultCached(const Request &request) {
    // If there is a cached result, return it.
    if (auto cached = request.getCachedResult())
      return *cached;

    // Compute the result.
    auto result = getResultUncached(request);

    // Cache the result if applicable.
    if (!result)
      return result;

    request.cacheResult(*result);

    // Return it.
    return result;
  }

  /// Get the result of a request, consulting the general cache to
  /// retrieve previously-computed results and detect recursion.
  template<
      typename Request,
      typename std::enable_if<!Request::hasExternalCache>::type * = nullptr>
  llvm::Expected<typename Request::OutputType>
  getResultCached(const Request &request) {
    // If we already have an entry for this request in the cache, return it.
    auto known = cache.find_as(request);
    if (known != cache.end()) {
      return known->second.template castTo<typename Request::OutputType>();
    }

    // Compute the result.
    auto result = getResultUncached(request);
    if (!result)
      return result;

    // Cache the result.
    cache.insert({getCanonicalRequest(request), *result});
    return result;
  }

public:
  /// Print the dependencies of the given request as a tree.
  ///
  /// This is the core printing operation; most callers will want to use
  /// the other overload.
  void printDependencies(const AnyRequest &request,
                         llvm::raw_ostream &out,
                         llvm::DenseSet<AnyRequest> &visitedAnywhere,
                         llvm::SmallVectorImpl<AnyRequest> &visitedAlongPath,
                         ArrayRef<AnyRequest> highlightPath,
                         std::string &prefixStr,
                         bool lastChild) const;

  /// Print the dependencies of the given request as a tree.
  template <typename Request>
  void printDependencies(const Request &request, llvm::raw_ostream &out) const {
    std::string prefixStr;
    llvm::DenseSet<AnyRequest> visitedAnywhere;
    llvm::SmallVector<AnyRequest, 4> visitedAlongPath;
    printDependencies(AnyRequest(request), out, visitedAnywhere,
                      visitedAlongPath, { }, prefixStr, /*lastChild=*/true);
  }

  /// Dump the dependencies of the given request to the debugging stream
  /// as a tree.
  LLVM_ATTRIBUTE_DEPRECATED(
    void dumpDependencies(const AnyRequest &request) const LLVM_ATTRIBUTE_USED,
    "Only meant for use in the debugger");

  /// Print all dependencies known to the evaluator as a single Graphviz
  /// directed graph.
  void printDependenciesGraphviz(llvm::raw_ostream &out) const;

  LLVM_ATTRIBUTE_DEPRECATED(
    void dumpDependenciesGraphviz() const LLVM_ATTRIBUTE_USED,
    "Only meant for use in the debugger");
};

template <typename Request>
void CyclicalRequestError<Request>::log(llvm::raw_ostream &out) const {
  out << "Cycle detected:\n";
  evaluator.printDependencies(request, out);
  out << "\n";
}

} // end namespace evaluator

#endif // SWIFT_AST_EVALUATOR_H
