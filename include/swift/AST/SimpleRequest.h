//===--- SimpleRequest.h - Simple Request Instances -------------*- C++ -*-===//
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
//  This file defines the SimpleRequest class template, which makes it easier
//  to define new request kinds.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_AST_SIMPLEREQUEST_H
#define SWIFT_AST_SIMPLEREQUEST_H

#include "swift/AST/DiagnosticEngine.h"
#include "swift/AST/DiagnosticsCommon.h"
#include "swift/Basic/SimpleDisplay.h"
#include "swift/Basic/Statistic.h"
#include "swift/Basic/TypeID.h"
#include "llvm/ADT/Hashing.h"
#include "llvm/ADT/STLExtras.h"
#include <tuple>
#include <type_traits>

namespace swift {

class Evaluator;

/// Describes how the result for a particular request will be cached.
enum class CacheKind {
  /// The result for a particular request should never be cached.
  Uncached,
  /// The result for a particular request should be cached within the
  /// evaluator itself.
  Cached,
  /// The result of a particular request will be cached via some separate
  /// mechanism, such as a mutable data structure.
  SeparatelyCached,
};

/// -------------------------------------------------------------------------
/// Extracting the source location "nearest" a request.
/// -------------------------------------------------------------------------

namespace detail {
  /// Dummy extract function used to detect when we can call
  /// extractNearestSourceLoc() safely.
  inline void extractNearestSourceLoc(...) { }

  /// Metaprogram to determine whether any input is true.
  constexpr bool anyTrue() { return false; }

  template<typename ...Rest>
  constexpr bool anyTrue(bool current, Rest... rest) {
    return current || anyTrue(rest...);
  }
}

/// Determine whether we can extract a nearest source location from a value of
/// the given type.
template<typename T>
constexpr bool canExtractNearestSourceLoc() {
  using detail::extractNearestSourceLoc;
  return !std::is_void<decltype(extractNearestSourceLoc(*(T*)nullptr))>::value;
}

/// Extract source locations when possible, or return an invalid source
/// location if not possible.
template<typename T,
         typename = typename std::enable_if<
             canExtractNearestSourceLoc<T>()>::type>
SourceLoc maybeExtractNearestSourceLoc(const T& value) {
  return extractNearestSourceLoc(value);
}

template<typename T,
         typename = void,
         typename = typename std::enable_if<
             !canExtractNearestSourceLoc<T>()>::type>
SourceLoc maybeExtractNearestSourceLoc(const T& value) {
  return SourceLoc();
}

/// Extract the nearest source location from a pointer union.
template<typename T, typename U,
          typename = typename std::enable_if<
            canExtractNearestSourceLoc<T>() &&
            canExtractNearestSourceLoc<U>()>::type>
SourceLoc extractNearestSourceLoc(const llvm::PointerUnion<T, U> &value) {
  if (auto first = value.template dyn_cast<T>()) {
    return extractNearestSourceLoc(first);
  }
  if (auto second = value.template dyn_cast<U>()) {
    return extractNearestSourceLoc(second);
  }
  return SourceLoc();
}

namespace detail {
  /// Basis case for extracting the nearest source location from a tuple.
  template<unsigned Index, typename ...Types,
           typename = void,
           typename = typename std::enable_if<
               (Index >= sizeof...(Types))>::type>
  SourceLoc extractNearestSourceLocTuple(const std::tuple<Types...> &) {
    return SourceLoc();
  }

  /// Extract the first, nearest source location from a tuple.
  template<unsigned Index, typename ...Types,
           typename = typename std::enable_if<(Index < sizeof...(Types))>::type>
  SourceLoc extractNearestSourceLocTuple(const std::tuple<Types...> &value) {
    SourceLoc loc = maybeExtractNearestSourceLoc(std::get<Index>(value));
    if (loc.isValid())
      return loc;

    return extractNearestSourceLocTuple<Index + 1>(value);
  }
}

/// Extract the first, nearest source location from a tuple.
template<typename First, typename ...Rest,
         typename = typename std::enable_if<
             detail::anyTrue(canExtractNearestSourceLoc<First>(),
                             canExtractNearestSourceLoc<Rest>()...)>::type>
SourceLoc extractNearestSourceLoc(const std::tuple<First, Rest...> &value) {
  return detail::extractNearestSourceLocTuple<0>(value);
}

/// -------------------------------------------------------------------------
/// Simple Requests
/// -------------------------------------------------------------------------

/// CRTP base class that describes a request operation that takes values
/// with the given input types (\c Inputs...) and produces an output of
/// the given type.
///
/// \tparam Derived The final, derived class type for the request.
/// \tparam Signature The signature of the request, described as a
/// function type whose inputs (\c Inputs) are the parameter types and
/// whose output (\c Output) is the result of evaluating this request.
/// \tparam Caching Describes how the output value is cached, if at all.
///
/// The \c Derived class needs to implement several operations. The most
/// important one takes an evaluator and the input values, then computes the
/// final result, optionally bubbling up errors from recursive evaulations:
/// \code
///   llvm::Expected<Output> evaluate(Evaluator &evaluator, Inputs...) const;
/// \endcode
///
/// Cycle diagnostics can be handled in one of two ways. Either the \c Derived
/// class can implement the two cycle-diagnosing operations directly:
/// \code
///   void diagnoseCycle(DiagnosticEngine &diags) const;
///   void noteCycleStep(DiagnosticEngine &diags) const;
/// \endcode
///
/// Or the implementation will use the default "circular reference" diagnostics
/// based on the "nearest" source location, which can be provided explicitly by
/// implementing the following in the subclass:
/// \code
///   SourceLoc getNearestLoc() const;
/// \endcode
/// If not provided, a default implementation for \c getNearestLoc() will pick
/// the source location from the first input that provides one.
///
/// Value caching is determined by the \c Caching parameter. When
/// \c Caching == CacheKind::SeparatelyCached, the \c Derived class is
/// responsible for implementing the two operations responsible to managing
/// the cache:
/// \code
///   Optional<Output> getCachedResult() const;
///   void cacheResult(Output value) const;
/// \endcode
template<typename Derived, typename Signature, CacheKind Caching>
class SimpleRequest;

template<typename Derived, CacheKind Caching, typename Output,
         typename ...Inputs>
class SimpleRequest<Derived, Output(Inputs...), Caching> {
  std::tuple<Inputs...> storage;

  Derived &asDerived() {
    return *static_cast<Derived *>(this);
  }

  const Derived &asDerived() const {
    return *static_cast<const Derived *>(this);
  }

  template<size_t ...Indices>
  llvm::Expected<Output>
  callDerived(Evaluator &evaluator, llvm::index_sequence<Indices...>) const {
    static_assert(sizeof...(Indices) > 0, "Subclass must define evaluate()");
    return asDerived().evaluate(evaluator, std::get<Indices>(storage)...);
  }

protected:
  /// Retrieve the storage value directly.
  const std::tuple<Inputs...> &getStorage() const { return storage; }

public:
  static const bool isEverCached = (Caching != CacheKind::Uncached);
  static const bool hasExternalCache = (Caching == CacheKind::SeparatelyCached);

  using OutputType = Output;
  
  explicit SimpleRequest(const Inputs& ...inputs)
    : storage(inputs...) { }

  /// Request evaluation function that will be registered with the evaluator.
  static llvm::Expected<OutputType>
  evaluateRequest(const Derived &request, Evaluator &evaluator) {
    return request.callDerived(evaluator,
                               llvm::index_sequence_for<Inputs...>());
  }

  /// Retrieve the nearest source location to which this request applies.
  SourceLoc getNearestLoc() const {
    return extractNearestSourceLoc(storage);
  }

  void diagnoseCycle(DiagnosticEngine &diags) const {
    diags.diagnose(asDerived().getNearestLoc(), diag::circular_reference);
  }

  void noteCycleStep(DiagnosticEngine &diags) const {
    diags.diagnose(asDerived().getNearestLoc(),
                   diag::circular_reference_through);
  }

  friend bool operator==(const SimpleRequest &lhs, const SimpleRequest &rhs) {
    return lhs.storage == rhs.storage;
  }

  friend bool operator!=(const SimpleRequest &lhs, const SimpleRequest &rhs) {
    return !(lhs == rhs);
  }

  friend llvm::hash_code hash_value(const SimpleRequest &request) {
    using llvm::hash_combine;

    return hash_combine(TypeID<Derived>::value, request.storage);
  }

  friend void simple_display(llvm::raw_ostream &out,
                             const Derived &request) {
    out << TypeID<Derived>::getName();
    simple_display(out, request.storage);
  }

  friend FrontendStatsTracer
  make_tracer(UnifiedStatsReporter *Reporter, const Derived &request) {
    return make_tracer(Reporter, TypeID<Derived>::getName(), request.storage);
  }
};
}

namespace llvm {
  template <typename T, unsigned N>
  llvm::hash_code hash_value(const SmallVector<T, N> &vec) {
    return hash_combine_range(vec.begin(), vec.end());
  }
}

#endif // SWIFT_BASIC_SIMPLEREQUEST_H
