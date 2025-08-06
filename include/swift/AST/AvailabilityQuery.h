//===--- AvailabilityQuery.h - Swift Availability Query ASTs ----*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2025 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the availability query AST classes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_AVAILABILITY_QUERY_H
#define SWIFT_AST_AVAILABILITY_QUERY_H

#include "swift/AST/AvailabilityDomain.h"
#include "swift/AST/AvailabilityRange.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/Support/VersionTuple.h"

namespace swift {
class ASTContext;
class FuncDecl;

/// Represents the information needed to evaluate an `#if available` query
/// (either at runtime or compile-time).
class AvailabilityQuery final {
  AvailabilityDomain domain;
  std::optional<AvailabilityRange> primaryRange;
  std::optional<AvailabilityRange> variantRange;

  enum class ResultKind : uint8_t {
    /// The result of the query is true at compile-time.
    ConstTrue = 0,
    /// The result of the query is false at compile-time.
    ConstFalse = 1,
    /// The result of the query must be determined at runtime.
    Dynamic = 2,
  };
  ResultKind kind;

  bool unavailable;

  AvailabilityQuery(AvailabilityDomain domain, ResultKind kind,
                    bool isUnavailable,
                    const std::optional<AvailabilityRange> &primaryRange,
                    const std::optional<AvailabilityRange> &variantRange)
      : domain(domain), primaryRange(primaryRange), variantRange(variantRange),
        kind(kind), unavailable(isUnavailable) {};

public:
  /// Returns an `AvailabilityQuery` for a query that evaluates to true or
  /// false at compile-time.
  static AvailabilityQuery constant(AvailabilityDomain domain,
                                    bool isUnavailable, bool value) {
    return AvailabilityQuery(
        domain, value ? ResultKind::ConstTrue : ResultKind::ConstFalse,
        isUnavailable, std::nullopt, std::nullopt);
  }

  /// Returns an `AvailabilityQuery` for a query that evaluates to true or
  /// false at compile-time in the universal availability domain.
  static AvailabilityQuery universallyConstant(bool isUnavailable, bool value) {
    return AvailabilityQuery(AvailabilityDomain::forUniversal(),
                             value ? ResultKind::ConstTrue
                                   : ResultKind::ConstFalse,
                             isUnavailable, std::nullopt, std::nullopt);
  }

  /// Returns an `AvailabilityQuery` for a query that must be evaluated at
  /// runtime with the given arguments, which may be zero, one, or two version
  /// tuples that should be passed to the query function.
  static AvailabilityQuery
  dynamic(AvailabilityDomain domain, bool isUnavailable,
          const std::optional<AvailabilityRange> &primaryRange,
          const std::optional<AvailabilityRange> &variantRange) {
    return AvailabilityQuery(domain, ResultKind::Dynamic, isUnavailable,
                             primaryRange, variantRange);
  }

  /// Returns the domain that the query applies to.
  AvailabilityDomain getDomain() const { return domain; }

  /// Returns true if the query's result is determined at compile-time.
  bool isConstant() const { return kind != ResultKind::Dynamic; }

  /// Returns true if the query was spelled `#unavailable`.
  bool isUnavailability() const { return unavailable; }

  /// Returns the boolean result of the query if it is known at compile-time, or
  /// `std::nullopt` otherwise. The returned value accounts for whether the
  /// query was spelled `#unavailable`.
  std::optional<bool> getConstantResult() const {
    switch (kind) {
    case ResultKind::ConstTrue:
      return !unavailable;
    case ResultKind::ConstFalse:
      return unavailable;
    case ResultKind::Dynamic:
      return std::nullopt;
    }
  }

  /// Returns the availability range that is the first argument to query
  /// function.
  std::optional<AvailabilityRange> getPrimaryRange() const {
    return primaryRange;
  }

  /// Returns the version tuple that is the first argument to query function.
  std::optional<llvm::VersionTuple> getPrimaryArgument() const {
    if (!primaryRange)
      return std::nullopt;
    return primaryRange->getRawMinimumVersion();
  }

  /// Returns the availability range that is the second argument to query
  /// function. This represents the `-target-variant` version when compiling a
  /// zippered library.
  std::optional<AvailabilityRange> getVariantRange() const {
    return variantRange;
  }

  /// Returns the version tuple that is the second argument to query function.
  /// This represents the `-target-variant` version when compiling a zippered
  /// library.
  std::optional<llvm::VersionTuple> getVariantArgument() const {
    if (!variantRange)
      return std::nullopt;
    return variantRange->getRawMinimumVersion();
  }

  /// Returns the `FuncDecl *` that should be invoked at runtime to evaluate
  /// the query, and populates `arguments` with the arguments to invoke it with
  /// (the integer components of the version tuples that are being tested). If
  /// the query does not have a dynamic result, returns `nullptr`.
  FuncDecl *
  getDynamicQueryDeclAndArguments(llvm::SmallVectorImpl<unsigned> &arguments,
                                  ASTContext &ctx) const;
};

} // end namespace swift

#endif
