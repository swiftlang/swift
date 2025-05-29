//===--- ThrownErrorDestination.h - Thrown error dest -----------*- C++ -*-===//
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
// This file defines the ThrownErrorDestination class.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_THROWNERRORDESTINATION
#define SWIFT_AST_THROWNERRORDESTINATION

#include "swift/AST/Type.h"
#include "swift/AST/TypeAlignments.h"
#include "llvm/ADT/PointerUnion.h"
#include <optional>

namespace swift {
class Expr;
class OpaqueValueExpr;

/// Describes an error thrown from a particular operation and its conversion
/// to the error type expected by its context.
class ThrownErrorDestination {
  struct Conversion {
    /// The opaque value, which is of the thrown error type.
    OpaqueValueExpr *thrownError;

    /// The conversion from the opaque value type to the type needed by the
    /// context.
    Expr *conversion;
  };

  llvm::PointerUnion<TypeBase *, Conversion *> storage;

  ThrownErrorDestination(Type type) : storage(type.getPointer()) { }
  ThrownErrorDestination(Conversion *conversion) : storage(conversion) { }

public:
  /// Construct a non-throwing destination.
  ThrownErrorDestination() : storage(nullptr) { }

  /// Construct a non-throwing destination.
  ThrownErrorDestination(std::nullptr_t) : storage(nullptr) { }

  /// Whether there is a thrown error destination at all.
  explicit operator bool() const { return !storage.isNull(); }

  /// A thrown error destination where the thrown type corresponds to the
  /// type caught (or rethrows) by the enclosing context.
  static ThrownErrorDestination forMatchingContextType(Type thrownError) {
    return ThrownErrorDestination(thrownError);
  }

  /// A thrown error destination where the thrown error requires a conversion
  /// to the error type expected by its context.
  static ThrownErrorDestination forConversion(OpaqueValueExpr *thrownError,
                                              Expr *conversion);

  /// Retrieve the type of error thrown from this function call.
  Type getThrownErrorType() const;

  /// Retrieve the type of the error expected in this context.
  Type getContextErrorType() const;

  /// Retrieve the conversion as a pair of (opaque thrown error value,
  /// conversion expression), when a conversion from the thrown error type
  /// to the context error type is required.
  std::optional<std::pair<OpaqueValueExpr *, Expr *>> getConversion() const {
    if (auto conversion = storage.dyn_cast<Conversion *>()) {
      return std::make_pair(conversion->thrownError, conversion->conversion);
    }

    return std::nullopt;
  }
};

} // end namespace swift

#endif // SWIFT_AST_THROWNERRORDESTINATION
