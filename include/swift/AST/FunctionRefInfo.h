//===--- FunctionRefInfo.h - Function reference info ------------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2024 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the FunctionRefInfo class, which describes how a function
// is referenced in an expression.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_FUNCTION_REF_INFO_H
#define SWIFT_AST_FUNCTION_REF_INFO_H

#include "swift/Basic/Debug.h"
#include "swift/Basic/LLVM.h"

#include <cstdint>

namespace swift {

class DeclNameLoc;
class DeclNameRef;

/// Describes how a function is referenced within an expression node.
///
/// This dictates things like:
/// - Whether argument labels are part of the resulting function type or not.
/// - Whether the result can produce an implicitly unwrapped optional.
/// - Whether the function type needs adjustment for concurrency.
///
/// How a function is referenced comes down to how it was spelled in
/// the source code, e.g., was it called in the source code and was it
/// spelled as a compound name.
class FunctionRefInfo final {
public:
  /// Whether the function reference is part of a call, and if so how many
  /// applications were used.
  enum class ApplyLevel : uint8_t {
    /// The function is not directly called.
    Unapplied,
    /// The function is directly applied once, e.g., "f(a: 1, b: 2)".
    SingleApply,
    /// The function is directly applied two or more times, e.g., "g(x)(y)".
    DoubleApply,
  };

private:
  /// The application level of the function reference.
  ApplyLevel ApplyLevelKind;

  /// Whether the function was referenced using a compound function name,
  /// e.g., "f(a:b:)".
  bool IsCompoundName;

  FunctionRefInfo(ApplyLevel applyLevel, bool isCompoundName)
      : ApplyLevelKind(applyLevel), IsCompoundName(isCompoundName) {}

public:
  /// An unapplied function reference for a given DeclNameLoc.
  static FunctionRefInfo unapplied(DeclNameLoc nameLoc);

  /// An unapplied function reference for a given DeclNameRef.
  static FunctionRefInfo unapplied(DeclNameRef nameRef);

  /// An unapplied function reference using a base name, e.g `let x = fn`.
  static FunctionRefInfo unappliedBaseName() {
    return FunctionRefInfo(ApplyLevel::Unapplied, /*isCompoundName*/ false);
  }

  /// An unapplied function reference using a compound name,
  /// e.g `let x = fn(x:)`.
  static FunctionRefInfo unappliedCompoundName() {
    return FunctionRefInfo(ApplyLevel::Unapplied, /*isCompoundName*/ true);
  }

  /// A single application using a base name, e.g `fn(x: 0)`.
  static FunctionRefInfo singleBaseNameApply() {
    return FunctionRefInfo(ApplyLevel::SingleApply, /*isCompoundName*/ false);
  }

  /// A single application using a compound name, e.g `fn(x:)(0)`.
  static FunctionRefInfo singleCompoundNameApply() {
    return FunctionRefInfo(ApplyLevel::SingleApply, /*isCompoundName*/ true);
  }

  /// A double application using a base name, e.g `S.fn(S())(x: 0)`.
  static FunctionRefInfo doubleBaseNameApply() {
    return FunctionRefInfo(ApplyLevel::DoubleApply, /*isCompoundName*/ false);
  }

  /// A double application using a compound name, e.g `S.fn(x:)(S())(0)`.
  static FunctionRefInfo doubleCompoundNameApply() {
    return FunctionRefInfo(ApplyLevel::DoubleApply, /*isCompoundName*/ true);
  }

  /// Reconstructs a FunctionRefInfo from its \c getOpaqueValue().
  static FunctionRefInfo fromOpaque(uint8_t bits) {
    return FunctionRefInfo(static_cast<ApplyLevel>(bits >> 1), bits & 0x1);
  }

  /// Retrieves an opaque value that can be stored in e.g a bitfield.
  uint8_t getOpaqueValue() const {
    return (static_cast<uint8_t>(ApplyLevelKind) << 1) | !!IsCompoundName;
  }

  /// Whether the function reference is part of a call, and if so how many
  /// applications were used.
  ApplyLevel getApplyLevel() const { return ApplyLevelKind; }

  /// Whether the function was referenced using a compound name,
  /// e.g `fn(x:)(0)`.
  bool isCompoundName() const { return IsCompoundName; }

  /// Whether the function reference is not part of a call.
  bool isUnapplied() const {
    return getApplyLevel() == ApplyLevel::Unapplied;
  }

  /// Whether the function reference is both not part of a call, and is
  /// not using a compound name.
  bool isUnappliedBaseName() const {
    return getApplyLevel() == ApplyLevel::Unapplied && !isCompoundName();
  }

  /// Whether the function reference has been applied a single time.
  bool isSingleApply() const {
    return getApplyLevel() == ApplyLevel::SingleApply;
  }

  /// Whether the function reference has been applied twice.
  bool isDoubleApply() const {
    return getApplyLevel() == ApplyLevel::DoubleApply;
  }

  /// Returns the FunctionRefInfo with an additional level of function
  /// application added.
  FunctionRefInfo addingApplicationLevel() const;

  friend bool operator==(const FunctionRefInfo &lhs,
                         const FunctionRefInfo &rhs) {
    return lhs.getApplyLevel() == rhs.getApplyLevel() &&
           lhs.isCompoundName() == rhs.isCompoundName();
  }
  friend bool operator!=(const FunctionRefInfo &lhs,
                         const FunctionRefInfo &rhs) {
    return !(lhs == rhs);
  }

  void dump(raw_ostream &os) const;
  SWIFT_DEBUG_DUMP;
};
}

#endif // SWIFT_AST_FUNCTION_REF_INFO_H
