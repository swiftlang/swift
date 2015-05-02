//===--- ForeignErrorConvention.h - Error conventions ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2015 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See http://swift.org/LICENSE.txt for license information
// See http://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines the ForeignErrorConvention structure, which
// describes the rules for how to detect that a foreign API returns an
// error.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_FOREIGN_ERROR_CONVENTION_H
#define SWIFT_FOREIGN_ERROR_CONVENTION_H

#include "swift/AST/Type.h"

namespace swift {

/// A small structure describing the error convention of a foreign declaration.
class ForeignErrorConvention {
public:
  enum Kind {
    /// An error is indicated by a zero result.  The function has
    /// been imported as returning Void.
    ///
    /// This convention provides a result type, which has a single
    /// field of integer type.
    ZeroResult,

    /// An error is indicated by a non-zero result.  The function has
    /// been imported as returning Void.
    ///
    /// This convention provides a result type, which has a single
    /// field of integer type.
    NonZeroResult,

    /// An error is indicated by a nil result.  The function has been
    /// imported as returning a non-optional type (which is not
    /// address-only).
    NilResult,

    /// An error is indicated by a non-nil error value being left in
    /// the error parameter.
    NonNilError,
  };

  /// Is the error owned by the caller, given that it exists?
  enum IsOwned_t : bool {
    IsNotOwned = false, IsOwned = true
  };

  /// Was the error parameter replaced by () or just removed?
  enum IsReplaced_t : bool {
    IsNotReplaced = false, IsReplaced = true
  };

private:  
  unsigned TheKind : 8;
  unsigned ErrorIsOwned : 1;
  unsigned ErrorParameterIsReplaced : 1;
  unsigned ErrorParameterIndex : 22;

  /// The error parameter type.  This is currently assumed to be an
  /// indirect out-parameter.
  CanType ErrorParameterType;

  /// The result type.  This is meaningful only for ZeroResult and
  /// NonZeroResult.
  CanType ResultType;

  ForeignErrorConvention(Kind kind, unsigned parameterIndex,
                         IsOwned_t isOwned, IsReplaced_t isReplaced,
                         Type parameterType, Type resultType = Type())
    : TheKind(unsigned(kind)), ErrorIsOwned(bool(isOwned)),
      ErrorParameterIsReplaced(bool(isOwned)),
      ErrorParameterIndex(parameterIndex), ErrorParameterType(parameterType),
      ResultType(resultType) {
  }

public:
  static ForeignErrorConvention getZeroResult(unsigned parameterIndex,
                                              IsOwned_t isOwned,
                                              IsReplaced_t isReplaced,
                                              CanType parameterType,
                                              CanType resultType) {
    return { ZeroResult, parameterIndex, isOwned, isReplaced,
             parameterType, resultType };
  }

  static ForeignErrorConvention getNonZeroResult(unsigned parameterIndex,
                                                 IsOwned_t isOwned,
                                                 IsReplaced_t isReplaced,
                                                 CanType parameterType,
                                                 CanType resultType) {
    return { NonZeroResult, parameterIndex, isOwned, isReplaced,
             parameterType, resultType };
  }

  static ForeignErrorConvention getNilResult(unsigned parameterIndex,
                                             IsOwned_t isOwned,
                                             IsReplaced_t isReplaced,
                                             CanType parameterType) {
    return { NilResult, parameterIndex, isOwned, isReplaced, parameterType };
  }

  static ForeignErrorConvention getNonNilError(unsigned parameterIndex,
                                               IsOwned_t isOwned,
                                               IsReplaced_t isReplaced,
                                               CanType parameterType) {
    return { NilResult, parameterIndex, isOwned, isReplaced, parameterType };
  }

  /// Returns the error convention in use.
  Kind getKind() const {
    return Kind(TheKind);
  }

  /// Returns true if this convention strips a layer of optionality
  /// from the formal result type.
  bool stripsResultOptionality() const {
    return getKind() == Kind::NilResult;
  }

  /// Returns the index of the error parameter.
  unsigned getErrorParameterIndex() const {
    return ErrorParameterIndex;
  }

  /// Has the error parameter been replaced with void?
  IsReplaced_t isErrorParameterReplacedWithVoid() const {
    return IsReplaced_t(ErrorParameterIsReplaced);
  }

  /// Returns whether the error result is owned.  It's assumed that the
  /// error parameter should be ignored if no error is present (unless
  /// it needs to be checked explicitly to determine that).
  IsOwned_t isErrorOwned() const {
    return IsOwned_t(ErrorIsOwned);
  }

  /// Returns the type of the error parameter.  Assumed to be an
  /// UnsafeMutablePointer<T?> for some T.
  CanType getErrorParameterType() const {
    return ErrorParameterType;
  }

  /// Returns the physical result type of the function, for functions
  /// that completely erase this information.
  CanType getResultType() const {
    assert(getKind() == ZeroResult ||
           getKind() == NonZeroResult);
    return ResultType;
  }
};

}

#endif
