//===--- Score.h - Online scoring of partial solutions ----------*- C++ -*-===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2014 - 2026 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SEMA_SCORE_H
#define SWIFT_SEMA_SCORE_H

#include "llvm/Support/raw_ostream.h"

namespace swift {

namespace constraints {

/// Describes an aspect of a solution that affects its overall score, i.e., a
/// user-defined conversions.
enum ScoreKind: unsigned int {
  // These values are used as indices into a Score value.

  /// A fix needs to be applied to the source.
  SK_Fix,
  /// A hole in the constraint system.
  SK_Hole,
  /// A reference to an @unavailable declaration.
  SK_Unavailable,
  /// A reference to a declaration from a module that has not been imported.
  SK_MissingImport,
  /// A reference to an async function in a synchronous context.
  ///
  /// \note Any score kind after this is considered a conversion that doesn't
  /// require fixing the source and will be ignored during code completion.
  SK_AsyncInSyncMismatch,
  /// Synchronous function in an asynchronous context or a conversion of
  /// a synchronous function to an asynchronous one.
  SK_SyncInAsync,
  /// A use of the "forward" scan for trailing closures.
  SK_ForwardTrailingClosure,
  /// A use of a disfavored overload.
  SK_DisfavoredOverload,
  /// A member for an \c UnresolvedMemberExpr found via unwrapped optional base.
  SK_UnresolvedMemberViaOptional,
  /// An implicit force of an implicitly unwrapped optional value.
  SK_ForceUnchecked,
  /// An implicit conversion from a value of one type (lhs)
  /// to another type (rhs) via implicit initialization of
  /// `rhs` type with an argument of `lhs` value.
  SK_ImplicitValueConversion,
  /// A user-defined conversion.
  SK_UserConversion,
  /// A non-trivial function conversion.
  SK_FunctionConversion,
  /// A literal expression bound to a non-default literal type.
  SK_NonDefaultLiteral,
  /// An implicit upcast conversion between collection types.
  SK_CollectionUpcastConversion,
  /// A value-to-optional conversion.
  SK_ValueToOptional,
  /// A conversion to an empty existential type ('Any' or '{}').
  SK_EmptyExistentialConversion,
  /// A key path application subscript.
  SK_KeyPathSubscript,
  /// A pointer conversion where the destination type is a generic parameter.
  /// This should eventually be removed in favor of outright banning pointer
  /// conversions for generic parameters. As such we consider it more impactful
  /// than \c SK_ValueToPointerConversion.
  SK_GenericParamPointerConversion,
  /// A conversion from a string, array, or inout to a pointer.
  SK_ValueToPointerConversion,
  /// A closure/function conversion to an autoclosure parameter.
  SK_FunctionToAutoClosureConversion,
  /// A type with a missing conformance(s) that has be synthesized
  /// or diagnosed later, such types are allowed to appear in
  /// a valid solution.
  SK_MissingSynthesizableConformance,
  /// An unapplied reference to a function. The purpose of this
  /// score bit is to prune overload choices that are functions
  /// when a solution has already been found using property.
  ///
  /// \Note The solver only prefers variables over functions
  /// to resolve ambiguities, so please be sure that any score
  /// kind added after this is truly less impactful. Only other
  /// ambiguity tie-breakers should go after this; anything else
  /// should be added above.
  SK_UnappliedFunction,

  SK_LastScoreKind = SK_UnappliedFunction,
};

/// The number of score kinds.
const unsigned NumScoreKinds = SK_LastScoreKind + 1;

struct Score;
/// Display a score.
llvm::raw_ostream &operator<<(llvm::raw_ostream &out, const Score &score);

/// Describes the fixed score of a solution to the constraint system.
struct Score {
  unsigned Data[NumScoreKinds] = {};

  friend Score &operator+=(Score &x, const Score &y) {
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      x.Data[i] += y.Data[i];
    }
    return x;
  }

  friend Score operator+(const Score &x, const Score &y) {
    Score result;
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      result.Data[i] = x.Data[i] + y.Data[i];
    }
    return result;
  }

  friend Score operator-(const Score &x, const Score &y) {
    Score result;
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      ASSERT(x.Data[i] >= y.Data[i]);
      result.Data[i] = x.Data[i] - y.Data[i];
    }
    return result;
  }

  friend Score &operator-=(Score &x, const Score &y) {
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      ASSERT(x.Data[i] >= y.Data[i]);
      x.Data[i] -= y.Data[i];
    }
    return x;
  }

  friend bool operator==(const Score &x, const Score &y) {
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      if (x.Data[i] != y.Data[i])
        return false;
    }

    return true;
  }

  friend bool operator!=(const Score &x, const Score &y) {
    return !(x == y);
  }

  friend bool operator<(const Score &x, const Score &y) {
    for (unsigned i = 0; i != NumScoreKinds; ++i) {
      if (x.Data[i] < y.Data[i])
        return true;

      if (x.Data[i] > y.Data[i])
        return false;
    }

    return false;
  }

  friend bool operator<=(const Score &x, const Score &y) {
    return !(y < x);
  }

  friend bool operator>(const Score &x, const Score &y) {
    return y < x;
  }

  friend bool operator>=(const Score &x, const Score &y) {
    return !(x < y);
  }
  
  /// Return ScoreKind descriptions for printing alongside non-zero ScoreKinds
  /// in debug output.
  static std::string getNameFor(ScoreKind kind) {
    switch (kind) {
    case SK_Hole:
      return "hole";

    case SK_Unavailable:
      return "use of an unavailable declaration";

    case SK_MissingImport:
      return "use of a declaration that has not been imported";

    case SK_AsyncInSyncMismatch:
      return "async-in-synchronous mismatch";

    case SK_SyncInAsync:
      return "sync-in-asynchronous";

    case SK_ForwardTrailingClosure:
      return "forward scan when matching a trailing closure";

    case SK_Fix:
      return "applied fix";

    case SK_DisfavoredOverload:
      return "disfavored overload";

    case SK_UnresolvedMemberViaOptional:
      return "unwrapping optional at unresolved member base";

    case SK_ForceUnchecked:
      return "force of an implicitly unwrapped optional";

    case SK_UserConversion:
      return "user conversion";

    case SK_FunctionConversion:
      return "function conversion";

    case SK_NonDefaultLiteral:
      return "non-default literal";

    case SK_CollectionUpcastConversion:
      return "collection upcast conversion";

    case SK_ValueToOptional:
      return "value to optional promotion";

    case SK_EmptyExistentialConversion:
      return "empty-existential conversion";

    case SK_KeyPathSubscript:
      return "key path subscript";

    case SK_GenericParamPointerConversion:
      return "pointer conversion to generic parameter";

    case SK_ValueToPointerConversion:
      return "value-to-pointer conversion";

    case SK_FunctionToAutoClosureConversion:
      return "function to autoclosure parameter conversion";

    case SK_ImplicitValueConversion:
      return "value-to-value conversion";

    case SK_UnappliedFunction:
      return "use of overloaded unapplied function";

    case SK_MissingSynthesizableConformance:
      return "type with missing synthesizable conformance";
    }
  }

  /// Print Score list a with brief description of any non-zero ScoreKinds.
  void print(llvm::raw_ostream &out) const {
    bool hasNonDefault = false;
    for (unsigned int i = 0; i < NumScoreKinds; ++i) {
      if (Data[i] != 0) {
        out << " [";
        out << getNameFor(ScoreKind(i));
        out << "(s), weight: ";
        out << std::to_string(NumScoreKinds - i);
        out << ", impact: ";
        out << std::to_string(Data[i]);
        out << "]";
        hasNonDefault = true;
      }
    }
    if (!hasNonDefault) {
      out << " <default ";
      out << *this;
      out << ">";
    }
  }
};

}  // end namespace constraints

}  // end namespace swift

#endif  // SWIFT_SEMA_SCORE_H
