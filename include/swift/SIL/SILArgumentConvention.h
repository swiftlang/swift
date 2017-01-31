//===--- SILArgumentConvention.h --------------------------------*- C++ -*-===//
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

#ifndef SWIFT_SIL_SILARGUMENTCONVENTION_H
#define SWIFT_SIL_SILARGUMENTCONVENTION_H

#include "swift/AST/Types.h"

namespace swift {

enum class InoutAliasingAssumption {
  /// Assume that an inout indirect parameter may alias other objects.
  /// This is the safe assumption an optimization should make if it may break
  /// memory safety in case the inout aliasing rule is violation.
  Aliasing,

  /// Assume that an inout indirect parameter cannot alias other objects.
  /// Optimizations should only use this if they can guarantee that they will
  /// not break memory safety even if the inout aliasing rule is violated.
  NotAliasing
};

/// Conventions for apply operands and function-entry arguments in SIL.
///
/// This is simply a union of ParameterConvention and ResultConvention
/// (ParameterConvention + Indirect_Out) for convenience when visiting all
/// arguments.
struct SILArgumentConvention {
  enum ConventionType : uint8_t {
    Indirect_In,
    Indirect_In_Guaranteed,
    Indirect_Inout,
    Indirect_InoutAliasable,
    Indirect_Out,
    Direct_Owned,
    Direct_Unowned,
    Direct_Deallocating,
    Direct_Guaranteed,
  } Value;

  SILArgumentConvention(decltype(Value) NewValue) : Value(NewValue) {}

  /// Turn a ParameterConvention into a SILArgumentConvention.
  explicit SILArgumentConvention(ParameterConvention Conv) : Value() {
    switch (Conv) {
    case ParameterConvention::Indirect_In:
      Value = SILArgumentConvention::Indirect_In;
      return;
    case ParameterConvention::Indirect_Inout:
      Value = SILArgumentConvention::Indirect_Inout;
      return;
    case ParameterConvention::Indirect_InoutAliasable:
      Value = SILArgumentConvention::Indirect_InoutAliasable;
      return;
    case ParameterConvention::Indirect_In_Guaranteed:
      Value = SILArgumentConvention::Indirect_In_Guaranteed;
      return;
    case ParameterConvention::Direct_Unowned:
      Value = SILArgumentConvention::Direct_Unowned;
      return;
    case ParameterConvention::Direct_Guaranteed:
      Value = SILArgumentConvention::Direct_Guaranteed;
      return;
    case ParameterConvention::Direct_Owned:
      Value = SILArgumentConvention::Direct_Owned;
      return;
    }
    llvm_unreachable("covered switch isn't covered?!");
  }

  operator ConventionType() const { return Value; }

  bool isIndirectConvention() const {
    return Value <= SILArgumentConvention::Indirect_Out;
  }

  /// Returns true if \p Value is a not-aliasing indirect parameter.
  /// The \p isInoutAliasing specifies what to assume about the inout
  /// convention.
  /// See InoutAliasingAssumption.
  bool isNotAliasedIndirectParameter(InoutAliasingAssumption isInoutAliasing) {
    switch (Value) {
    case SILArgumentConvention::Indirect_In:
    case SILArgumentConvention::Indirect_Out:
    case SILArgumentConvention::Indirect_In_Guaranteed:
      return true;

    case SILArgumentConvention::Indirect_Inout:
      return isInoutAliasing == InoutAliasingAssumption::NotAliasing;

    case SILArgumentConvention::Indirect_InoutAliasable:
    case SILArgumentConvention::Direct_Unowned:
    case SILArgumentConvention::Direct_Guaranteed:
    case SILArgumentConvention::Direct_Owned:
    case SILArgumentConvention::Direct_Deallocating:
      return false;
    }
    llvm_unreachable("covered switch isn't covered?!");
  }
};

} // namespace swift

#endif
