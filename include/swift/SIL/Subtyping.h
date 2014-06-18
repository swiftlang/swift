//===--- Subtyping.h - SIL subtyping utilities ------------------*- C++ -*-===//
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
// This file provides basic utilities for working with subtyping
// relationships.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_SUBTYPING_H
#define SWIFT_SIL_SUBTYPING_H

namespace swift {

class CanType;
class Module;
class SILBuilder;
class SILLocation;
class SILValue;
class SILType;

enum class DynamicCastFeasibility {
  /// The cast will always succeed.
  WillSucceed,

  /// The cast can succeed for some values.
  MaySucceed,

  /// The cast cannot succeed.
  WillFail,
};

/// Classify the feasibility of a dynamic cast.  The source and target
/// types should be unlowered formal types.
DynamicCastFeasibility classifyDynamicCast(Module *context,
                                           CanType sourceType,
                                           CanType targetType);

SILValue
swift::emitSuccessfulScalarUnconditionalCast(SILBuilder &B, Module *M,
                                             SILLocation loc, SILValue value,
                                             CanType formalSourceType,
                                             CanType formalTargetType) {

} // end namespace swift

#endif

