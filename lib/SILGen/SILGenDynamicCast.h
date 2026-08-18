//===--- SILGenDynamicCast.h - SILGen for dynamic casts ---------*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_DYNAMIC_CAST_H
#define SWIFT_SILGEN_DYNAMIC_CAST_H

#include "SILGenFunction.h"

namespace swift {
namespace Lowering {

/// The SIL representation and runtime operation used for a checked cast.
enum class CastStrategy : uint8_t {
  Address,
  Scalar,
  COM,
};

CastStrategy computeCastStrategy(SILGenFunction &SGF, CanType sourceType,
                                 CanType targetType);

ManagedValue prepareCOMCastSource(SILGenFunction &SGF, SILLocation loc,
                                  ManagedValue source);

inline bool usesAddress(CastStrategy strategy) {
  return strategy != CastStrategy::Scalar;
}

inline bool isCOMCast(CastStrategy strategy) {
  return strategy == CastStrategy::COM;
}

RValue emitUnconditionalCheckedCast(SILGenFunction &SGF,
                                    SILLocation loc,
                                    Expr *operand,
                                    Type targetType,
                                    CheckedCastKind castKind,
                                    SGFContext C);

RValue emitConditionalCheckedCast(SILGenFunction &SGF, SILLocation loc,
                                  ManagedValue operand, Type operandType,
                                  Type targetType, CheckedCastKind castKind,
                                  SGFContext C, ProfileCounter TrueCount,
                                  ProfileCounter FalseCount);

SILValue emitIsa(SILGenFunction &SGF, SILLocation loc,
                 Expr *operand, Type targetType,
                 CheckedCastKind castKind);

}
}

#endif
