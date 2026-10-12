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

/// True if a cast from \p sourceType to \p targetType can be answered by a
/// non-consuming type test rather than by extracting the payload.
bool canUseNoncopyableTypeTest(CanType sourceType, CanType targetType,
                               CheckedCastKind castKind);

/// Borrow the storage \p operand names so a type test can read it without
/// copying or consuming it.
///
/// The caller must have established a FormalEvaluationScope covering the use
/// of the returned value.
ManagedValue emitTypeTestOperand(SILGenFunction &SGF, Expr *operand);

/// Emit a non-consuming test of whether the existential at \p existentialAddr
/// can be cast to \p targetType.  This implements certain `is` casting tests.
///
/// The source existential is only read. No value is produced on either edge.
/// The caller must keep \p existentialAddr borrowed across the emitted
/// terminator.
///
/// Only valid when canUseNoncopyableTypeTest() returns true.
void emitNoncopyableTypeTest(SILGenFunction &SGF, SILLocation loc,
                             ManagedValue existentialAddr, CanType sourceType,
                             CanType targetType, SILBasicBlock *trueBB,
                             SILBasicBlock *falseBB,
                             ProfileCounter trueCount = ProfileCounter(),
                             ProfileCounter falseCount = ProfileCounter());

}
}

#endif
