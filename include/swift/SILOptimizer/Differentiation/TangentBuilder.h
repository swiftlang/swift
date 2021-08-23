//===--- TangentBuilder.h - Tangent SIL builder --------------*- C++ -*----===//
//
// This source file is part of the Swift.org open source project
//
// Copyright (c) 2019 - 2020 Apple Inc. and the Swift project authors
// Licensed under Apache License v2.0 with Runtime Library Exception
//
// See https://swift.org/LICENSE.txt for license information
// See https://swift.org/CONTRIBUTORS.txt for the list of Swift project authors
//
//===----------------------------------------------------------------------===//
//
// This file defines a helper class for emitting tangent code for automatic
// differentiation.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_TANGENTBUILDER_H
#define SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_TANGENTBUILDER_H

#include "swift/SIL/SILBuilder.h"

namespace swift {
namespace autodiff {

class ADContext;

class TangentBuilder: public SILBuilder {
private:
  ADContext &adContext;

public:
  TangentBuilder(SILFunction &fn, ADContext &adContext)
      : SILBuilder(fn), adContext(adContext) {}
  TangentBuilder(SILBasicBlock *bb, ADContext &adContext)
      : SILBuilder(bb), adContext(adContext) {}
  TangentBuilder(SILBasicBlock::iterator insertionPt, ADContext &adContext)
      : SILBuilder(insertionPt), adContext(adContext) {}
  TangentBuilder(SILBasicBlock *bb, SILBasicBlock::iterator insertionPt,
                 ADContext &adContext)
      : SILBuilder(bb, insertionPt), adContext(adContext) {}

  /// Emits an `AdditiveArithmetic.zero` into the given buffer. If it is not an
  /// initialization (`isInit`), a `destroy_addr` will be emitted on the buffer
  /// first. The buffer must have a type that conforms to `AdditiveArithmetic`
  /// or be a tuple thereof.
  void emitZeroIntoBuffer(SILLocation loc, SILValue buffer,
                          IsInitialization_t isInit);

  /// Emits an `AdditiveArithmetic.zero` of the given type. The type must be a
  /// loadable type, and must conform to `AddditiveArithmetic` or be a tuple
  /// thereof.
  SILValue emitZero(SILLocation loc, CanType type);

  /// Emits an `AdditiveArithmetic.+=` for the given destination buffer and
  /// operand. The type of the buffer and the operand must conform to
  /// `AddditiveArithmetic` or be a tuple thereof. The operand will not be
  /// consumed.
  void emitInPlaceAdd(SILLocation loc, SILValue destinationBuffer,
                      SILValue operand);

  /// Emits an `AdditiveArithmetic.+` for the given operands. The type of the
  /// operands must conform to `AddditiveArithmetic` or be a tuple thereof. The
  /// operands will not be consumed.
  void emitAddIntoBuffer(SILLocation loc, SILValue destinationBuffer,
                         SILValue lhsAddress, SILValue rhsAddress);

  /// Emits an `AdditiveArithmetic.+` for the given operands. The type of the
  /// operands must be a loadable type, and must conform to
  /// `AddditiveArithmetic` or be a tuple thereof. The operands will not be
  /// consumed.
  SILValue emitAdd(SILLocation loc, SILValue lhs, SILValue rhs);
};

} // end namespace autodiff
} // end namespace swift

#endif /* SWIFT_SILOPTIMIZER_UTILS_DIFFERENTIATION_TANGENTBUILDER_H */
