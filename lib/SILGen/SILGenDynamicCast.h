//===--- SILGenDynamicCast.h - SILGen for dynamic casts ---------*- C++ -*-===//
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

#ifndef SWIFT_SILGEN_DYNAMIC_CAST_H
#define SWIFT_SILGEN_DYNAMIC_CAST_H

#include "SILGenFunction.h"

namespace swift {
namespace Lowering {

RValue emitUnconditionalCheckedCast(SILGenFunction &SGF,
                                    SILLocation loc,
                                    Expr *operand,
                                    Type targetType,
                                    CheckedCastKind castKind,
                                    SGFContext C);

RValue emitConditionalCheckedCast(SILGenFunction &SGF,
                                  SILLocation loc,
                                  ManagedValue operand,
                                  Type operandType,
                                  Type targetType,
                                  CheckedCastKind castKind,
                                  SGFContext C);

SILValue emitIsa(SILGenFunction &SGF, SILLocation loc,
                 Expr *operand, Type targetType,
                 CheckedCastKind castKind);

}
}

#endif
