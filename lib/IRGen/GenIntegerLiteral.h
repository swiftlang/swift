//===--- GenIntegerLiteral.h - IRGen for Builtin.IntegerLiteral -*- C++ -*-===//
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
// This file defines interfaces for emitting code for Builtin.IntegerLiteral
// values.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_IRGEN_GENINTEGERLITERAL_H
#define SWIFT_IRGEN_GENINTEGERLITERAL_H

#include "swift/Basic/APIntMap.h"

namespace llvm {
class Constant;
class IntegerType;
class Type;
class Value;
}

namespace swift {
class IntegerLiteralInst;

namespace irgen {
class Explosion;
class IRGenFunction;
class IRGenModule;

/// A constant integer literal value.
struct ConstantIntegerLiteral {
  llvm::Constant *Data;
  llvm::Constant *Flags;
};

/// A map for caching globally-emitted constant integers.
class ConstantIntegerLiteralMap {
  APIntMap<ConstantIntegerLiteral> map;

public:
  ConstantIntegerLiteralMap() {}

  ConstantIntegerLiteral get(IRGenModule &IGM, APInt &&value);
};

/// Construct a constant IntegerLiteral from an IntegerLiteralInst.
ConstantIntegerLiteral
emitConstantIntegerLiteral(IRGenModule &IGM, IntegerLiteralInst *ILI);

/// Emit a checked truncation of an IntegerLiteral value.
void emitIntegerLiteralCheckedTrunc(IRGenFunction &IGF,
                                    Explosion &in,
                                    llvm::IntegerType *resultTy,
                                    bool resultIsSigned,
                                    Explosion &out);

/// Emit a sitofp operation on an IntegerLiteral value.
llvm::Value *emitIntegerLiteralToFP(IRGenFunction &IGF,
                                    Explosion &in,
                                    llvm::Type *toType);

}
}

#endif