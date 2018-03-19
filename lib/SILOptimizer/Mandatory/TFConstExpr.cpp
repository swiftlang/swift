//===--- TFConstExpr.cpp - TensorFlow constant expressions ----------------===//
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

#define DEBUG_TYPE "TFConstExpr"
#include "TFConstExpr.h"
#include "swift/SIL/SILInstruction.h"

using namespace swift;
using namespace tf;

/// For constant values, return the type classification of this value.
auto LatticeValue::getTypeKind() const -> TypeKind {
  switch (kind) {
  case Undefined:
  case Overdefined: assert(0 && "Not a constant value");
  case ConstantArray: return Array;
  case ConstantInst:
    auto *inst = value.inst;
    if (isa<IntegerLiteralInst>(inst))
      return Integer;
    if (isa<FloatLiteralInst>(inst))
      return Float;
    assert(isa<StringLiteralInst>(inst) && "Unknown ConstantInst kind");
    return String;
  }
}

APInt LatticeValue::getIntegerValue() const {
  assert(getTypeKind() == Integer);
  assert(kind == ConstantInst);
  return cast<IntegerLiteralInst>(value.inst)->getValue();
}

APFloat LatticeValue::getFloatValue() const {
  assert(getTypeKind() == Float);
  assert(kind == ConstantInst);
  return cast<FloatLiteralInst>(value.inst)->getValue();
}


/// Analyze the body of the specified function (which itself may not be a
/// constexpr).  Determine whether the specified SymbolicValue's are
/// constants, and return their LatticeValue's.
///
/// TODO: Return information about which callees were found to be
/// constexprs, which would allow the caller to delete dead calls to them
/// that occur after
void ConstExprEvaluator::
computeConstantValues(SILFunction &fn, ArrayRef<SymbolicValue> values,
                      SmallVectorImpl<LatticeValue> &results) {
  // This is a conservative but not very useful implementation of this
  // interface.
  for (auto v : values)
    results.push_back(LatticeValue::getOverdefined());
}
