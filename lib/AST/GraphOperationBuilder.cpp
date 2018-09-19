//=== GraphOperationBuilder.h - GraphOperationInst Build Logic --*- C++ -*-===//
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

#include "swift/SIL/GraphOperationBuilder.h"
#include "swift/SIL/SILBuilder.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILLocation.h"

namespace swift {
namespace tf {

/// Start building a GraphOperationInst for op `OpName`.
GraphOperationBuilder::GraphOperationBuilder(StringRef OpName)
    : MangledName(OpName) {}

/// Add a single operand to the GraphOperationInst, with an optional name.
void GraphOperationBuilder::addOperand(SILValue operand, StringRef name) {
  MangledName += ",i";
  MangledName += name;
  Operands.push_back(operand);
}

/// Add a rank-1 list of operands to the GraphOperationInst, with an optional
/// name.
void GraphOperationBuilder::addListOperand(ArrayRef<SILValue> operands,
                                           StringRef name) {
  MangledName += ",L";
  MangledName += name;
  for (auto operand : operands) {
    MangledName += ",e";
    Operands.push_back(operand);
  }
}

/// Add an attribute with known constant value to the GraphOperationInst.
GraphOperationAttribute &GraphOperationBuilder::addAttribute(
    const GraphOperationAttribute &attribute) {
  Attributes.push_back(attribute);
  return Attributes.back();
}

/// Special method that should only be used for "tfc.scalarToTensor"'s operand,
/// because it has special name mangling. (Marker is "s").
void GraphOperationBuilder::addScalarOperand(SILValue operand) {
  MangledName += ",s";
  Operands.push_back(operand);
}

/// Special method that should only be used for "tf_tensor_to_i1"'s operand,
/// because it has special name mangling. (No marker for its operand).
void GraphOperationBuilder::addTFTensorToI1Operand(SILValue operand) {
  Operands.push_back(operand);
}

/// Build the GraphOperationInst.
GraphOperationInst* GraphOperationBuilder::build(
    SILBuilder &B, ASTContext &C, SILLocation loc,
    ArrayRef<SILType> resultSILTypes) const {
  return B.createGraphOperation(loc, C.getIdentifier(MangledName), Operands,
                                Attributes, resultSILTypes);
}

} // end namespace tf
} // end namespace swift
