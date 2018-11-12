//=== GraphOperationBuilder.cpp - GraphOperationInst Build Logic *- C++ -*-===//
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
    : MangledName(OpName) {
  assert(MangledName.find(',') == StringRef::npos &&
         "graph_op name cannot include ','");
}

/// Add a single argument to the GraphOperationInst, with an optional name.
void GraphOperationBuilder::addArgument(SILValue argument, StringRef name) {
  MangledName += ",i";
  MangledName += name;
  Operands.push_back(argument);
}

/// Add a list argument to the GraphOperationInst, with an optional name.
void GraphOperationBuilder::addListArgument(ArrayRef<SILValue> arguments,
                                            StringRef name) {
  MangledName += ",L";
  MangledName += name;
  for (auto argument : arguments) {
    MangledName += ",e";
    Operands.push_back(argument);
  }
}

/// Add a list argument to the GraphOperationInst, with an optional name.
void GraphOperationBuilder::addListArgument(OperandValueArrayRef arguments,
                                            StringRef name) {
  MangledName += ",L";
  MangledName += name;
  for (auto argument : arguments) {
    MangledName += ",e";
    Operands.push_back(argument);
  }
}

/// Add an attribute with known constant value to the GraphOperationInst.
void GraphOperationBuilder::addAttribute(
    const GraphOperationAttribute &attribute) {
  Attributes.push_back(attribute);
}

/// Build the GraphOperationInst.
GraphOperationInst* GraphOperationBuilder::build(
    SILBuilder &B, ASTContext &C, SILLocation loc,
    ArrayRef<SILType> resultSILTypes) const {
  return B.createGraphOperation(loc, C.getIdentifier(MangledName), Operands,
                                Attributes, /*runOutOfGraph*/ false,
                                resultSILTypes);
}

} // end namespace tf
} // end namespace swift
