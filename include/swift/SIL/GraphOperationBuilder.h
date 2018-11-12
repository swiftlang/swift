//=== GraphOperationBuilder.h - GraphOperationInst Build Logic *- C++ ---*-===//
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
// This file defines the construction logic for a GraphOperationInst, in
// particular encoding the mangled inst name string for the arguments and
// attributes.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SIL_GRAPH_OPERATION_BUILDER_H
#define SWIFT_SIL_GRAPH_OPERATION_BUILDER_H

#include "swift/SIL/SILConstants.h"
#include "swift/SIL/SILInstruction.h"
#include "swift/SIL/SILValue.h"
#include "llvm/ADT/ArrayRef.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"

namespace swift {
namespace tf {

class GraphOperationBuilder {
  std::string MangledName;
  llvm::SmallVector<SILValue, 4> Operands;
  llvm::SmallVector<GraphOperationAttribute, 4> Attributes;

public:
  /// Start building a GraphOperationInst for op `OpName`.
  GraphOperationBuilder(llvm::StringRef OpName);

  /// Add a single argument to the GraphOperationInst, with an optional name.
  void addArgument(SILValue argument, llvm::StringRef name = llvm::StringRef());

  /// Add a list argument to the GraphOperationInst, with an optional name.
  void addListArgument(llvm::ArrayRef<SILValue> arguments,
                       llvm::StringRef name = llvm::StringRef());

  /// Add a list argument to the GraphOperationInst, with an optional name.
  void addListArgument(OperandValueArrayRef arguments,
                       llvm::StringRef name = llvm::StringRef());

  /// Add an attribute with known constant value to the GraphOperationInst.
  void addAttribute(const GraphOperationAttribute &attribute);

  ArrayRef<GraphOperationAttribute> getAttributes() const { return Attributes; }

  /// Build the GraphOperationInst.
  GraphOperationInst *build(SILBuilder &B, ASTContext &C, SILLocation loc,
                            llvm::ArrayRef<SILType> resultSILTypes) const;
};

} // end namespace tf
} // end namespace swift

#endif // SWIFT_SIL_GRAPH_OPERATION_BUILDER_H
