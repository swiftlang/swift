//===--- TensorFlow.h - AST Level TensorFlow Support Logic ------*- C++ -*-===//
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
// This file defines the AST level TensorFlow support logic that is used across
// the Swift compiler.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_AST_TENSORFLOW_H
#define SWIFT_AST_TENSORFLOW_H

#include "swift/Basic/LLVM.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/SmallVector.h"

namespace swift {
namespace tf {
/// This is the interpretation of the operand or result value passed into a
/// Tensor op.
enum class OpDescriptor {
  /// 't' - This is a tensor operand and result type, whose Unit type is
  /// inferred from context.
  Tensor,

  /// 's' - This is a scalar (non-tensor) operand or result value, whose type
  /// is inferred from context.
  Scalar,

  /// 'd' - GraphGen should add the 'dtype' attribute to the generated TF graph
  /// node, where the type added is the element type of the op's tensor result.
  AddDType,

  /// 'c' - This operand is required to be a scalar constant after
  /// deabstraction.
  Constant,
};

/// Represent decoded information about a TensorFlow operation.
struct TensorOpInfo {

  /// This is the string representation of the operand & result descriptors.
  StringRef operandDescriptorStr, resultDescriptorStr;

  /// These are decoded descriptors for operands and results.
  SmallVector<OpDescriptor, 4> operandDescriptors, resultDescriptors;

  /// This little struct represents an error parsing the constraint string for
  /// an operation.  If parsing is successful, the location is null.
  struct ParseErrorInfo {
    const char *loc;
    std::string message;

    bool isSuccess() const { return loc == nullptr; }
    bool isError() const { return loc != nullptr; }

    static ParseErrorInfo getSuccess() {
      return { nullptr, "" };
    }
  };

  /// This decodes the specified string as an operand & result constraint string
  /// which looks something like "tt:t".  It fills in the operand/result
  /// descriptor strings and decoded forms.
  ParseErrorInfo decodeDescriptorString(StringRef operandAndResult);
};


} // end namespace tf
} // end namespace swift
#endif
