//===--- TensorFlow.h - Definitions for TensorFlow lowering xforms --------===//
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
// This defines the shared code that implements the various TensorFlow related
// lowerings and other transformations.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_TENSORFLOW_H
#define SWIFT_SILOPTIMIZER_TENSORFLOW_H

#include "swift/SIL/SILFunction.h"

namespace swift {
namespace tf {
  /// This returns true if we should dump out intermediate results to standard
  /// out.  This is used for integration unit tests.
  bool shouldDumpIntermediates();

  /// If the specified type is the well-known TensorHandle<T> type, then return
  /// "T".  If not, return a null type.
  Type isTensorHandle(Type ty);

  /// This function maps a Swift type (either a language type like Float or an
  /// LLVM Builtin type like Builtin.f32) into the TensorFlow TF_DataType value.
  unsigned convertSwiftTypeToTF(Type ty);


  /// This is the interpretation of the operand passed into a SILInstruction for
  /// an op in the host program.
  enum class OpCommand {
    Tensor,     // 't'
    Constant,   // 'c'
    AddDType,   // 'd'
  };

  /// Represent information about a TensorFlow operation as represented in SIL
  /// as Builtin instructions.
  struct TensorOpInfo {
    /// The instruction being analyzed.
    SILInstruction *inst;

    /// This is the TensorFlow name for the op.
    StringRef opName;

    /// This is the string representation of the operand & result descriptors.
    StringRef operandDescriptorStr;
    StringRef resultDescriptorStr;

    /// These are decoded descriptors for operands.
    SmallVector<OpCommand, 4> operandDescriptors;

    TensorOpInfo(SILInstruction &inst) : inst(&inst) {}

    /// Return true and fill in this struct if this is a tensor operation that
    /// should be partitioned out to run on the accelerator.
    bool decode();

    /// If the specified value is a valid value for a constant operand, return
    /// the literal it is initialized to, otherwise null.
    LiteralInst *getTensorConstantOperand(unsigned operandNumber) {
      return getTensorConstantOperand(inst->getOperand(operandNumber));
    }
    LiteralInst *getTensorConstantOperand(SILValue v);
  };



  /// The SIL location for operations we process are usually deep in the bowels
  /// of the tensor library code, which are all implementation details to the
  /// user.  As such, walk the inlining location of the specified node to return
  /// the first location *outside* of the tensor implementation goop.
  SILDebugLocation getUserSourceLocation(SILDebugLocation loc);

  /// This form of getUserSourceLocation is useful when working with SILValue's.
  SILLocation getUserSourceLocation(SILLocation loc, SILNode *value);

  /// Lower the specified SIL function (which was formed by the partitioner)
  /// into a TensorFlow graph, and encode into a vector of bytes.
  ///
  std::vector<char> lowerTFGraph(SILFunction *fn);

  /// Return true if the specified type is a valid tensor element type.  For
  /// example, int128 and pointers are not.
  bool isValidTensorFlowElementType(Type ty);

} // end namespace tf
} // end namespace swift
#endif


