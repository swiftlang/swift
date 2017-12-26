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
  /// If the specified type is the well-known TensorCore<T> type, then return
  /// "T".  If not, return a null type.
  Type isTensorCore(Type ty);

  /// This is the interpretation of the operand passed into a SILInstruction for
  /// an op in the host program.
  enum class OpCommand {
    Tensor,     // 't'
    Constant,   // 'c'
    AddDType,   // 'd'
  };

  /// Represent information about a TensorFlow operation as represented in SIL.
  /// This handles both forms that these operations can be in: the one coming
  /// from the parser using apply_inst/function_ref, and the builtin_inst form.
  struct TensorOpInfo {
    /// The instruction being analyzed.
    SILInstruction *inst;

    /// This is the TensorFlow name for the op.
    StringRef opName;

    /// This is the string representation of the operand descriptors.
    StringRef operandDescriptorStr;

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
  /// into a TensorFlow graph, and write it to disk.
  void emitTensorFlowGraph(SILFunction *F, StringRef fnName);

  /// Return true if the specified type is a valid tensor element type.  For
  /// example, int128 and pointers are not.
  bool isValidTensorFlowElementType(Type ty);

} // end namespace tf
} // end namespace swift
#endif


