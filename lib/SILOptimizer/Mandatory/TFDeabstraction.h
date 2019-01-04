//===--- TFDeabstraction.h - TensorFlow Deabstraction Utilities ----------*- C++ -*-===//
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
// This defines the code that implements deabstraction for TensorFlow.
//
//===----------------------------------------------------------------------===//
#ifndef SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H
#define SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H

#include "TFConstExpr.h"
#include "TFUtilities.h"
#include "swift/AST/Module.h"

namespace swift {
namespace tf {

/// A helper class to launch deabstraction on functions.
class TFDeabstractionHelper {
public:
  TFDeabstractionHelper(SILTransform &transform, SILModule *module)
      : transform(transform), module(module), constantEvaluator(*module) {}
  /// Deabstract functions. If acceleratorOnly is set, tensorflow convention
  /// functions are deabstracted. Otherwise, all functions other than tensorflow
  /// convention functions are deabstracted,
  void deabstractFunctions(bool acceleratorOnly);

private:
  /// Deabstract the given function and returns true if this function was
  /// deabstracted. If the flag forceTFFunctions is true, forces partitioning of
  /// functions that operate on Tensors even if it would have been rejected
  /// otherwise.
  bool deabstract(SILFunction &fn, bool forceTFFunctions);

  SILTransform &transform;
  SILModule *module;
  ConstExprEvaluator constantEvaluator;
  TensorFunctionClassifier tfc;
};

} // namespace tf
} // namespace swift


#endif // SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H
