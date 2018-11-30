//===--- TFDeabstraction.h - TensorFlow deabstraction ----------*- C++ -*-===//
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
// This file defines the interface to invoke deabstraction on a function.
//
//===----------------------------------------------------------------------===//

#ifndef SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H
#define SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H

#include "TFConstExpr.h"
#include "TFUtilities.h"
#include "swift/SIL/ApplySite.h"
#include "swift/SIL/SILModule.h"
#include "swift/SILOptimizer/PassManager/Transforms.h"

namespace swift {
namespace tf {

/// A helper class to launch deabstraction on functions.
class TFDeabstractionHelper {
public:
  TFDeabstractionHelper(SILTransform &transform, SILModule *module)
      : transform(transform), module(module), constantEvaluator(*module) {}

  /// Deabstract the given function and returns true if this function was
  /// deabstracted. If the flag forceTFFunctions is true, forces partitioning of
  /// functions that operate on Tensors even if it would have been rejected
  /// otherwise.
  bool deabstract(SILFunction &fn, bool forceTFFunctions);

  /// Deabstract all tensorflow convention functions.
  void deabstractAcceleratorOnlyFunctions();

  /// Returns true if this is special callee that should not be inlined for TF analysis.
  static bool isSpecialNoInlineCallee(FullApplySite site, const SILFunction& callee);

private:
  SILTransform &transform;
  SILModule *module;
  ConstExprEvaluator constantEvaluator;
  TensorFunctionClassifier tfc;
};

} // namespace tf
} // namespace swift
#endif  // SWIFT_SILOPTIMIZER_MANDATORY_TFDEABSTRACTION_H
