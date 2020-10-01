// RUN: %target-swift-frontend -emit-sil -verify %s

// Tests that differentiation features interact correctly with non-differentiation SILOptimizer
// passes.

import _Differentiation

// - MARK: DiagnoseInvalidEscapingCaptures

func nonEscapingUseOfDifferentiableFunction(_ f: @differentiable (Float) -> Float) {}
func testDiagnoseInvalidEscapingCaptures(_ f: @differentiable (Float) -> Float) {
  // Should not be diagnosed as invalid escaping capture.
  nonEscapingUseOfDifferentiableFunction { f($0) }
}
