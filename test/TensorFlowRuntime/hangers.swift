// RUN: %target-run-simple-swift
// RUN: %target-run-dynamic-compilation-swift
// REQUIRES: executable_test
// REQUIRES: swift_test_mode_optimize
//
// Compiler-only testing for TPU graph lowering (e.g. shape requirements by XLA).
// TODO: enable this after https://github.com/apple/swift/pull/18458 is submitted.
// UN: %target-swift-frontend -Xllvm -tf-dump-intermediates -Xllvm -tf-dump-graph -Xllvm -tf-target-tpu -O -emit-sil %s >/dev/null

// Test cases that used to hang due to bugs (e.g. insufficient control
// dependency edges in graph lowering).

import TensorFlow
#if TPU
import TensorFlowUnittestTPU
#else
import TensorFlowUnittest
#endif
import StdlibUnittest

var HangersTests = TestSuite("Hangers")

// The top level graph function returns a result tensor `x` -- make sure we hook
// up control dependency from that result value to the While/If node in the
// body, so that the latter node gets run, producing the intended side effect of
// enqueuing tensor `images` for swift host to dequeue from.
//
// An If node can be created instead of While because of swift compiler's code
// transformation.
@inline(never)
public func SR8443(n: Int32) {
  var i: Int32 = 0
// expected-warning @+1 {{implicitly copied to the accelerator}}
  while i < n { // expected-note {{value used here}}

    // expected-warning @+1 {{implicitly copied to the host}}
    let images = Tensor<Float>(0.0)
    _hostOp(images)
    i += 1
  }

  let x = Tensor<Float>(1.0)
  _hostOp(x)
}
HangersTests.testAllBackends("SR8443") {
  SR8443(n: 2)
}

runAllTests()
