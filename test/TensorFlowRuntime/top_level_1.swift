// RUN: %target-run-simple-swift
// REQUIRES: executable_test

// This tests various issues with top level code, ensuring deabstraction and
// other things work here.  This is not intended to be a place to test device
// specifics, so we can keep it simple and just test CPU.

import TensorFlow
import StdlibUnittest

_RuntimeConfig.usesTFEagerAPI = false
_RuntimeConfig.executionMode = .cpu
_RuntimeConfig.printsDebugLog = false

var x = Tensor<Int8>([1,2,3])*2

x = x + x

expectEqual(x.array, ShapedArray(shape: [3], scalars: [2,4,6]))
