// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver  -target %target-swift-5.1-abi-triple %s -o %t/out
// RUN: %target-codesign %t/out
// RUN: %target-run %t/out

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

import ObjectiveC
import _Concurrency
import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("Actor.SubClass.Metatype")

actor Actor5<T> {
  var state: T
  init(state: T) { self.state = state }
}

Tests.test("base generic class")
  .code {
  let x = Actor5(state: 5)
  print(type(of: x))
}
