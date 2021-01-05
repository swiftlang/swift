// RUN: %empty-directory(%t)
// RUN: %target-swiftc_driver -Xfrontend -enable-experimental-concurrency %s -o %t/out
// RUN: %target-run %t/out

// REQUIRES: concurrency
// REQUIRES: objc_interop
// REQUIRES: executable_test

import ObjectiveC
import _Concurrency
import StdlibUnittest

defer { runAllTests() }

var Tests = TestSuite("Actor.SubClass.Metatype")

actor class Actor5<T> {
  var state: T
  init(state: T) { self.state = state }
}

Tests.test("base generic class")
  .code {
  let x = Actor5(state: 5)
  print(type(of: x))
}

class Actor6<T> : Actor5<T> {
  override init(state: T) { super.init(state: state) }
}

Tests.test("non-final sub-generic class parent generic class crash")
  .code {
  let x = Actor6(state: 5)
  print(type(of: x))
}

final class Actor6Final<T> : Actor5<T> {
  override init(state: T) { super.init(state: state) }
}

Tests.test("final sub-generic class parent generic class crash")
  .code {
  let x = Actor6Final(state: 5)
  print(type(of: x))
}
