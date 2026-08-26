// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest

let suite = TestSuite("Optional Noncopyable Additions")

if #available(StdlibDeploymentTarget 6.4, *) {
suite.test("Optional.ref") {
  var o = Optional<[Int]>.none
  do {
    let ref = o.ref
    expectNil(ref)
  }

  o = [1, 2, 3]
  do {
    let ref = o.ref
    if let ref = expectNotNil(ref) {
      expectEqual(ref.value.count, o?.count)
    }
  }
}
}

if #available(StdlibDeploymentTarget 6.4, *) {
suite.test("Optional.mutableRef") {
  var o = Optional<[Int]>.none
  do {
    let ref = o.mutableRef
    expectNil(ref)
  }

  o = [1, 2, 3]
  do {
    var ref = o.mutableRef
    if var ref = expectNotNil(ref) {
      expectEqual(ref.value.isEmpty, false)
      ref.value.removeAll()
      expectEqual(ref.value.isEmpty, true)
      ref.value.append(100)
    }
  }
  expectEqual(o?.count, 1)
  expectEqual(o?.first, 100)
}
}

if #available(StdlibDeploymentTarget 6.4, *) {
suite.test("Optional.put") {
  var o: Int? = nil
  var oRef = o.put(123)

  expectEqual(o, 123)
  expectEqual(oRef.value, 123)

  oRef.value = 67

  expectEqual(o, 67)
  expectEqual(oRef.value, 67)
}
}

runAllTests()
