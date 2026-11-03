// RUN: %empty-directory(%t)
// RUN: %target-run-simple-swift

// REQUIRES: executable_test
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

import StdlibUnittest

let suite = TestSuite("Optional Noncopyable Additions")

suite.test("Optional.ref")
.require(.minimumStdlib(.stdlib_6_4)) {
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

suite.test("Optional.mutableRef")
.require(.minimumStdlib(.stdlib_6_4)) {
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

runAllTests()
