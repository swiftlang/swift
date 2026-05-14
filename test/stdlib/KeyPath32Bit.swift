// RUN: %target-run-simple-swift
// RUN: %target-run-simple-swift(-O)
// REQUIRES: executable_test
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// Regression test for a 32-bit-only mask-overlap bug in `ComputedArgumentSize`
// (`stdlib/public/core/KeyPath.swift`).

import StdlibUnittest

let tests = TestSuite("KeyPath32Bit")

struct Inner {
  let base: Int
  subscript(idx: SIMD4<Int32>) -> Int {
    return base + Int(idx[0]) + Int(idx[1]) + Int(idx[2]) + Int(idx[3])
  }
}

struct Outer {
  var inner: Inner
}

tests.test("appended keypath with 16-byte-aligned subscript argument") {
  // Precondition: SIMD4<Int32> must have 16-byte alignment on this target.
  // If a future stdlib change demotes the alignment, this test would stop
  // exercising the bug's codepath — assert loudly instead of silently.
  expectEqual(16, MemoryLayout<SIMD4<Int32>>.alignment)
  expectEqual(16, MemoryLayout<SIMD4<Int32>>.size)

  let arg = SIMD4<Int32>(1, 2, 3, 4)

  // The leaf keypath captures a 16-aligned argument.
  let leaf: KeyPath<Inner, Int> = \Inner.[arg]

  // `appending(path:)` forces the buggy `calculateAppendedKeyPathSize` /
  // `_storeInto` paths that read `.alignment` back from the leaf component's
  // header. Under the buggy mask, the read returns 0; the appended buffer is
  // under-allocated; projection traps inside the stdlib.
  let composed: KeyPath<Outer, Int> = (\Outer.inner).appending(path: leaf)

  // Normal keypath projection. On a correct stdlib this returns the expected
  // value. On a buggy stdlib it traps before returning, which lit records as
  // a non-zero exit code (test FAIL).
  let observed = Outer(inner: Inner(base: 100))[keyPath: composed]

  // 100 (base) + 1 + 2 + 3 + 4 = 110.
  expectEqual(110, observed)
}

runAllTests()
