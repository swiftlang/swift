// RUN: %empty-directory(%t)
// RUN: %target-build-swift -Xfrontend -disable-availability-checking %s -module-name main -o %t/a.out
// RUN: %target-codesign %t/a.out
// RUN: %target-run %t/a.out
// REQUIRES: executable_test
// REQUIRES: PTRSIZE=64
// UNSUPPORTED: use_os_stdlib
// UNSUPPORTED: back_deployment_runtime

// SILGen passes an address where a borrowed InlineArray value is expected.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

// Ensure that various size computations trap when overflowing the 32-bit or
// 64-bit values they're stored in, rather than wrapping and producing an
// incorrect small value.

import Swift
import StdlibUnittest

let tests = TestSuite("LayoutOverflow")

func sizeOf<T>(_: T.Type) -> Int { MemoryLayout<T>.size }
func strideOf<T>(_: T.Type) -> Int { MemoryLayout<T>.stride }

struct TwoFields<let N: Int> {
  var a: InlineArray<N, Int>
  var b: InlineArray<N, Int>
}

class BigClass<let N: Int> {
  var a = InlineArray<N, Int>(repeating: 0)
  var b = 0
}

tests.test("nested Builtin.FixedArray whose cumulative stride overflows") {
  expectCrashLater(
    withMessage:
      "Builtin.FixedArray of 2097152 elements with stride 35184372088832 is too large to be representable")
  _ = _typeByName("$2097151_$2097151_$2097151_SiBVBVBV")
}

tests.test("Builtin.FixedArray whose stride times count overflows in one step") {
  expectCrashLater(
    withMessage:
      "Builtin.FixedArray of 2147483647 elements with stride 17179869176 is too large to be representable")
  _ = _typeByName("$2147483646_$2147483646_SiBVBV")
}

tests.test("genuine Builtin.FixedArray layouts are unaffected") {
  let t = _typeByName("$3_SiBV")!
  expectEqual(4 * MemoryLayout<Int>.stride, _openExistential(t, do: sizeOf))
  expectEqual(4 * MemoryLayout<Int>.stride, _openExistential(t, do: strideOf))
}

tests.test("tuple aggregating two representable-but-unsummable fields") {
  expectCrashLater(
    withMessage: "tuple has a layout size that is too large to be representable")
  _ = _typeByName("$1073741823_$1073741823_SiBVBV_$1073741823_$1073741823_SiBVBVt")
}

tests.test("struct field offset exceeding the 32-bit field offset vector") {
  expectCrashLater(
    withMessage:
      "struct TwoFields has a field offset that does not fit in the 32-bit field offset vector")
  _ = _typeByName("4main9TwoFieldsVy$2147483646_G")
}

tests.test("class instance size exceeding the 32-bit InstanceSize field") {
  expectCrashLater(
    withMessage: "class BigClass has an instance size of")
  _ = _typeByName("4main8BigClassCy$2147483646_G")
}

tests.test("ordinary generic struct and class layouts still work") {
  let s = _typeByName("4main9TwoFieldsVy$3_G")!
  expectEqual(2 * 4 * MemoryLayout<Int>.stride,
              _openExistential(s, do: sizeOf))
  expectNotNil(_typeByName("4main8BigClassCy$3_G"))
}

tests.test("InlineArray of a representable size is still usable") {
  var array = InlineArray<128, Int>(repeating: 7)
  expectEqual(128 * MemoryLayout<Int>.stride,
              MemoryLayout<InlineArray<128, Int>>.size)
  expectEqual(7, array[0])
  array[127] = 9
  expectEqual(9, array[127])
}

runAllTests()
