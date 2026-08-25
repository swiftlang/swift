// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -enable-experimental-feature ValueGenerics -enable-experimental-feature MoveOnlyTuples -O -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop %target-embedded-posix-shim) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ValueGenerics
// REQUIRES: swift_feature_MoveOnlyTuples

// Specializing an apply with an indirect result trips a lowered-addresses
// assumption in replaceWithSpecializedCallee (the `VoidVal->getType().isVoid()`
// assertion in fixUsedVoidType). That is pre-existing and unrelated to deinits;
// specializing the element deinits of an InlineArray or tuple is just the first
// thing in this configuration to reach that code.
// XFAIL: swift_test_mode_optimize_none_with_opaque_values

var deinits = 0

struct Element<T>: ~Copyable {
  var p: UnsafeMutablePointer<Int>
  init() { p = .allocate(capacity: 1) }
  deinit {
    deinits += 1
    p.deallocate()
  }
}

@export(interface)
struct ArrayBox: ~Copyable {
  var a: InlineArray<2, Element<Int>>
  init() { a = InlineArray<2, Element<Int>>(first: Element<Int>()) { _ in Element<Int>() } }
}

@export(interface)
struct TupleBox: ~Copyable {
  var t: (Element<Int>, Int)
  init() { t = (Element<Int>(), 1) }
}

@main
struct Main {
  static func main() {
    do { let b = ArrayBox(); _ = b.a.count }
    print("after array: \(deinits)")
    // CHECK: after array: 2

    do { let b = TupleBox(); _ = b.t.1 }
    print("after tuple: \(deinits)")
    // CHECK: after tuple: 3
  }
}
