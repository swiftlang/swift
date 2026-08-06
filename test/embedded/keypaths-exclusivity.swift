// Key path mutations that cross a class boundary must enforce exclusive access
// to the projected address, matching non-embedded Swift. The embedded
// multi-component `ReferenceWritableKeyPath` walker brackets the access with
// `ClassHolder`, whose `beginUnpairedModifyAccess` / `endUnpairedAccess` pair
// IRGen lowers to `swift_beginAccess` / `swift_endAccess` when dynamic
// exclusivity is enabled (and drops entirely when it is not).

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// With dynamic exclusivity enabled, a conflicting mutation traps.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -enforce-exclusivity=checked -parse-as-library -wmo -O %t/conflict.swift -module-name conflict -c -o %t/conflict.o
// RUN: %target-embedded-link %t/conflict.o -o %t/conflict.out %target-embedded-single-threaded-shim -dead_strip
// RUN: %target-run not --crash %t/conflict.out

// Legitimate mutations through a class boundary must not trap, at either
// optimization level.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -enforce-exclusivity=checked -parse-as-library -wmo -O %t/ok.swift -module-name ok -c -o %t/ok-o.o
// RUN: %target-embedded-link %t/ok-o.o -o %t/ok-o.out %target-embedded-single-threaded-shim -dead_strip
// RUN: %target-run %t/ok-o.out | %FileCheck %s

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -enforce-exclusivity=checked -parse-as-library -wmo -Onone %t/ok.swift -module-name ok -c -o %t/ok-onone.o
// RUN: %target-embedded-link %t/ok-onone.o -o %t/ok-onone.out %target-embedded-single-threaded-shim -dead_strip
// RUN: %target-run %t/ok-onone.out | %FileCheck %s

// The access calls must be emitted when dynamic exclusivity is on, and dropped
// by IRGen when it is off. Check this on conflict.swift: in ok.swift the
// accesses are provably non-conflicting, so at -O the access-enforcement
// optimizer removes them and the check would pass vacuously.
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -enable-experimental-feature EmbeddedDynamicExclusivity -enforce-exclusivity=checked -parse-as-library -wmo -O %t/conflict.swift -module-name conflict -emit-ir -o - | %FileCheck -check-prefix=CHECK-EXCL %s
// CHECK-EXCL: call void @swift_beginAccess
// CHECK-EXCL: call void @swift_endAccess

// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/conflict.swift -module-name conflict -emit-ir -o - | %FileCheck -check-prefix=CHECK-NOEXCL %s
// CHECK-NOEXCL-NOT: swift_beginAccess

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_EmbeddedDynamicExclusivity

//--- conflict.swift

final class Ref { var value: Int32 = 0 }
struct Holder { var r = Ref() }

func conflict(_ x: inout Int32, _ y: inout Int32) { x = 1; y = 2 }

@main
struct M {
  static func main() {
    let kp: ReferenceWritableKeyPath<Holder, Int32> = \.r.value
    var h = Holder()
    // Two overlapping `modify` accesses to the same projected address. This is
    // not statically diagnosable because it goes through the key path
    // subscript, so it must be caught dynamically.
    conflict(&h[keyPath: kp], &h[keyPath: kp])
  }
}

//--- ok.swift

final class Ref { var value: Int32 = 0 }
struct Holder { var r = Ref() }
struct Outer { var h = Holder() }
final class Cell { var v: Int32 = 0; var inner = Ref() }

@main
struct M {
  static func main() {
    // stored -> class -> stored
    var h = Holder()
    let kp: ReferenceWritableKeyPath<Holder, Int32> = \.r.value
    h[keyPath: kp] = 7
    print(h.r.value == 7 ? "OK!" : "FAIL") // CHECK: OK!

    // stored -> stored -> class -> stored
    var o = Outer()
    let kp2: ReferenceWritableKeyPath<Outer, Int32> = \.h.r.value
    o[keyPath: kp2] = 11
    print(o.h.r.value == 11 ? "OK!" : "FAIL") // CHECK: OK!

    // class root -> class -> stored (two class boundaries)
    let c = Cell()
    let kp3: ReferenceWritableKeyPath<Cell, Int32> = \.inner.value
    c[keyPath: kp3] = 13
    print(c.inner.value == 13 ? "OK!" : "FAIL") // CHECK: OK!

    // Sequential mutations through the same key path must not conflict with
    // each other — each access has to be ended before the next begins.
    for i in Int32(1)...5 { h[keyPath: kp] = i }
    print(h.r.value == 5 ? "OK!" : "FAIL") // CHECK: OK!

    // Read after write through the same key path.
    print(h[keyPath: kp] == 5 ? "OK!" : "FAIL") // CHECK: OK!
  }
}
