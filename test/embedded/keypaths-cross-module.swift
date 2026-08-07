// Key paths that reference a computed property or subscript from *another*
// module. Outside embedded Swift, SILGen marks such a component "external", so
// its layout is deferred to a property descriptor in the defining module. That
// mechanism exists to survive resilient changes to the defining module, which
// can't happen in embedded Swift -- library evolution is rejected outright, and
// IRGen deliberately emits no property descriptors. So the component is always
// fully describable in the client, and marking it external would only produce a
// dangling reference. See `SILGenModule::emitKeyPathComponentForDecl`.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O %t/Lib.swift -module-name Lib -emit-module -emit-module-path %t/Lib.swiftmodule -c -o %t/Lib.o
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -wmo -O -I %t %t/main.swift -module-name main -c -o %t/main.o
// RUN: %target-embedded-link %t/Lib.o %t/main.o -o %t/a.out %target-clang-resource-dir-opt -dead_strip
// RUN: %target-run %t/a.out | %FileCheck %s
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx
// REQUIRES: swift_feature_Embedded

//--- Lib.swift
public struct Box {
  public var stored: Int32 = 0
  public var computed: Int32 {
    get { stored &+ 1 }
    set { stored = newValue &- 1 }
  }
  public subscript(i: Int) -> Int32 {
    get { stored &+ Int32(i) }
    set { stored = newValue &- Int32(i) }
  }
  public init() {}
}

// `@export(interface)` withholds the *implementation* from clients, but not
// the accessor symbols or the type's layout, so it doesn't reintroduce a need
// for external components either: the client still emits its own key path
// thunks, which call these accessors as ordinary cross-module calls.
public struct Hidden {
  public var stored: Int32 = 0
  @export(interface)
  public var computed: Int32 {
    get { stored &+ 1 }
    set { stored = newValue &- 1 }
  }
  @export(interface)
  public subscript(i: Int) -> Int32 {
    get { stored &+ Int32(i) }
    set { stored = newValue &- Int32(i) }
  }
  public init() {}
}

//--- main.swift
import Lib
@inline(never) func kpStored() -> WritableKeyPath<Box, Int32> { \Box.stored }
@inline(never) func kpComputed() -> WritableKeyPath<Box, Int32> { \Box.computed }
@inline(never) func kpSub(_ i: Int) -> WritableKeyPath<Box, Int32> { \Box[i] }
@inline(never) func kpHidden() -> WritableKeyPath<Hidden, Int32> { \Hidden.computed }
@inline(never) func kpHiddenSub(_ i: Int) -> WritableKeyPath<Hidden, Int32> { \Hidden[i] }
@inline(never) func opaque(_ x: Int) -> Int { x }

@main struct M {
  static func main() {
    var b = Box()
    b.stored = 10
    print(b[keyPath: kpStored()] == 10 ? "OK!" : "FAIL")   // CHECK: OK!
    print(b[keyPath: kpComputed()] == 11 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    b[keyPath: kpComputed()] = 20
    print(b.stored == 19 ? "OK!" : "FAIL")                 // CHECK-NEXT: OK!
    print(b[keyPath: kpSub(3)] == 22 ? "OK!" : "FAIL")     // CHECK-NEXT: OK!
    b[keyPath: kpSub(3)] = 30
    print(b.stored == 27 ? "OK!" : "FAIL")                 // CHECK-NEXT: OK!
    print(kpSub(opaque(2)) == kpSub(opaque(2)) ? "OK!" : "FAIL") // CHECK-NEXT: OK!
    print(kpSub(opaque(2)) != kpSub(opaque(5)) ? "OK!" : "FAIL") // CHECK-NEXT: OK!

    var h = Hidden()
    h.stored = 10
    print(h[keyPath: kpHidden()] == 11 ? "OK!" : "FAIL")   // CHECK-NEXT: OK!
    h[keyPath: kpHidden()] = 20
    print(h.stored == 19 ? "OK!" : "FAIL")                 // CHECK-NEXT: OK!
    print(h[keyPath: kpHiddenSub(3)] == 22 ? "OK!" : "FAIL") // CHECK-NEXT: OK!
  }
}
