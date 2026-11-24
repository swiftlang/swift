// A program links and runs with '@specialized(exported: true)' bodies from
// separately built libraries: A's own, and those that B and C, which don't
// define them, both export.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// '-wmo' turns on the default cross-module optimization, so that B and C can
// specialize A's bodies.
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) %t/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) %t/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t -L %t -lA -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift-dylib(%t/%target-library-name(C)) %t/B.swift -module-name C -emit-module -emit-module-path %t/C.swiftmodule -I %t -L %t -lA -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift %t/main.swift -I %t -L %t -lA -lB -lC -o %t/main %target-rpath(%t) -O -wmo
// RUN: %target-codesign %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C)
// RUN: %target-run %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_ExtendedSpecializedAttribute
// UNSUPPORTED: wasm
// UNSUPPORTED: swift_test_mode_optimize_none_with_implicit_dynamic

//--- A.swift

@specialized(exported: true, where T == Int)
public func twice<T: FixedWidthInteger>(_ x: T) -> T { x &+ x }

@specialized(exported: true, where T == Int)
public struct Pair<T> {
  public var a: T
  public init(_ a: T) { self.a = a }
  @inline(never) public func get() -> T { a }
}

public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }

public struct Box<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  @inline(never) public func get() -> T { v }
}

//--- B.swift

// Built as both B and C.
import A

@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(exported: true, where T == Int)
extension Box {}

//--- main.swift

import A
import B
import C

print(twice(21), Pair<Int>(3).get(), sum(20, 22), Box<Int>(7).get())
// CHECK: 42 3 42 7
