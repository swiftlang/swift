// A program that links B only through D's import of it uses B's exported
// specialized bodies, and links and runs on each platform.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A's bodies must be serialized for B to specialize them: '-wmo' turns on the
// default cross-module optimization. B has a link name, so that importing it
// autolinks it.
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) %t/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule -O -wmo
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) %t/B.swift -module-name B -module-link-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t -L %t -lA -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift-dylib(%t/%target-library-name(D)) %t/D.swift -module-name D -emit-module -emit-module-path %t/D.swiftmodule -I %t -L %t -lA -lB -O -wmo
// RUN: %target-build-swift %t/main.swift -module-name main -I %t -L %t -lA -lD -o %t/main %target-rpath(%t) -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-codesign %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(D)
// RUN: %target-run %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(D) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- A.swift

public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }

public struct Box<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  public func get() -> T { v }
}

//--- B.swift

import A

@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(exported: true, where T == Int)
extension Box {}

//--- D.swift

import B

public func d() -> Int { 1 }

//--- main.swift

import A
import D

// 'main' doesn't import B, but links it through D, so it calls B's bodies.
print(sum(20, 22), Box(7).get(), d())
// CHECK: 42 7 1
