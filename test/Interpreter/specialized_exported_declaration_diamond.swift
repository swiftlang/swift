// Two modules export the same specializations of a third module's
// declarations. A program links both libraries (a "diamond": B and C depend on
// A, the program on B and C). Checks that it links and runs on each platform:
// the exported bodies are weak ('weak_odr'), so the duplicates must not be a
// link error.

// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// A's bodies must be serialized for B and C to specialize them: '-wmo' turns on
// the default cross-module optimization.
// RUN: %target-build-swift-dylib(%t/%target-library-name(A)) %t/A.swift -module-name A -emit-module -emit-module-path %t/A.swiftmodule -O -wmo
// RUN: %target-build-swift-dylib(%t/%target-library-name(B)) %t/B.swift -module-name B -emit-module -emit-module-path %t/B.swiftmodule -I %t -L %t -lA -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift-dylib(%t/%target-library-name(C)) %t/C.swift -module-name C -emit-module -emit-module-path %t/C.swiftmodule -I %t -L %t -lA -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-build-swift %t/main.swift -module-name main -I %t -L %t -lA -lB -lC -o %t/main %target-rpath(%t) -O -wmo -enable-experimental-feature ExtendedSpecializedAttribute
// RUN: %target-codesign %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C)
// RUN: %target-run %t/main %t/%target-library-name(A) %t/%target-library-name(B) %t/%target-library-name(C) | %FileCheck %s

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

@inline(never) public func useB(_ x: Int) -> Int { sum(x, x) &+ Box(x).get() }

//--- C.swift

import A

// The same specializations as B.
@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

@specialized(exported: true, where T == Int)
extension Box {}

@inline(never) public func useC(_ x: Int) -> Int { sum(x, x) &+ Box(x).get() }

//--- main.swift

import A
import B
import C

// The program calls the specialized bodies, which both B and C export, and
// each library calls its own.
print(sum(20, 22), Box(7).get(), useB(1), useC(2))
// CHECK: 42 7 3 6
