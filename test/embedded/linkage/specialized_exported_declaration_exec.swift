// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -emit-module -o %t/A.o %t/A.swift -emit-module-path %t/A.swiftmodule -parse-as-library -enable-experimental-feature Embedded -O
// RUN: %target-swift-frontend -c -emit-module -o %t/B.o %t/B.swift -I %t -emit-module-path %t/B.swiftmodule -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O
// RUN: %target-swift-frontend -c -o %t/B2.o %t/B2.swift -I %t -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O
// RUN: %target-swift-frontend -c -o %t/Main.o %t/Main.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/A.o %t/B.o %t/B2.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- A.swift

public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T { a &+ b }

public protocol Shape { func area() -> Int }

public struct SBox<T> { public var v: T; public init(_ v: T) { self.v = v } }
extension SBox: Shape { public func area() -> Int { 10 } }

open class CBox<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  open func get() -> T { v }
  deinit { deinitCount += 1 }
}
nonisolated(unsafe) public var deinitCount = 0

//--- B.swift

import A

@specialized(exported: true, where T == Int)
extension SBox {}

@specialized(exported: true, where T == Int)
extension CBox {}

@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

public func makeAny() -> Any { SBox(4) }

//--- B2.swift

import A

// The same specialization again: B and B2 link together.
@specialized(exported: true, where T == Int)
public func sum<T: FixedWidthInteger>(_ a: T, _ b: T) -> T

//--- Main.swift

import A
import B

final class Sub: CBox<Int> { var extra = 1 }

func test() -> String {
  let shape: any Shape = SBox(1)
  let c: CBox<Int> = Sub(6)
  let cast = makeAny() as? SBox<Int>
  return "\(shape.area()) \(cast?.v ?? -1) \(c.get()) \(sum(20, 22))"
}

print("\(test()) \(deinitCount)")
// CHECK: 10 4 6 42 1
