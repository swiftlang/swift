// RUN: %empty-directory(%t)
// RUN: split-file %s %t

// RUN: %target-swift-frontend -c -emit-module -o %t/Library.o %t/Library.swift -emit-module-path %t/Library.swiftmodule -parse-as-library -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -O
// RUN: %target-swift-frontend -c -o %t/Main.o %t/Main.swift -I %t -enable-experimental-feature Embedded -enable-experimental-feature ExtendedSpecializedAttribute -Onone
// RUN: %target-embedded-link %target-clang-resource-dir-opt %t/Main.o %t/Library.o -o %t/a.out
// RUN: %target-run %t/a.out | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_ExtendedSpecializedAttribute

//--- Library.swift

public protocol Shape { func area() -> Int }

@specialized(exported: true, where T == Int)
public struct SBox<T> { public var v: T; public init(_ v: T) { self.v = v } }
extension SBox: Shape { public func area() -> Int { 10 } }

@specialized(exported: true, where T == Int)
open class CBox<T> {
  public var v: T
  public init(_ v: T) { self.v = v }
  open func get() -> T { v }
  deinit { deinitCount += 1 }
}
nonisolated(unsafe) public var deinitCount = 0
extension CBox: Shape { public func area() -> Int { 20 } }

public func isSBox(_ a: Any) -> Bool { (a as? SBox<Int>) != nil }
public func isCBox(_ a: AnyObject) -> Bool { a is CBox<Int> }

//--- Main.swift

import Library

final class Sub: CBox<Int> { var extra = 1 }

func test() -> String {
  let shapes: [any Shape] = [SBox(1), CBox(2)]
  let c: CBox<Int> = Sub(6)
  return "\(shapes[0].area()) \(shapes[1].area()) \(isSBox(SBox(4))) \(isCBox(Sub(5))) \(c.get())"
}

print("\(test()) \(deinitCount)")
// CHECK: 10 20 true true 6 3
