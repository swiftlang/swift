// Integer generic expression interface printingEnum case raw value expressions
// REQUIRES: swift_feature_LiteralExpressions
// RUN: %target-swift-frontend -emit-module -module-name IntegerGenericExpressionInterface -emit-module-interface-path %t/IntegerGenericExpressionInterface.swiftinterface -enable-library-evolution -swift-version 5 -disable-availability-checking -disable-experimental-parser-round-trip %s -enable-experimental-feature LiteralExpressions

// RUN: %FileCheck %s < %t/IntegerGenericExpressionInterface.swiftinterface

// Verify that integer generic expressions are constant-folded to plain
// integer literals in the emitted .swiftinterface file.

// CHECK-LABEL: public let add: Swift.InlineArray<5, Swift.Int>
public let add: InlineArray<(2 + 3), Int> = [1, 2, 3, 4, 5]
// CHECK-LABEL: public let sub: Swift.InlineArray<3, Swift.Int>
public let sub: InlineArray<(5 - 2), Int> = [1, 2, 3]
// CHECK-LABEL: public let mul: Swift.InlineArray<6, Swift.Int>
public let mul: InlineArray<(2 * 3), Int> = [1, 2, 3, 4, 5, 6]
// CHECK-LABEL: public let div: Swift.InlineArray<5, Swift.Int>
public let div: InlineArray<(10 / 2), Int> = [1, 2, 3, 4, 5]
// CHECK-LABEL: public let preced: Swift.InlineArray<4, (Swift.Int, Swift.Float)>
public let preced: InlineArray<(1 + 3 * 2 - 3), (Int, Float)> = [(1, 1.1), (2, 2.2), (3, 3.3), (4, 4.4)]
// CHECK-LABEL: public let nestedParens: Swift.InlineArray<10, Swift.Int>
public let nestedParens: InlineArray<((2 + 3) * 2), Int> = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
// CHECK-LABEL: public let nested: Swift.InlineArray<4, (Swift.InlineArray<2, Swift.Int>, Swift.Float)>
public let nested: InlineArray<(1 + 3 * 2 - 3), (InlineArray<(1+1), Int>, Float)> = [([1, 1], 1.1), ([2, 2], 2.2), ([3, 3], 3.3), ([4, 4], 4.4)]
// CHECK-LABEL: public let nestedBoth: Swift.InlineArray<2, Swift.InlineArray<3, Swift.Int>>
public let nestedBoth: InlineArray<(1 + 1), InlineArray<(2 + 1), Int>> = [[1, 2, 3], [4, 5, 6]]
// CHECK-LABEL: public let sugar: [5 of Swift.Int]
public let sugar: [(2 + 3) of Int] = [1, 2, 3, 4, 5]
// CHECK-LABEL: public let sugarShift: [4 of Swift.String]
public let sugarShift: [(1 << 2) of String] = ["a", "b", "c", "d"]
// CHECK-LABEL: public let sugarNested: [2 of [3 of Swift.Int]]
public let sugarNested: [(1 + 1) of [(2 + 1) of Int]] = [[1, 2, 3], [4, 5, 6]]


// CHECK-LABEL: public typealias PowerOfTwo = Swift.InlineArray<16, Swift.Int>
public typealias PowerOfTwo = InlineArray<(1 << 4), Int>
// CHECK-LABEL: public typealias ComputedSize = Swift.InlineArray<10, Swift.Int>
public typealias ComputedSize = InlineArray<((3 + 2) * (4 - 2)), Int>
// CHECK-LABEL: public typealias SugarAlias = [6 of Swift.Double]
public typealias SugarAlias = [(2 * 3) of Double]

// CHECK-LABEL: public func takeArray(_ arr: Swift.InlineArray<4, Swift.Int>)
public func takeArray(_ arr: InlineArray<(2 + 2), Int>) {}
// CHECK-LABEL: public func takeSugar(_ arr: [2 of Swift.Int])
public func takeSugar(_ arr: [(1 << 1) of Int]) {}
// CHECK-LABEL: public func returnArray() -> Swift.InlineArray<3, Swift.Int>
public func returnArray() -> InlineArray<(2 + 1), Int> { return [1, 2, 3] }
// CHECK-LABEL: public func returnSugar() -> [3 of Swift.String]
public func returnSugar() -> [(4 - 1) of String] { return ["a", "b", "c"] }

// CHECK-LABEL: public func takeGeneric<T>(_ arr: Swift.InlineArray<4, T>)
public func takeGeneric<T>(_ arr: InlineArray<(2 * 2), T>) {}
// CHECK-LABEL: public func returnGeneric<T>(_ t: T) -> Swift.InlineArray<3, T>
public func returnGeneric<T>(_ t: T) -> InlineArray<(1 + 2), T> {
  return InlineArray(repeating: t)
}

// CHECK-LABEL: public struct Container {
// CHECK-NEXT:    public var data: Swift.InlineArray<5, Swift.Int>
// CHECK-NEXT:    public var buffer: [4 of Swift.UInt8]
public struct Container {
  public var data: InlineArray<(3 + 2), Int>
  public var buffer: [(1 << 2) of UInt8]

  // CHECK:       public init(data: Swift.InlineArray<5, Swift.Int>, buffer: [4 of Swift.UInt8])
  public init(data: InlineArray<(3 + 2), Int>, buffer: [(1 << 2) of UInt8]) {
    self.data = data
    self.buffer = buffer
  }
// CHECK:       }
}

// CHECK-LABEL: public let plainLiteral: Swift.InlineArray<3, Swift.Int>
public let plainLiteral: InlineArray<3, Int> = [1, 2, 3]
// CHECK-LABEL: public let plainSugar: [4 of Swift.Int]
public let plainSugar: [4 of Int] = [1, 2, 3, 4]
