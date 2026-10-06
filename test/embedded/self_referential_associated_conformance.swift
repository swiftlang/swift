// REQUIRES: swift_feature_Embedded

// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded | %FileCheck %s

// The `SubSequence: MyColl` associated conformance of `MySlice<Buf>: MyColl`
// is `MySlice<Buf>: MyColl` itself (like `Slice<Slice<X>>` being `Slice<X>`
// in the standard library). Specializing its witness table used to recurse
// infinitely.

public protocol MySeq { associatedtype Element }
public protocol MyColl: MySeq {
  associatedtype SubSequence: MyColl where SubSequence.Element == Element
  func slice() -> SubSequence
}
public struct MySlice<Base: MyColl>: MyColl {
  public typealias Element = Base.Element
  public var base: Base
  public init(base: Base) { self.base = base }
  public func slice() -> MySlice<Base> { self }
}
public struct Buf: MyColl {
  public typealias Element = UInt8
  public init() {}
  public func slice() -> MySlice<Buf> { MySlice(base: self) }
}
public func makeAny() -> any MyColl { MySlice(base: Buf()) }

// CHECK: @"$e5Repro7MySliceVyAA3BufVGAA0B4CollAAWP" = {{.*}}global [5 x ptr] [{{.*}}@"$e5Repro7MySliceVyAA3BufVGAA0B4CollAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
