// REQUIRES: swift_feature_Embedded

// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface | %FileCheck %s --implicit-check-not='WP" = external'
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -DEXPLICIT_EXPORT | %FileCheck %s --implicit-check-not='WP" = external'

// When a conformance gets a strongly emitted witness table (via
// @export(interface) or CodeGenerationModel=interface), the witness tables
// for its associated conformances must be emitted too.

public protocol MySeq {
  associatedtype Element
}

public protocol MyColl: MySeq {
  associatedtype SubSequence: MyColl where SubSequence.Element == Element
  func slice() -> SubSequence
}

public protocol MyBidi: MyColl where SubSequence: MyBidi {
  func before(_ i: Int) -> Int
}

public struct MySlice<Base: MyColl>: MyColl {
  public typealias Element = Base.Element
  public var base: Base
  public init(base: Base) { self.base = base }
  public func slice() -> MySlice<Base> { self }
}

extension MySlice: MyBidi where Base: MyBidi {
  public func before(_ i: Int) -> Int { base.before(i) }
}

#if EXPLICIT_EXPORT
@export(interface)
#endif
public struct Buf: MyBidi {
  public typealias Element = UInt8
  public init() {}
  public func slice() -> MySlice<Buf> { MySlice(base: self) }
  public func before(_ i: Int) -> Int { i - 1 }
}

// CHECK-DAG: @"$e5Repro3BufVAA6MyCollAAWP" = {{.*}}global {{.*}}@"$e5Repro7MySliceVyAA3BufVGAA0B4CollAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
// CHECK-DAG: @"$e5Repro3BufVAA6MyBidiAAWP" = {{.*}}global {{.*}}@"$e5Repro7MySliceVyAA3BufVGAA0B4BidiAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
// CHECK-DAG: @"$e5Repro7MySliceVyAA3BufVGAA0B4CollAAWP" = {{.*}}{{global|constant}} [
// CHECK-DAG: @"$e5Repro7MySliceVyAA3BufVGAA0B4BidiAAWP" = {{.*}}{{global|constant}} [
