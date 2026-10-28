// REQUIRES: swift_feature_Embedded

// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -DEXPLICIT_EXPORT | %FileCheck %s

// `S: P` has a strongly emitted witness table (@export(interface)). Its
// `SubSequence` entry needs the specialized witness table of `Wrapper<S>: P`,
// whose `Index` entry in turn needs the table of the non-generic `Idx: Q`.

public protocol Q {
  func q() -> Int
}

extension Q {
  public func q() -> Int { 42 }
}

public protocol P {
  associatedtype SubSequence: P
  associatedtype Index: Q
  func slice() -> SubSequence
}

open class Idx: Q {
  public init() {}
}

public struct SIdx: Q {
  public func q() -> Int { 1 }
}

public struct Wrapper<Base: P>: P {
  public typealias Index = Idx
  public var base: Base
  public init(base: Base) { self.base = base }
  public func slice() -> Wrapper<Base> { self }
}

#if EXPLICIT_EXPORT
@export(interface)
#endif
public struct S: P {
  public typealias Index = SIdx
  public init() {}
  public func slice() -> Wrapper<S> { Wrapper(base: self) }
}

// CHECK-DAG: @"$e5Repro7WrapperVyAA1SVGAA1PAAWP" = {{.*}}global {{.*}}@"$e5Repro3IdxCAA1QAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
// CHECK-DAG: @"$e5Repro3IdxCAA1QAAWP" = {{.*}}constant [2 x ptr] [ptr null, ptr @"$e5Repro3IdxCAA1QA2aDP1qSiyFTWAC_TGq5{{(\.ptrauth(\.[0-9]+)?)?}}"]
