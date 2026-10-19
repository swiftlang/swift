// REQUIRES: swift_feature_Embedded

// RUN: %target-swift-frontend -emit-sil %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface | %FileCheck %s --check-prefix=SIL
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -enable-experimental-feature CodeGenerationModel=interface | %FileCheck %s --check-prefix=IR
// RUN: %target-swift-frontend -emit-ir %s -parse-as-library -wmo -module-name Repro -enable-experimental-feature Embedded -DEXPLICIT_EXPORT | %FileCheck %s --check-prefix=IR

// The explicit `typealias SubSequence = Wrapper<S>` makes the associated
// conformance `SubSequence: P` of `S: P` refer to `Wrapper<S>: P` through type
// sugar, while other references to it are canonical. These are distinct
// conformance objects for the same conformance, and they share a single
// specialized witness table and symbol.

public protocol P {
  associatedtype SubSequence: P
  func slice() -> SubSequence
}

public struct Wrapper<Base: P>: P {
  public var base: Base
  public init(base: Base) { self.base = base }
  public func slice() -> Wrapper<Base> { self }
}

#if EXPLICIT_EXPORT
@export(interface)
#endif
public struct S: P {
  public typealias SubSequence = Wrapper<S>
  public init() {}
  public func slice() -> Wrapper<S> { Wrapper(base: self) }
}

// SIL-COUNT-1: sil_witness_table shared [specialized] Wrapper<S>: specialize <S>
// SIL-NOT: sil_witness_table shared [specialized] Wrapper<S>: specialize <S>

// IR-DAG: @"$e5Repro1SVAA1PAAWP" = {{.*}}global {{.*}}@"$e5Repro7WrapperVyAA1SVGAA1PAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
// IR-DAG: @"$e5Repro7WrapperVyAA1SVGAA1PAAWP" = {{.*}}global [4 x ptr] [ptr null, ptr @"$e5Repro7WrapperVyAA1SVGAA1PAAWP{{(\.ptrauth(\.[0-9]+)?)?}}"
