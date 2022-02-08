// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=on -requirement-machine-inferred-signatures=on 2>&1 | %FileCheck %s

struct MyOptionSet : OptionSet {
  let rawValue: UInt
}

struct G1<T : OptionSet> where T.RawValue: FixedWidthInteger & UnsignedInteger {}

// CHECK-LABEL: ExtensionDecl line={{[0-9]+}} base=G1
// CHECK-NEXT: Generic signature: <T where T == MyOptionSet>
extension G1 where T == MyOptionSet {}

struct G2<T : SIMD> {}

// CHECK-LABEL: ExtensionDecl line={{[0-9]+}} base=G2
// CHECK-NEXT: Generic signature: <T where T == SIMD2<Double>>
extension G2 where T == SIMD2<Double> {}

struct G3<T : StringProtocol> {}

// CHECK-LABEL: ExtensionDecl line={{[0-9]+}} base=G3
// CHECK-NEXT: Generic signature: <T where T == String>
extension G3 where T == String {}
