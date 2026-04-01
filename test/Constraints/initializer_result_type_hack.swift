// RUN: %target-swift-emit-silgen %s -solver-disable-performance-hacks | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-NO-HACKS
// RUN: %target-swift-emit-silgen %s -solver-enable-performance-hacks | %FileCheck %s --check-prefix=CHECK --check-prefix=CHECK-HACKS

// This was reduced from a bit of code in Foundation that was calling the wrong
// overload by mistake:
//
// https://github.com/swiftlang/swift-foundation/issues/1779

struct BitArray: ExpressibleByStringLiteral {
  init(stringLiteral: String) { fatalError() }
}

struct S1 {}
struct S2 {}
struct S3 {}
struct S4 {}
struct S5 {}
struct S6 {}

extension UInt16 {
  init(_: BitArray) { fatalError() }
  init(_: S1) { fatalError() }
  init(_: S2) { fatalError() }
  init(_: S3) { fatalError() }
  init(_: S4) { fatalError() }
  init(_: S5) { fatalError() }
  init(_: S6) { fatalError() }
}

// CHECK-LABEL: sil hidden [ossa] @$s28initializer_result_type_hack22testOverloadResolutionyyF : $@convention(thin) () -> () {
// CHECK-NO-HACKS: function_ref @$ss17FixedWidthIntegerPsEyxSgSScfC : $@convention(method) <τ_0_0 where τ_0_0 : FixedWidthInteger> (@owned String, @thick τ_0_0.Type) -> @out Optional<τ_0_0>
// CHECK-NO-HACKS: witness_method $Optional<UInt16>, #Equatable."=="
// CHECK-HACKS: function_ref @$s28initializer_result_type_hack8BitArrayV13stringLiteralACSS_tcfC : $@convention(method) (@owned String, @thin BitArray.Type) -> BitArray // user: %16
// CHECK-HACKS: function_ref @$ss6UInt16V2eeoiySbAB_ABtFZ : $@convention(method) (UInt16, UInt16, @thin UInt16.Type) -> Bool // user: %20
// CHECK: return
func testOverloadResolution() {
    let x: UInt16 = 0
    _ = (x == UInt16("\n"))
}

