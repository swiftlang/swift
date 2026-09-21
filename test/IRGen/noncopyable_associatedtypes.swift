// RUN: %target-swift-frontend -module-name test %s -emit-ir > %t.ll
// RUN: %FileCheck %s --check-prefix=CHECK < %t.ll

// REQUIRES: PTRSIZE=64

public protocol P<A>: ~Copyable {
  associatedtype A: ~Copyable
}

// T.A: ~Copyable in G's conformance to P is redundant as P already declares A: ~Copyable,
// so two conditional requirements remain: T: P and T: ~Copyable.
public struct G<T>: ~Copyable where T: ~Copyable {}
extension G: P where T: P & ~Copyable, T.A: ~Copyable {
  public typealias A = T.A
}

// CHECK: @"$s4test1GVyxGAA1PA2aERzRi_z1ARj_zrlMc" ={{.*}}constant {

// ConformanceFlags == 0x20200 == 131584 == HasGenericWitnessTable (0x20000) | (NumConditionalRequirements=2 << 8)
// CHECK-SAME: i32 131584,

// ------------
// Req #1, T: P

// GenericRequirementFlags == Protocol(0) | HasKeyArgument(0x80) == 128
// CHECK-SAME: i32 128,

// mangled name of the subject type. "x" is the mangling for the first generic parameter T
// CHECK-SAME: @"symbolic x"

// relative reference to P's protocol descriptor.
// CHECK-SAME: @"$s4test1PMp"

// ------------
// Req #2, T: ~Copyable

// GenericRequirementFlags == InvertedProtocols(5)
// CHECK-SAME: i32 5,

// CHECK-SAME: @"symbolic x"

// paramIndex == 0 == T == 0th generic param of the conformance's signature)
// mask = 0x1 (Copyable bit set: suppress the implicit Copyable requirement on T).
// CHECK-SAME: i16 0, i16 1


// T.A: Copyable in H's conformance to P is NOT redundant. P doesn't require it but
// the conformance does. All three conditional requirements are emitted: T: P, T.A: Copyable, and T: ~Copyable
public struct H<T>: ~Copyable where T: ~Copyable {}
extension H: P where T: P & ~Copyable, T.A: Copyable {
  public typealias A = T.A
}

// CHECK: @"$s4test1HVyxGAA1PA2aERzRi_zrlMc" ={{.*}}constant {

// ConformanceFlags == 0x20300 == 131840 == HasGenericWitnessTable (0x20000) | (NumConditionalRequirements=3 << 8)
// CHECK-SAME: i32 131840,

// ------------
// Req #1, T: P: same as in G above.
// CHECK-SAME: i32 128,
// CHECK-SAME: @"symbolic x"
// CHECK-SAME: @"$s4test1PMp"

// ------------
// Req #2, T.A: Copyable is encoded with the "ignore checking all except" form.

// GenericRequirementFlags == InvertedProtocols(5)
// CHECK-SAME: i32 5,

// Param: mangled name for T.A aka (τ_0_0).A

// CHECK-SAME: @"symbolic 1A_____Qz 4test1PP"

// paramIndex = 0xFFFF == -1 == "subject is not a top-level generic param"
// mask = 0xFFFE == -2 == "ignore all but Copyable"
// CHECK-SAME: i16 -1, i16 -2

// ------------
// Req #3, T: ~Copyable: same in G above.
// CHECK-SAME: i32 5,
// CHECK-SAME: @"symbolic x"
// CHECK-SAME: i16 0, i16 1
