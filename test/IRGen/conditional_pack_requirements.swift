// RUN: %target-swift-frontend -emit-ir %s -target %target-swift-5.9-abi-triple | %FileCheck %s

public protocol P {
  associatedtype A
}

public protocol Q {}

public class C<each T> {}

public struct GG1<A: P, each B: P> where A.A == C<repeat (each B).A> {}

extension GG1: Q where A: Q, repeat each B: Q {}

// This mangling is incorrect; the correct mangling is "$s29conditional_pack_requirements3GG1Vyxq_q_Qp_QPGAA1QA2aERzAaER_rlMc"
// However, we retain the incorrect behavior for ABI compatibility.
//
// CHECK-LABEL: @"$s29conditional_pack_requirements3GG1Vyxq_q_Qp_QPGAA1QA2aERzAaER_AA1CCy1AAA1PPQy_q_Qp_QPGAhJRtzrlMc" =


public struct GG2<each A: P> {
  public struct Nested<each B: P> where repeat (each A).A == (each B).A {}
}

extension GG2.Nested: Q where repeat each A: Q, repeat each B: Q {}

// This mangling is correct.
// CHECK-LABEL: @"$s29conditional_pack_requirements3GG2V6NestedVyxxQp_QP_qd__qd__Qp_QPGAA1QA2aGRzAaGRd__rlMc" =


public struct GG3<A: P, each B: P> where A.A : C<repeat (each B).A> {}

extension GG3: Q where A: Q, repeat each B: Q {}

// This mangling is incorrect; the correct mangling is "$s29conditional_pack_requirements3GG3Vyxq_q_Qp_QPGAA1QA2aERzAaER_rlMc"
// However, we retain the incorrect behavior for ABI compatibility.
//
// CHECK-LABEL: @"$s29conditional_pack_requirements3GG3Vyxq_q_Qp_QPGAA1QA2aERzAaER_AA1CCy1AAA1PPQy_q_Qp_QPGAhJRczrlMc" =


public struct GG4<each A: P> {
  public struct Nested<each B: P> where repeat (each A).A : C<(each B).A> {}
}

extension GG4.Nested: Q where repeat each A: Q, repeat each B: Q {}

// This mangling is correct.
// CHECK-LABEL: @"$s29conditional_pack_requirements3GG4V6NestedVyxxQp_QP_qd__qd__Qp_QPGAA1QA2aGRzAaGRd__rlMc" =
