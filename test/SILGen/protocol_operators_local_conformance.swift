// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s
//
// Test that we find and pick the right witnesses for operator function
// requirements when the conforming type is declared in a local context.

infix operator =*=
protocol P1 {
  static func =*= (lhs: Self, rhs: Self) -> Bool
}
func test1() {
  struct S: P1 {
    static func =*= (lhs: S, rhs: S) -> Bool { true }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test1yyF1SL_VAA2P1A2aEP3emeoiySbx_xtFZTW : $@convention(witness_method: P1) (@in_guaranteed S, @in_guaranteed S, @thick S.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test1yyF1SL_V3emeoiySbAD_ADtFZ : $@convention(method) (S, S, @thin S.Type) -> Bool

infix operator =**=
protocol P2 {
  static func =**= (lhs: Self, rhs: Self) -> Bool
}
extension P2 {
  static func =**= (lhs: Self, rhs: Self) -> Bool { true }
}
func test2() {
  struct S: P2 {}
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test2yyF1SL_VAA2P2A2aEP4emmeoiySbx_xtFZTW : $@convention(witness_method: P2) (@in_guaranteed S, @in_guaranteed S, @thick S.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance2P2PAAE4emmeoiySbx_xtFZ : $@convention(method) <τ_0_0 where τ_0_0 : P2> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0, @thick τ_0_0.Type) -> Bool

func test3() {
  struct S: P2 {
    static func =**= (lhs: S, rhs: S) -> Bool { true }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test3yyF1SL_VAA2P2A2aEP4emmeoiySbx_xtFZTW : $@convention(witness_method: P2) (@in_guaranteed S, @in_guaranteed S, @thick S.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test3yyF1SL_V4emmeoiySbAD_ADtFZ : $@convention(method) (S, S, @thin S.Type) -> Bool

func test4() {
  enum E: Int, Equatable {
    case a, b

    static func == (lhs: E, rhs: E) -> Bool { true }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test4yyF1EL_OSQAASQ2eeoiySbx_xtFZTW : $@convention(witness_method: Equatable) (@in_guaranteed E, @in_guaranteed E, @thick E.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test4yyF1EL_O2eeoiySbAD_ADtFZ : $@convention(method) (E, E, @thin E.Type) -> Bool

func test5() {
  enum E: Equatable {
    case a, b
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test5yyF1EL_OSQAASQ2eeoiySbx_xtFZTW : $@convention(witness_method: Equatable) (@in_guaranteed E, @in_guaranteed E, @thick E.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test5yyF1EL_O21__derived_enum_equalsySbAD_ADtFZ : $@convention(method) (E, E, @thin E.Type) -> Bool


func test6() {
  enum E: Int, Equatable {
    case a, b
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test6yyF1EL_OSQAASQ2eeoiySbx_xtFZTW : $@convention(witness_method: Equatable) (@in_guaranteed E, @in_guaranteed E, @thick E.Type) -> Bool {
// CHECK: function_ref @$[[TEST6_EQUALS_WITNESS:[_0-9a-zA-Z]+]]
// CHECK: }

// CHECK: sil [serialized] {{.*}}@$[[TEST6_EQUALS_WITNESS]] : $@convention(thin) <τ_0_0 where τ_0_0 : RawRepresentable, τ_0_0.RawValue : Equatable> (@in_guaranteed τ_0_0, @in_guaranteed τ_0_0) -> Bool

func test7() {
  struct Outer {
    enum Inner: Int, Comparable {
      case a, b

      static func < (lhs: Inner, rhs: Inner) -> Bool { true }
    }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test7yyF5OuterL_V5InnerOSLAASL1loiySbx_xtFZTW : $@convention(witness_method: Comparable) (@in_guaranteed Outer.Inner, @in_guaranteed Outer.Inner, @thick Outer.Inner.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test7yyF5OuterL_V5InnerO1loiySbAF_AFtFZ : $@convention(method) (Outer.Inner, Outer.Inner, @thin Outer.Inner.Type) -> Bool

func test8() {
  class C: Equatable {
    static func == (lhs: C, rhs: C) -> Bool { true }
  }
}
// CHECK-LABEL: sil private [transparent] [thunk] [ossa] @$s36protocol_operators_local_conformance5test8yyF1CL_CSQAASQ2eeoiySbx_xtFZTW : $@convention(witness_method: Equatable) (@in_guaranteed C, @in_guaranteed C, @thick C.Type) -> Bool {
// CHECK: function_ref @$s36protocol_operators_local_conformance5test8yyF1CL_C2eeoiySbAD_ADtFZ : $@convention(method) (@guaranteed C, @guaranteed C, @thick C.Type) -> Bool
