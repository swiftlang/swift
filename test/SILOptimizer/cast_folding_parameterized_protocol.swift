// RUN: %target-swift-frontend %s -emit-sil -enable-parameterized-existential-types -O -o - | %FileCheck %s

public protocol P<T> {
  associatedtype T
}
public protocol Q: P where T == Int {}
public struct X: Q {
  public typealias T = Int
}
public struct Y<T>: P {}
extension Y: Q where T == Int {}

@_optimize(none)
func use<T>(_ t: T) {}

// CHECK-LABEL: sil @$s35cast_folding_parameterized_protocol23concrete_to_existentialyyAA1XV_AA1YVyxGAFySiGtlF : $@convention(thin) <T> (X, Y<T>, Y<Int>) -> ()
public func concrete_to_existential<T>(_ x: X, _ yt: Y<T>, _ yi: Y<Int>) {
  // CHECK:{{%.*}} = init_existential_addr %6 : $*P, $X
  use(x as! any P)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to P<T> in {{%.*}} : $*P<T>
  use(x as! any P<T>)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to P<Int> in {{%.*}} : $*P<Int>
  use(x as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to P<String> in {{%.*}} : $*P<String>
  use(x as! any P<String>)
  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*Q, $X
  use(x as! any Q)

  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*P, $Y<T>
  use(yt as! any P)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to P<T> in {{%.*}} : $*P<T>
  use(yt as! any P<T>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to P<Int> in {{%.*}} : $*P<Int>
  use(yt as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to P<String> in {{%.*}} : $*P<String>
  use(yt as! any P<String>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to Q in {{%.*}} : $*Q
  use(yt as! any Q)

  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*P, $Y<Int>
  use(yi as! any P)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to P<T> in {{%.*}} : $*P<T>
  use(yi as! any P<T>)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to P<Int> in {{%.*}} : $*P<Int>
  use(yi as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to P<String> in {{%.*}} : $*P<String>
  use(yi as! any P<String>)
  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*Q, $Y<Int>
  use(yi as! any Q)
}

// CHECK-LABEL: sil @$s35cast_folding_parameterized_protocol23existential_to_concreteyyxm_AA1P_pyxXPtlF : $@convention(thin) <T> (@thick T.Type, @in_guaranteed P<T>) -> ()
public func existential_to_concrete<T>(_: T.Type, _ p: any P<T>) {
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to X in {{%.*}} : $*X
  _ = p as! X
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to Y<T> in {{%.*}} : $*Y<T>
  _ = p as! Y<T>
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to Y<Int> in {{%.*}} : $*Y<Int>
  _ = p as! Y<Int>
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to Y<String> in {{%.*}} : $*Y<String>
  _ = p as! Y<String>
}

// CHECK-LABEL: sil @$s35cast_folding_parameterized_protocol015existential_to_E0yyAA1P_pyxXP_AA1Q_ptlF : $@convention(thin) <T> (@in_guaranteed P<T>, @in_guaranteed Q) -> ()
public func existential_to_existential<T>(_ p: any P<T>, _ q: any Q) {
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to Q in {{%.*}} : $*Q
  _ = p as! any Q
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to P in {{%.*}} : $*P
  _ = p as! any P
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to P<Int> in {{%.*}} : $*P<Int>
  _ = p as! any P<Int>
  // CHECK: unconditional_checked_cast_addr P<T> in {{%.*}} : $*P<T> to P<String> in {{%.*}} : $*P<String>
  _ = p as! any P<String>

  // CHECK: unconditional_checked_cast_addr Q in {{%.*}} : $*Q to P in {{%.*}} : $*P
  _ = q as! any P
  // CHECK: unconditional_checked_cast_addr Q in {{%.*}} : $*Q to P<T> in {{%.*}} : $*P<T>
  _ = q as! any P<T>
  // CHECK: unconditional_checked_cast_addr Q in {{%.*}} : $*Q to P<Int> in {{%.*}} : $*P<Int>
  _ = q as! any P<Int>
  // CHECK: unconditional_checked_cast_addr Q in {{%.*}} : $*Q to P<String> in {{%.*}} : $*P<String>
  _ = q as! any P<String>
}
