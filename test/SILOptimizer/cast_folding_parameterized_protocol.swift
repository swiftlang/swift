// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -emit-sil -disable-availability-checking -O -o - | %FileCheck %s

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
  // CHECK:{{%.*}} = init_existential_addr %6 : $*any P, $X
  use(x as! any P)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to any P<T> in {{%.*}} : $*any P<T>
  use(x as! any P<T>)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to any P<Int> in {{%.*}} : $*any P<Int>
  use(x as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr X in {{%.*}} : $*X to any P<String> in {{%.*}} : $*any P<String>
  use(x as! any P<String>)
  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*any Q, $X
  use(x as! any Q)

  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*any P, $Y<T>
  use(yt as! any P)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to any P<T> in {{%.*}} : $*any P<T>
  use(yt as! any P<T>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to any P<Int> in {{%.*}} : $*any P<Int>
  use(yt as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to any P<String> in {{%.*}} : $*any P<String>
  use(yt as! any P<String>)
  // CHECK: unconditional_checked_cast_addr Y<T> in {{%.*}} : $*Y<T> to any Q in {{%.*}} : $*any Q
  use(yt as! any Q)

  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*any P, $Y<Int>
  use(yi as! any P)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to any P<T> in {{%.*}} : $*any P<T>
  use(yi as! any P<T>)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to any P<Int> in {{%.*}} : $*any P<Int>
  use(yi as! any P<Int>)
  // CHECK: unconditional_checked_cast_addr Y<Int> in {{%.*}} : $*Y<Int> to any P<String> in {{%.*}} : $*any P<String>
  use(yi as! any P<String>)
  // CHECK: {{%.*}} = init_existential_addr {{%.*}} : $*any Q, $Y<Int>
  use(yi as! any Q)
}

// CHECK-LABEL: sil @$s35cast_folding_parameterized_protocol23existential_to_concreteyyxm_AA1P_px1TRts_XPtlF : $@convention(thin) <T> (@thick T.Type, @in_guaranteed any P<T>) -> ()
public func existential_to_concrete<T>(_: T.Type, _ p: any P<T>) {
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to X in {{%.*}} : $*X
  _ = p as! X
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to Y<T> in {{%.*}} : $*Y<T>
  _ = p as! Y<T>
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to Y<Int> in {{%.*}} : $*Y<Int>
  _ = p as! Y<Int>
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to Y<String> in {{%.*}} : $*Y<String>
  _ = p as! Y<String>
}

// CHECK-LABEL: sil @$s35cast_folding_parameterized_protocol015existential_to_E0yyAA1P_px1TRts_XP_AA1Q_ptlF : $@convention(thin) <T> (@in_guaranteed any P<T>, @in_guaranteed any Q) -> ()
public func existential_to_existential<T>(_ p: any P<T>, _ q: any Q) {
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to any Q in {{%.*}} : $*any Q
  _ = p as! any Q
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to any P in {{%.*}} : $*any P
  _ = p as! any P
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to any P<Int> in {{%.*}} : $*any P<Int>
  _ = p as! any P<Int>
  // CHECK: unconditional_checked_cast_addr any P<T> in {{%.*}} : $*any P<T> to any P<String> in {{%.*}} : $*any P<String>
  _ = p as! any P<String>

  // CHECK: unconditional_checked_cast_addr any Q in {{%.*}} : $*any Q to any P in {{%.*}} : $*any P
  _ = q as! any P
  // CHECK: unconditional_checked_cast_addr any Q in {{%.*}} : $*any Q to any P<T> in {{%.*}} : $*any P<T>
  _ = q as! any P<T>
  // CHECK: unconditional_checked_cast_addr any Q in {{%.*}} : $*any Q to any P<Int> in {{%.*}} : $*any P<Int>
  _ = q as! any P<Int>
  // CHECK: unconditional_checked_cast_addr any Q in {{%.*}} : $*any Q to any P<String> in {{%.*}} : $*any P<String>
  _ = q as! any P<String>
}
