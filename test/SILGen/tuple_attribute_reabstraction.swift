// RUN: %target-swift-emit-silgen %s | %FileCheck %s

public struct G<T> {
  var t: T

  public init(t: T) { self.t = t }
}

public func takesAutoclosureAndEscaping(_: @autoclosure () -> (), _: @escaping () -> ()) {}
public func takesVarargs(_: Int...) {}

public func f() {
  _ = G(t: takesAutoclosureAndEscaping)
  _ = G(t: takesVarargs)
}

// We shouldn't have @autoclosure and @escaping attributes in the lowered tuple type:

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sIg_Ieg_Ieggg_xRi_zRi0_zlyytIsgr_xRi_zRi0_zlyytIsegr_ytIegnnr_TR : $@convention(thin) (@in_guaranteed @noescape @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>, @in_guaranteed @callee_guaranteed @substituted <τ_0_0> () -> @out τ_0_0 for <()>, @guaranteed @callee_guaranteed (@guaranteed @noescape @callee_guaranteed () -> (), @guaranteed @callee_guaranteed () -> ()) -> ()) -> @out ()

// The one-element vararg tuple ([Int]...) should be exploded and not treated as opaque,
// even though its materializable:

// CHECK-LABEL: sil shared [transparent] [serialized] [reabstraction_thunk] [ossa] @$sSaySiGIegg_AAytIegnr_TR : $@convention(thin) (@in_guaranteed Array<Int>, @guaranteed @callee_guaranteed (@guaranteed Array<Int>) -> ()) -> @out ()
