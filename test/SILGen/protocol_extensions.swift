// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

public protocol P1 {
  func reqP1a() 
}

extension P1 {
  // CHECK-LABEL: sil hidden @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(witness_method) @thin <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  func extP1a() {
    // CHECK: [[WITNESS:%[0-9]+]] = witness_method $Self, #P1.reqP1a!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[WITNESS]]<Self>([[SELF]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    reqP1a()
    // CHECK: return
  }

  // CHECK-LABEL: sil @_TFP19protocol_extensions2P16extP1bUS0___fQPS0_FT_T_ : $@cc(witness_method) @thin <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  public func extP1b() {
    // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[FN]]<Self>([[SELF]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    extP1a()
    // CHECK: return
  }
}
