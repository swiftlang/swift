// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

public protocol P1 {
  func reqP1a() 
}

extension P1 {
  // CHECK-LABEL: sil hidden @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(method) @thin <Self where Self : P1> (@in Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  final func extP1a() {
    // CHECK: [[WITNESS:%[0-9]+]] = witness_method $Self, #P1.reqP1a!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[WITNESS]]<Self>([[SELF]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    reqP1a()
    // CHECK: return
  }

  // CHECK-LABEL: sil @_TFP19protocol_extensions2P16extP1bUS0___fQPS0_FT_T_ : $@cc(method) @thin <Self where Self : P1> (@in Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  public final func extP1b() {
    // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in τ_0_0) -> ()
    // CHECK-NEXT: [[SELF_COPY:%[0-9]+]] = alloc_stack $Self
    // CHECK-NEXT: copy_addr [[SELF]] to [initialization] [[SELF_COPY]]#1 : $*Self
    // CHECK-NEXT: apply [[FN]]<Self>([[SELF_COPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in τ_0_0) -> ()
    extP1a()
    // CHECK: return
  }
}

// ----------------------------------------------------------------------------
// Using protocol extension members with existentials
// ----------------------------------------------------------------------------
extension P1 {
  final func f1() { }

  final func curried1(b: Bool)(_ i: Int64) { }

  final subscript (i: Int64) -> Bool {
    get { return true }

    // FIXME: setter
  }

  final var prop: Bool {
    get { return true }

    // FIXME: getter
  }
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions17testExistentials1
// CHECK: bb0([[P:%[0-9]+]] : $*P1, [[B:%[0-9]+]] : $Bool, [[I:%[0-9]+]] : $Int64):
func testExistentials1(p1: P1, b: Bool, i: Int64) {
  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]])
  // CHECK: [[F1:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P12f1US0___fQPS0_FT_T_
  // CHECK-NEXT: apply [[F1]]<@opened([[UUID]]) P1>([[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in τ_0_0) -> ()
  // CHECK-NEXT: deinit_existential_addr [[PCOPY]]#1 : $*P1
  p1.f1()

  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[CURRIED1:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P18curried1US0___fQPS0_fSbFVSs5Int64T_
  // CHECK-NEXT: [[CURRIED1]]<@opened([[UUID]]) P1>([[I]], [[B]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Int64, Bool, @in τ_0_0) -> ()
  // CHECK-NEXT: deinit_existential_addr [[PCOPY]]#1 : $*P1
  p1.curried1(b)(i)

  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g9subscriptFVSs5Int64Sb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[I]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Int64, @in τ_0_0) -> Bool
  // CHECK: store{{.*}} : $*Bool
  // CHECK: deinit_existential_addr [[PCOPY]]#1 : $*P1
  var b2 = p1[i]

  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g4propSb
  // CHECK-NEXT: apply [[GETTER]]<@opened([[UUID]]) P1>([[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in τ_0_0) -> Bool
  // CHECK-NEXT: store{{.*}} : $*Bool                     // id: %35
  // CHECK-NEXT: deinit_existential_addr [[PCOPY]]#1 : $*P1
  var b3 = p1.prop
}
