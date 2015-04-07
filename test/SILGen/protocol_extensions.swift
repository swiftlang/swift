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
// Using protocol extension members with concrete types
// ----------------------------------------------------------------------------
class C : P1 {
  func reqP1a() { }
}

class D : C { }

// CHECK-LABEL: sil hidden @_TF19protocol_extensions5testDFCS_1DT_ : $@thin (@owned D) -> () {
// CHECK-NEXT: bb0([[D:%[0-9]+]] : $D):
func testD(d: D) {
  // CHECK: [[D2:%[0-9]+]] = alloc_box $D
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P111returnsSelfUS0___fQPS0_FT_S1_
  // CHECK: [[DCOPY:%[0-9]+]] = alloc_stack $D
  // CHECK: store [[D]] to [[DCOPY]]#1 : $*D
  // CHECK: [[RESULT:%[0-9]+]] = alloc_stack $D
  // CHECK: apply [[FN]]<D>([[RESULT]]#1, [[DCOPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in τ_0_0) -> ()
  var d2: D = d.returnsSelf()
}

// ----------------------------------------------------------------------------
// Using protocol extension members with existentials
// ----------------------------------------------------------------------------
extension P1 {
  final func f1() { }

  final func curried1(b: Bool)(_ i: Int64) { }

  final subscript (i: Int64) -> Bool {
    get { return true }
  }

  final var prop: Bool {
    get { return true }
  }

  final func returnsSelf() -> Self { return self }

  final var prop2: Bool {
    get { return true }
    set { }
  }

  final subscript (b: Bool) -> Bool {
    get { return b }
    set { }
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

// CHECK-LABEL: sil hidden @_TF19protocol_extensions17testExistentials2
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentials2(p1: P1) {
  // CHECK: [[P1A:%[0-9]+]] = alloc_box $P1
  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK-NEXT: [[P1AINIT:%[0-9]+]] = init_existential_addr [[P1A]]#1 : $*P1, $@opened([[UUID2:".*"]]) P1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P111returnsSelfUS0___fQPS0_FT_S1_
  // CHECK-NEXT: apply [[FN]]<@opened([[UUID]]) P1>([[P1AINIT]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in τ_0_0) -> ()
  var p1a: P1 = p1.returnsSelf()
  // CHECK-NEXT: deinit_existential_addr [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: dealloc_stack [[PCOPY]]#0 : $*@local_storage P1
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions23testExistentialsGetters
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentialsGetters(p1: P1) {
  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g5prop2Sb
  // CHECK-NEXT: [[B:%[0-9]+]] = apply [[FN]]<@opened([[UUID]]) P1>([[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in τ_0_0) -> Bool
  let b: Bool = p1.prop2

  // CHECK: [[PCOPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: copy_addr [[P]] to [initialization] [[PCOPY]]#1 : $*P1
  // CHECK-NEXT: [[POPENED:%[0-9]+]] = open_existential_addr [[PCOPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g9subscriptFSbSb
  // CHECK-NEXT: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, @in τ_0_0) -> Bool
  let b2: Bool = p1[b]
}
