// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -enable-guaranteed-self %s | FileCheck %s --check-prefix=GUARANTEED

public protocol P1 {
  func reqP1a()
}

extension P1 {
  // CHECK-LABEL: sil hidden @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(method) @thin <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  final func extP1a() {
    // CHECK: [[WITNESS:%[0-9]+]] = witness_method $Self, #P1.reqP1a!1 : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[WITNESS]]<Self>([[SELF]]) : $@cc(witness_method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    reqP1a()
    // CHECK: return
  }

  // CHECK-LABEL: sil @_TFP19protocol_extensions2P16extP1bUS0___fQPS0_FT_T_ : $@cc(method) @thin <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  public final func extP1b() {
    // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P16extP1aUS0___fQPS0_FT_T_ : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[FN]]<Self>([[SELF]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
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
  // CHECK: apply [[FN]]<D>([[RESULT]]#1, [[DCOPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in_guaranteed τ_0_0) -> ()
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
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]])
  // CHECK: [[F1:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P12f1US0___fQPS0_FT_T_
  // CHECK: apply [[F1]]<@opened([[UUID]]) P1>([[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
  p1.f1()

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[CURRIED1:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P18curried1US0___fQPS0_fSbFVSs5Int64T_
  // CHECK: [[CURRIED1]]<@opened([[UUID]]) P1>([[I]], [[B]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Int64, Bool, @in_guaranteed τ_0_0) -> ()
  p1.curried1(b)(i)

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g9subscriptFVSs5Int64Sb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[I]], [[POPENED_COPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Int64, @in_guaranteed τ_0_0) -> Bool
  // CHECK: destroy_addr [[POPENED_COPY]]#1
  // CHECK: store{{.*}} : $*Bool
  // CHECK: dealloc_stack [[POPENED_COPY]]
  var b2 = p1[i]

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g4propSb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[POPENED_COPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
  // CHECK: store{{.*}} : $*Bool
  // CHECK: dealloc_stack [[POPENED_COPY]]
  var b3 = p1.prop
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions17testExistentials2
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentials2(p1: P1) {
  // CHECK: [[P1A:%[0-9]+]] = alloc_box $P1
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[P1AINIT:%[0-9]+]] = init_existential_addr [[P1A]]#1 : $*P1, $@opened([[UUID2:".*"]]) P1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P111returnsSelfUS0___fQPS0_FT_S1_
  // CHECK: apply [[FN]]<@opened([[UUID]]) P1>([[P1AINIT]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in_guaranteed τ_0_0) -> ()
  var p1a: P1 = p1.returnsSelf()
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions23testExistentialsGetters
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentialsGetters(p1: P1) {
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g5prop2Sb
  // CHECK: [[B:%[0-9]+]] = apply [[FN]]<@opened([[UUID]]) P1>([[POPENED_COPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
  let b: Bool = p1.prop2

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1g9subscriptFSbSb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED_COPY]]#1) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, @in_guaranteed τ_0_0) -> Bool
  let b2: Bool = p1[b]
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions22testExistentialSetters
// CHECK: bb0([[P:%[0-9]+]] : $*P1, [[B:%[0-9]+]] : $Bool):
func testExistentialSetters(var p1: P1, b: Bool) {
  // CHECK: [[PBOX:%[0-9]+]] = alloc_box $P1
  // CHECK-NEXT: copy_addr [take] [[P]] to [initialization] [[PBOX]]#1 : $*P1
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[PBOX]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1s5prop2Sb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK-NOT: deinit_existential_addr
  p1.prop2 = b

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[PBOX]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[SUBSETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1s9subscriptFSbSb
  // CHECK: apply [[SUBSETTER]]<@opened([[UUID]]) P1>([[B]], [[B]], [[POPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, Bool, @inout τ_0_0) -> ()
  // CHECK-NOT: deinit_existential_addr [[PBOX]]#1 : $*P1
  p1[b] = b

  // CHECK: return
}

struct HasAP1 {
  var p1: P1

  var someP1: P1 {
    get { return p1 }
    set { p1 = newValue }
  }
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions29testLogicalExistentialSetters
// CHECK: bb0([[HASP1:%[0-9]+]] : $*HasAP1, [[B:%[0-9]+]] : $Bool)
func testLogicalExistentialSetters(var hasAP1: HasAP1, b: Bool) {
  // CHECK: [[HASP1_BOX:%[0-9]+]] = alloc_box $HasAP1
  // CHECK-NEXT: copy_addr [take] [[HASP1]] to [initialization] [[HASP1_BOX]]#1 : $*HasAP1
  // CHECK: [[P1_COPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: [[HASP1_COPY:%[0-9]+]] = alloc_stack $HasAP1
  // CHECK-NEXT: copy_addr [[HASP1_BOX]]#1 to [initialization] [[HASP1_COPY]]#1 : $*HasAP1
  // CHECK: [[SOMEP1_GETTER:%[0-9]+]] = function_ref @_TFV19protocol_extensions6HasAP1g6someP1PS_2P1_ : $@cc(method) @thin (@out P1, @in_guaranteed HasAP1) -> ()
  // CHECK: [[RESULT:%[0-9]+]] = apply [[SOMEP1_GETTER]]([[P1_COPY]]#1, %6#1) : $@cc(method) @thin (@out P1, @in_guaranteed HasAP1) -> ()
  // CHECK: [[P1_OPENED:%[0-9]+]] = open_existential_addr [[P1_COPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[PROP2_SETTER:%[0-9]+]] = function_ref @_TFP19protocol_extensions2P1s5prop2Sb : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: apply [[PROP2_SETTER]]<@opened([[UUID]]) P1>([[B]], [[P1_OPENED]]) : $@cc(method) @thin <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: [[SOMEP1_SETTER:%[0-9]+]] = function_ref @_TFV19protocol_extensions6HasAP1s6someP1PS_2P1_ : $@cc(method) @thin (@in P1, @inout HasAP1) -> ()
  // CHECK: apply [[SOMEP1_SETTER]]([[P1_COPY]]#1, [[HASP1_BOX]]#1) : $@cc(method) @thin (@in P1, @inout HasAP1) -> ()
  // CHECK-NOT: deinit_existential_addr
  hasAP1.someP1.prop2 = b
  // CHECK: return
}

func plusOneP1() -> P1 {}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions38test_open_existential_semantics_opaqueFTPS_2P1_PS0___T_
// GUARANTEED-LABEL: sil hidden @_TF19protocol_extensions38test_open_existential_semantics_opaqueFTPS_2P1_PS0___T_
func test_open_existential_semantics_opaque(guaranteed: P1,
                                            var immediate: P1) {
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $P1
  // GUARANTEED: [[IMMEDIATE_BOX:%.*]] = alloc_box $P1

  // CHECK: [[VALUE:%.*]] = open_existential_addr %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])

  // GUARANTEED: [[VALUE:%.*]] = open_existential_addr %0
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  guaranteed.f1()
  
  // -- Need a guaranteed copy because it's immutable
  // CHECK: copy_addr [[IMMEDIATE_BOX]]#1 to [initialization] [[IMMEDIATE:%.*]]#1
  // CHECK: [[VALUE:%.*]] = open_existential_addr [[IMMEDIATE]]#1
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[IMMEDIATE]]
  // CHECK: dealloc_stack [[IMMEDIATE]]

  // GUARANTEED: copy_addr [[IMMEDIATE_BOX]]#1 to [initialization] [[IMMEDIATE:%.*]]#1
  // GUARANTEED: [[VALUE:%.*]] = open_existential_addr [[IMMEDIATE]]#1
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // -- TODO: Avoid splitting the cleanup here.
  // GUARANTEED: destroy_addr [[VALUE]]
  // GUARANTEED: deinit_existential_addr [[IMMEDIATE]]
  // GUARANTEED: dealloc_stack [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[PLUS_ONE:%.*]] = alloc_stack $P1
  // CHECK: [[VALUE:%.*]] = open_existential_addr [[PLUS_ONE]]#1
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[PLUS_ONE]]
  // CHECK: dealloc_stack [[PLUS_ONE]]

  // GUARANTEED: [[PLUS_ONE:%.*]] = alloc_stack $P1
  // GUARANTEED: [[VALUE:%.*]] = open_existential_addr [[PLUS_ONE]]#1
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // -- TODO: Avoid splitting the cleanup here.
  // GUARANTEED: destroy_addr [[VALUE]]
  // GUARANTEED: deinit_existential_addr [[PLUS_ONE]]
  // GUARANTEED: dealloc_stack [[PLUS_ONE]]
  plusOneP1().f1()
}

protocol CP1: class {}

extension CP1 {
  final func f1() { }
}

func plusOneCP1() -> CP1 {}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions37test_open_existential_semantics_classFTPS_3CP1_PS0___T_
// GUARANTEED-LABEL: sil hidden @_TF19protocol_extensions37test_open_existential_semantics_classFTPS_3CP1_PS0___T_
func test_open_existential_semantics_class(guaranteed: CP1,
                                           var immediate: CP1) {
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $CP1
  // GUARANTEED: [[IMMEDIATE_BOX:%.*]] = alloc_box $CP1

  // CHECK-NOT: strong_retain %0
  // CHECK: [[VALUE:%.*]] = open_existential_ref %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK-NOT: strong_release [[VALUE]]
  // CHECK-NOT: strong_release %0

  // GUARANTEED-NOT: strong_retain %0
  // GUARANTEED: [[VALUE:%.*]] = open_existential_ref %0
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED-NOT: strong_release [[VALUE]]
  // GUARANTEED-NOT: strong_release %0
  guaranteed.f1()

  // CHECK: [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]
  // CHECK: strong_retain [[IMMEDIATE]]
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[IMMEDIATE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[VALUE]]
  // CHECK-NOT: strong_release [[IMMEDIATE]]

  // GUARANTEED: [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]
  // GUARANTEED: strong_retain [[IMMEDIATE]]
  // GUARANTEED: [[VALUE:%.*]] = open_existential_ref [[IMMEDIATE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED: strong_release [[VALUE]]
  // GUARANTEED-NOT: strong_release [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneCP1
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[VALUE]]
  // CHECK-NOT: strong_release [[PLUS_ONE]]

  // GUARANTEED: [[F:%.*]] = function_ref {{.*}}plusOneCP1
  // GUARANTEED: [[PLUS_ONE:%.*]] = apply [[F]]()
  // GUARANTEED: [[VALUE:%.*]] = open_existential_ref [[PLUS_ONE]]
  // GUARANTEED: [[METHOD:%.*]] = function_ref
  // GUARANTEED: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // GUARANTEED: strong_release [[VALUE]]
  // GUARANTEED-NOT: strong_release [[PLUS_ONE]]
  plusOneCP1().f1()
}

