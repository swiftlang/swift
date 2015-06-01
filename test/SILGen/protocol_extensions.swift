// RUN: %target-swift-frontend -disable-objc-attr-requires-foundation-module -emit-silgen %s | FileCheck %s

public protocol P1 {
  func reqP1a()
}

extension P1 {
  // CHECK-LABEL: sil hidden @_TFeRq_19protocol_extensions2P1_S_S0_6extP1auRq_S0__fq_FT_T_ : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  final func extP1a() {
    // CHECK: [[WITNESS:%[0-9]+]] = witness_method $Self, #P1.reqP1a!1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[WITNESS]]<Self>([[SELF]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    reqP1a()
    // CHECK: return
  }

  // CHECK-LABEL: sil @_TFeRq_19protocol_extensions2P1_S_S0_6extP1buRq_S0__fq_FT_T_ : $@convention(method) <Self where Self : P1> (@in_guaranteed Self) -> () {
  // CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $*Self):
  public final func extP1b() {
    // CHECK: [[FN:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_6extP1auRq_S0__fq_FT_T_ : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
    // CHECK-NEXT: apply [[FN]]<Self>([[SELF]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
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

// CHECK-LABEL: sil hidden @_TF19protocol_extensions5testDFCS_1DT_ : $@convention(thin) (@owned D) -> () {
// CHECK-NEXT: bb0([[D:%[0-9]+]] : $D):
func testD(d: D) {
  // CHECK: [[D2:%[0-9]+]] = alloc_box $D
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_11returnsSelfuRq_S0__fq_FT_q_
  // CHECK: [[DCOPY:%[0-9]+]] = alloc_stack $D
  // CHECK: store [[D]] to [[DCOPY]]#1 : $*D
  // CHECK: [[RESULT:%[0-9]+]] = alloc_stack $D
  // CHECK: apply [[FN]]<D>([[RESULT]]#1, [[DCOPY]]#1) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in_guaranteed τ_0_0) -> ()
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
  // CHECK: [[F1:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_2f1uRq_S0__fq_FT_T_
  // CHECK: apply [[F1]]<@opened([[UUID]]) P1>([[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> ()
  p1.f1()

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[CURRIED1:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_8curried1uRq_S0__fq_fSbFVSs5Int64T_
  // CHECK: [[CURRIED1]]<@opened([[UUID]]) P1>([[I]], [[B]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Int64, Bool, @in_guaranteed τ_0_0) -> ()
  p1.curried1(b)(i)

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_g9subscriptFVSs5Int64Sb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[I]], [[POPENED_COPY]]#1) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Int64, @in_guaranteed τ_0_0) -> Bool
  // CHECK: destroy_addr [[POPENED_COPY]]#1
  // CHECK: store{{.*}} : $*Bool
  // CHECK: dealloc_stack [[POPENED_COPY]]
  var b2 = p1[i]

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_g4propSb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[POPENED_COPY]]#1) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
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
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_11returnsSelfuRq_S0__fq_FT_q_
  // CHECK: apply [[FN]]<@opened([[UUID]]) P1>([[P1AINIT]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@out τ_0_0, @in_guaranteed τ_0_0) -> ()
  var p1a: P1 = p1.returnsSelf()
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions23testExistentialsGetters
// CHECK: bb0([[P:%[0-9]+]] : $*P1):
func testExistentialsGetters(p1: P1) {
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[FN:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_g5prop2Sb
  // CHECK: [[B:%[0-9]+]] = apply [[FN]]<@opened([[UUID]]) P1>([[POPENED_COPY]]#1) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (@in_guaranteed τ_0_0) -> Bool
  let b: Bool = p1.prop2

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[P]] : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: copy_addr [[POPENED]] to [initialization] [[POPENED_COPY:%.*]]#1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_g9subscriptFSbSb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED_COPY]]#1) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @in_guaranteed τ_0_0) -> Bool
  let b2: Bool = p1[b]
}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions22testExistentialSetters
// CHECK: bb0([[P:%[0-9]+]] : $*P1, [[B:%[0-9]+]] : $Bool):
func testExistentialSetters(var p1: P1, b: Bool) {
  // CHECK: [[PBOX:%[0-9]+]] = alloc_box $P1
  // CHECK-NEXT: copy_addr [take] [[P]] to [initialization] [[PBOX]]#1 : $*P1
  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[PBOX]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_s5prop2Sb
  // CHECK: apply [[GETTER]]<@opened([[UUID]]) P1>([[B]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK-NOT: deinit_existential_addr
  p1.prop2 = b

  // CHECK: [[POPENED:%[0-9]+]] = open_existential_addr [[PBOX]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[SUBSETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_s9subscriptFSbSb
  // CHECK: apply [[SUBSETTER]]<@opened([[UUID]]) P1>([[B]], [[B]], [[POPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, Bool, @inout τ_0_0) -> ()
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
func testLogicalExistentialSetters(var hasAP1: HasAP1, _ b: Bool) {
  // CHECK: [[HASP1_BOX:%[0-9]+]] = alloc_box $HasAP1
  // CHECK-NEXT: copy_addr [take] [[HASP1]] to [initialization] [[HASP1_BOX]]#1 : $*HasAP1
  // CHECK: [[P1_COPY:%[0-9]+]] = alloc_stack $P1
  // CHECK-NEXT: [[HASP1_COPY:%[0-9]+]] = alloc_stack $HasAP1
  // CHECK-NEXT: copy_addr [[HASP1_BOX]]#1 to [initialization] [[HASP1_COPY]]#1 : $*HasAP1
  // CHECK: [[SOMEP1_GETTER:%[0-9]+]] = function_ref @_TFV19protocol_extensions6HasAP1g6someP1PS_2P1_ : $@convention(method) (@out P1, @in_guaranteed HasAP1) -> ()
  // CHECK: [[RESULT:%[0-9]+]] = apply [[SOMEP1_GETTER]]([[P1_COPY]]#1, %6#1) : $@convention(method) (@out P1, @in_guaranteed HasAP1) -> ()
  // CHECK: [[P1_OPENED:%[0-9]+]] = open_existential_addr [[P1_COPY]]#1 : $*P1 to $*@opened([[UUID:".*"]]) P1
  // CHECK: [[PROP2_SETTER:%[0-9]+]] = function_ref @_TFeRq_19protocol_extensions2P1_S_S0_s5prop2Sb : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: apply [[PROP2_SETTER]]<@opened([[UUID]]) P1>([[B]], [[P1_OPENED]]) : $@convention(method) <τ_0_0 where τ_0_0 : P1> (Bool, @inout τ_0_0) -> ()
  // CHECK: [[SOMEP1_SETTER:%[0-9]+]] = function_ref @_TFV19protocol_extensions6HasAP1s6someP1PS_2P1_ : $@convention(method) (@in P1, @inout HasAP1) -> ()
  // CHECK: apply [[SOMEP1_SETTER]]([[P1_COPY]]#1, [[HASP1_BOX]]#1) : $@convention(method) (@in P1, @inout HasAP1) -> ()
  // CHECK-NOT: deinit_existential_addr
  hasAP1.someP1.prop2 = b
  // CHECK: return
}

func plusOneP1() -> P1 {}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions38test_open_existential_semantics_opaque
func test_open_existential_semantics_opaque(guaranteed: P1,
                                            var immediate: P1) {
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $P1
  // CHECK: [[VALUE:%.*]] = open_existential_addr %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])

  guaranteed.f1()
  
  // -- Need a guaranteed copy because it's immutable
  // CHECK: copy_addr [[IMMEDIATE_BOX]]#1 to [initialization] [[IMMEDIATE:%.*]]#1
  // CHECK: [[VALUE:%.*]] = open_existential_addr [[IMMEDIATE]]#1
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[IMMEDIATE]]
  // CHECK: dealloc_stack [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[PLUS_ONE:%.*]] = alloc_stack $P1
  // CHECK: [[VALUE:%.*]] = open_existential_addr [[PLUS_ONE]]#1
  // CHECK: [[METHOD:%.*]] = function_ref
  // -- Can consume the value from our own copy
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: deinit_existential_addr [[PLUS_ONE]]
  // CHECK: dealloc_stack [[PLUS_ONE]]
  plusOneP1().f1()
}

protocol CP1: class {}

extension CP1 {
  final func f1() { }
}

func plusOneCP1() -> CP1 {}

// CHECK-LABEL: sil hidden @_TF19protocol_extensions37test_open_existential_semantics_class
func test_open_existential_semantics_class(guaranteed: CP1,
                                           var immediate: CP1) {
  // CHECK: [[IMMEDIATE_BOX:%.*]] = alloc_box $CP1

  // CHECK-NOT: strong_retain %0
  // CHECK: [[VALUE:%.*]] = open_existential_ref %0
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK-NOT: strong_release [[VALUE]]
  // CHECK-NOT: strong_release %0
  guaranteed.f1()

  // CHECK: [[IMMEDIATE:%.*]] = load [[IMMEDIATE_BOX]]
  // CHECK: strong_retain [[IMMEDIATE]]
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[IMMEDIATE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[VALUE]]
  // CHECK-NOT: strong_release [[IMMEDIATE]]
  immediate.f1()

  // CHECK: [[F:%.*]] = function_ref {{.*}}plusOneCP1
  // CHECK: [[PLUS_ONE:%.*]] = apply [[F]]()
  // CHECK: [[VALUE:%.*]] = open_existential_ref [[PLUS_ONE]]
  // CHECK: [[METHOD:%.*]] = function_ref
  // CHECK: apply [[METHOD]]<{{.*}}>([[VALUE]])
  // CHECK: strong_release [[VALUE]]
  // CHECK-NOT: strong_release [[PLUS_ONE]]
  plusOneCP1().f1()
}

protocol InitRequirement {
  init(c: C)
}

extension InitRequirement {
  // CHECK-LABEL: sil hidden @_TFeRq_19protocol_extensions15InitRequirement_S_S0_CuRq_S0__fMq_FT1dCS_1D_q_ : $@convention(thin) <Self where Self : InitRequirement> (@out Self, @owned D, @thick Self.Type) -> ()
  // CHECK:       bb0([[OUT:%.*]] : $*Self, [[ARG:%.*]] : $D, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  init(d: D) {
  // CHECK:         [[DELEGATEE:%.*]] = witness_method $Self, #InitRequirement.init!allocator.1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : InitRequirement> (@out τ_0_0, @owned C, @thick τ_0_0.Type) -> ()
  // CHECK:         [[ARG_UP:%.*]] = upcast [[ARG]]
  // CHECK:         apply [[DELEGATEE]]<Self>({{%.*}}, [[ARG_UP]], [[SELF_TYPE]])
    self.init(c: d)
  }

  // CHECK-LABEL: sil hidden @_TFeRq_19protocol_extensions15InitRequirement_S_S0_CuRq_S0__fMq_FT2d2CS_1D_q_
  // CHECK:         function_ref @_TFeRq_19protocol_extensions15InitRequirement_S_S0_CuRq_S0__fMq_FT1dCS_1D_q_
  init(d2: D) {
    self.init(d: d2)
  }
}

protocol ClassInitRequirement: class {
  init(c: C)
}

extension ClassInitRequirement {
  // CHECK-LABEL: sil hidden @_TFeRq_19protocol_extensions20ClassInitRequirement_S_S0_CuRq_S0__fMq_FT1dCS_1D_q_ : $@convention(thin) <Self where Self : ClassInitRequirement> (@owned D, @thick Self.Type) -> @owned Self
  // CHECK:       bb0([[ARG:%.*]] : $D, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  // CHECK:         [[DELEGATEE:%.*]] = witness_method $Self, #ClassInitRequirement.init!allocator.1 : $@convention(witness_method) <τ_0_0 where τ_0_0 : ClassInitRequirement> (@owned C, @thick τ_0_0.Type) -> @owned τ_0_0
  // CHECK:         [[ARG_UP:%.*]] = upcast [[ARG]]
  // CHECK:         apply [[DELEGATEE]]<Self>([[ARG_UP]], [[SELF_TYPE]])
  init(d: D) {
    self.init(c: d)
  }
}

@objc class OC {}
@objc class OD: OC {}

@objc protocol ObjCInitRequirement {
  init(c: OC, d: OC)
}

func foo(t: ObjCInitRequirement.Type, c: OC) -> ObjCInitRequirement {
  return t(c: OC(), d: OC())
}

extension ObjCInitRequirement {
  // CHECK-LABEL: sil hidden @_TFeRq_19protocol_extensions19ObjCInitRequirement_S_S0_CuRq_S0__fMq_FT1dCS_2OD_q_ : $@convention(thin) <Self where Self : ObjCInitRequirement> (@owned OD, @thick Self.Type) -> @owned Self
  // CHECK:       bb0([[ARG:%.*]] : $OD, [[SELF_TYPE:%.*]] : $@thick Self.Type):
  // CHECK:         [[OBJC_SELF_TYPE:%.*]] = thick_to_objc_metatype [[SELF_TYPE]]
  // CHECK:         [[SELF:%.*]] = alloc_ref_dynamic [objc] [[OBJC_SELF_TYPE]] : $@objc_metatype Self.Type, $Self
  // CHECK:         [[WITNESS:%.*]] = witness_method [volatile] $Self, #ObjCInitRequirement.init!initializer.1.foreign : $@convention(objc_method) <τ_0_0 where τ_0_0 : ObjCInitRequirement> (OC, OC, @owned τ_0_0) -> @owned τ_0_0
  // CHECK:         [[UPCAST1:%.*]] = upcast [[ARG]]
  // CHECK:         [[UPCAST2:%.*]] = upcast [[ARG]]
  // CHECK:         apply [[WITNESS]]<Self>([[UPCAST1]], [[UPCAST2]], [[SELF]])
  init(d: OD) {
    self.init(c: d, d: d)
  }
}
