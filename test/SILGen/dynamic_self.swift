// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s -disable-objc-attr-requires-foundation-module | %FileCheck %s

protocol P {
  func f() -> Self
}

protocol CP : class {
  func f() -> Self
}

class X : P, CP {
  required init(int i: Int) { }

  // CHECK-LABEL: sil hidden @_T012dynamic_self1XC1f{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed X) -> @owned
  func f() -> Self { return self }

  // CHECK-LABEL: sil hidden @_T012dynamic_self1XC7factory{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (Int, @thick X.Type) -> @owned X
  // CHECK: bb0([[I:%[0-9]+]] : $Int, [[SELF:%[0-9]+]] : $@thick X.Type):
  // CHECK: [[CTOR:%[0-9]+]] = class_method [[SELF]] : $@thick X.Type, #X.init!allocator.1 : (X.Type) -> (Int) -> X, $@convention(method) (Int, @thick X.Type) -> @owned X
  // CHECK: apply [[CTOR]]([[I]], [[SELF]]) : $@convention(method) (Int, @thick X.Type) -> @owned X
  class func factory(i: Int) -> Self { return self.init(int: i) }
}

class Y : X { 
  required init(int i: Int) { }
}

class GX<T> {
  func f() -> Self { return self }
}

class GY<T> : GX<[T]> { }

// CHECK-LABEL: sil hidden @_T012dynamic_self23testDynamicSelfDispatch{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned Y) -> ()
// CHECK: bb0([[Y:%[0-9]+]] : $Y):
// CHECK:   [[BORROWED_Y:%.*]] = begin_borrow [[Y]]
// CHECK:   [[Y_COPY:%.*]] = copy_value [[BORROWED_Y]]
// CHECK:   [[Y_AS_X_COPY:%[0-9]+]] = upcast [[Y_COPY]] : $Y to $X  
// CHECK:   [[X_F:%[0-9]+]] = class_method [[Y_AS_X_COPY]] : $X, #X.f!1 : (X) -> () -> @dynamic_self X, $@convention(method) (@guaranteed X) -> @owned X
// => SEMANTIC SIL TODO: This argument here needs to be borrowed.
// CHECK:   [[X_RESULT:%[0-9]+]] = apply [[X_F]]([[Y_AS_X_COPY]]) : $@convention(method) (@guaranteed X) -> @owned X
// CHECK:   destroy_value [[Y_AS_X_COPY]]
// CHECK:   [[Y_RESULT:%[0-9]+]] = unchecked_ref_cast [[X_RESULT]] : $X to $Y
// CHECK:   destroy_value [[Y_RESULT]] : $Y
// CHECK:   end_borrow [[BORROWED_Y]] from [[Y]]
// CHECK:   destroy_value [[Y]] : $Y
func testDynamicSelfDispatch(y: Y) {
  y.f()
}

// CHECK-LABEL: sil hidden @_T012dynamic_self30testDynamicSelfDispatchGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned GY<Int>) -> ()
func testDynamicSelfDispatchGeneric(gy: GY<Int>) {
  // CHECK: bb0([[GY:%[0-9]+]] : $GY<Int>):
  // CHECK:   [[BORROWED_GY:%.*]] = begin_borrow [[GY]]
  // CHECK:   [[GY_COPY:%.*]] = copy_value [[BORROWED_GY]]
  // CHECK:   [[GY_AS_GX_COPY:%[0-9]+]] = upcast [[GY_COPY]] : $GY<Int> to $GX<Array<Int>>
  // CHECK:   [[GX_F:%[0-9]+]] = class_method [[GY_AS_GX_COPY]] : $GX<Array<Int>>, #GX.f!1 : <T> (GX<T>) -> () -> @dynamic_self GX<T>, $@convention(method) <τ_0_0> (@guaranteed GX<τ_0_0>) -> @owned GX<τ_0_0>
  // CHECK:   [[GX_RESULT:%[0-9]+]] = apply [[GX_F]]<[Int]>([[GY_AS_GX_COPY]]) : $@convention(method) <τ_0_0> (@guaranteed GX<τ_0_0>) -> @owned GX<τ_0_0>
  // CHECK:   destroy_value [[GY_AS_GX_COPY]]
  // CHECK:   [[GY_RESULT:%[0-9]+]] = unchecked_ref_cast [[GX_RESULT]] : $GX<Array<Int>> to $GY<Int>
  // CHECK:   destroy_value [[GY_RESULT]] : $GY<Int>
  // CHECK:   end_borrow [[BORROWED_GY]] from [[GY]]
  // CHECK:   destroy_value [[GY]]
  gy.f()
}

// CHECK-LABEL: sil hidden @_T012dynamic_self21testArchetypeDispatch{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P> (@in T) -> ()
func testArchetypeDispatch<T: P>(t: T) {
  // CHECK: bb0([[T:%[0-9]+]] : $*T):
  // CHECK:   [[ARCHETYPE_F:%[0-9]+]] = witness_method $T, #P.f!1 : {{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK:   [[T_RESULT:%[0-9]+]] = alloc_stack $T
  // CHECK:   [[SELF_RESULT:%[0-9]+]] = apply [[ARCHETYPE_F]]<T>([[T_RESULT]], [[T]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  t.f()
}

// CHECK-LABEL: sil hidden @_T012dynamic_self23testExistentialDispatch{{[_0-9a-zA-Z]*}}F
func testExistentialDispatch(p: P) {
// CHECK: bb0([[P:%[0-9]+]] : $*P):
// CHECK:   [[PCOPY_ADDR:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P to $*@opened([[N:".*"]]) P
// CHECK:   [[P_RESULT:%[0-9]+]] = alloc_stack $P
// CHECK:   [[P_RESULT_ADDR:%[0-9]+]] = init_existential_addr [[P_RESULT]] : $*P, $@opened([[N]]) P
// CHECK:   [[P_F_METHOD:%[0-9]+]] = witness_method $@opened([[N]]) P, #P.f!1 : {{.*}}, [[PCOPY_ADDR]]{{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   apply [[P_F_METHOD]]<@opened([[N]]) P>([[P_RESULT_ADDR]], [[PCOPY_ADDR]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   destroy_addr [[P_RESULT]] : $*P
// CHECK:   dealloc_stack [[P_RESULT]] : $*P
// CHECK:   destroy_addr [[P]] : $*P
  p.f()
}

// CHECK-LABEL: sil hidden @_T012dynamic_self28testExistentialDispatchClass{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned CP) -> ()
// CHECK: bb0([[CP:%[0-9]+]] : $CP):
// CHECK:   [[BORROWED_CP:%.*]] = begin_borrow [[CP]]
// CHECK:   [[CP_ADDR:%[0-9]+]] = open_existential_ref [[BORROWED_CP]] : $CP to $@opened([[N:".*"]]) CP
// CHECK:   [[CP_F:%[0-9]+]] = witness_method $@opened([[N]]) CP, #CP.f!1 : {{.*}}, [[CP_ADDR]]{{.*}} : $@convention(witness_method) <τ_0_0 where τ_0_0 : CP> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK:   [[CP_F_RESULT:%[0-9]+]] = apply [[CP_F]]<@opened([[N]]) CP>([[CP_ADDR]]) : $@convention(witness_method) <τ_0_0 where τ_0_0 : CP> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK:   [[RESULT_EXISTENTIAL:%[0-9]+]] = init_existential_ref [[CP_F_RESULT]] : $@opened([[N]]) CP : $@opened([[N]]) CP, $CP
// CHECK:   destroy_value [[CP_F_RESULT]] : $@opened([[N]]) CP
// CHECK:   end_borrow [[BORROWED_CP]] from [[CP]]
// CHECK:   destroy_value [[CP]]
func testExistentialDispatchClass(cp: CP) {
  cp.f()
}

@objc class ObjC {
  @objc func method() -> Self { return self }
}

// CHECK-LABEL: sil hidden @_T012dynamic_self21testAnyObjectDispatchys0dE0_p1o_tF : $@convention(thin) (@owned AnyObject) -> () {
func testAnyObjectDispatch(o: AnyObject) {
  // CHECK: dynamic_method_br [[O_OBJ:%[0-9]+]] : $@opened({{.*}}) AnyObject, #ObjC.method!1.foreign, bb1, bb2

  // CHECK: bb1([[METHOD:%[0-9]+]] : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @autoreleased AnyObject):
  // CHECK:   [[O_OBJ_COPY:%.*]] = copy_value [[O_OBJ]]
  // CHECK:   [[VAR_9:%[0-9]+]] = partial_apply [[METHOD]]([[O_OBJ_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @autoreleased AnyObject
  var x = o.method
}
// CHECK: } // end sil function '_T012dynamic_self21testAnyObjectDispatchys0dE0_p1o_tF'


// <rdar://problem/16270889> Dispatch through ObjC metatypes.
class ObjCInit {
  dynamic required init() { }
}

// CHECK: sil hidden @_T012dynamic_self12testObjCInit{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@thick ObjCInit.Type) -> ()
func testObjCInit(meta: ObjCInit.Type) {
// CHECK: bb0([[THICK_META:%[0-9]+]] : $@thick ObjCInit.Type):
// CHECK:   [[O:%[0-9]+]] = alloc_box ${ var ObjCInit }
// CHECK:   [[PB:%.*]] = project_box [[O]]
// CHECK:   [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[THICK_META]] : $@thick ObjCInit.Type to $@objc_metatype ObjCInit.Type
// CHECK:   [[OBJ:%[0-9]+]] = alloc_ref_dynamic [objc] [[OBJC_META]] : $@objc_metatype ObjCInit.Type, $ObjCInit
// CHECK:   [[INIT:%[0-9]+]] = class_method [volatile] [[OBJ]] : $ObjCInit, #ObjCInit.init!initializer.1.foreign : (ObjCInit.Type) -> () -> ObjCInit, $@convention(objc_method) (@owned ObjCInit) -> @owned ObjCInit
// CHECK:   [[RESULT_OBJ:%[0-9]+]] = apply [[INIT]]([[OBJ]]) : $@convention(objc_method) (@owned ObjCInit) -> @owned ObjCInit
// CHECK:   store [[RESULT_OBJ]] to [init] [[PB]] : $*ObjCInit
// CHECK:   destroy_value [[O]] : ${ var ObjCInit }
// CHECK:   [[RESULT:%[0-9]+]] = tuple ()
// CHECK:   return [[RESULT]] : $()
  var o = meta.init()
}

class OptionalResult {
  func foo() -> Self? { return self }
}

// CHECK-LABEL: sil hidden @_T012dynamic_self14OptionalResultC3fooACXDSgyF : $@convention(method) (@guaranteed OptionalResult) -> @owned Optional<OptionalResult> {
// CHECK: bb0([[SELF:%.*]] : $OptionalResult):
// CHECK-NEXT: debug_value [[SELF]] : $OptionalResult
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK-NEXT: [[T0:%.*]] = enum $Optional<OptionalResult>, #Optional.some!enumelt.1, [[SELF_COPY]] : $OptionalResult
// CHECK-NEXT: return [[T0]] : $Optional<OptionalResult>
// CHECK: } // end sil function '_T012dynamic_self14OptionalResultC3fooACXDSgyF'

class OptionalResultInheritor : OptionalResult {
  func bar() {}
}

func testOptionalResult(v : OptionalResultInheritor) {
  v.foo()?.bar()
}

// CHECK-LABEL: sil hidden @_T012dynamic_self18testOptionalResultyAA0dE9InheritorC1v_tF : $@convention(thin) (@owned OptionalResultInheritor) -> () {
// CHECK: bb0([[ARG:%.*]] : $OptionalResultInheritor):
// CHECK:      [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:      [[COPY_ARG:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:      [[CAST_COPY_ARG:%.*]] = upcast [[COPY_ARG]]
// CHECK:      [[T0:%.*]] = class_method [[CAST_COPY_ARG]] : $OptionalResult, #OptionalResult.foo!1 : (OptionalResult) -> () -> @dynamic_self OptionalResult?, $@convention(method) (@guaranteed OptionalResult) -> @owned Optional<OptionalResult>
// CHECK-NEXT: [[RES:%.*]] = apply [[T0]]([[CAST_COPY_ARG]])
// CHECK:      switch_enum [[RES]] : $Optional<OptionalResult>, case #Optional.some!enumelt.1: [[SOME_BB:bb[0-9]+]]
// CHECK: [[SOME_BB]]([[T1:%.*]] : $OptionalResult):
// CHECK-NEXT: [[T4:%.*]] = unchecked_ref_cast [[T1]] : $OptionalResult to $OptionalResultInheritor
// CHECK-NEXT: enum $Optional<OptionalResultInheritor>, #Optional.some!enumelt.1, [[T4]]

class Z {

  // CHECK-LABEL: sil hidden @_T012dynamic_self1ZC23testDynamicSelfCapturesACXDSi1x_tF : $@convention(method) (Int, @guaranteed Z) -> @owned Z {
  func testDynamicSelfCaptures(x: Int) -> Self {
    // CHECK: bb0({{.*}}, [[SELF:%.*]] : $Z):

    // Single capture of 'self' type

    // CHECK:      [[FN:%.*]] = function_ref @_T012dynamic_self1ZC23testDynamicSelfCapturesACXDSi1x_tFyycfU_ : $@convention(thin) (@owned Z) -> ()
    // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Z
    // CHECK-NEXT: partial_apply [[FN]]([[SELF_COPY]])
    let fn1 = { _ = self }
    fn1()

    // Capturing 'self', but it's not the last capture. Make sure it ends
    // up at the end of the list anyway

    // CHECK:      [[FN:%.*]] = function_ref @_T012dynamic_self1ZC23testDynamicSelfCapturesACXDSi1x_tFyycfU0_ : $@convention(thin) (Int, @owned Z) -> ()
    // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Z
    // CHECK-NEXT: partial_apply [[FN]]({{.*}}, [[SELF_COPY]])
    let fn2 = {
      _ = self
      _ = x
    }
    fn2()

    // Capturing 'self' weak, so we have to pass in a metatype explicitly
    // so that IRGen can recover metadata.

    // CHECK:      [[WEAK_SELF:%.*]] = alloc_box ${ var @sil_weak Optional<Z> }
    // CHECK:      [[FN:%.*]] = function_ref @_T012dynamic_self1ZC23testDynamicSelfCapturesACXDSi1x_tFyycfU1_ : $@convention(thin) (@owned { var @sil_weak Optional<Z> }, @thick Z.Type) -> ()
    // CHECK:      [[WEAK_SELF_COPY:%.*]] = copy_value [[WEAK_SELF]] : ${ var @sil_weak Optional<Z> }
    // CHECK-NEXT: [[DYNAMIC_SELF:%.*]] = metatype $@thick @dynamic_self Z.Type
    // CHECK-NEXT: [[STATIC_SELF:%.*]] = upcast [[DYNAMIC_SELF]] : $@thick @dynamic_self Z.Type to $@thick Z.Type
    // CHECK:      partial_apply [[FN]]([[WEAK_SELF_COPY]], [[STATIC_SELF]]) : $@convention(thin) (@owned { var @sil_weak Optional<Z> }, @thick Z.Type) -> ()
    let fn3 = {
      [weak self] in
      _ = self
    }
    fn3()

    return self
  }

}

// Unbound reference to a method returning Self.

class Factory {
  func newInstance() -> Self {}
  class func classNewInstance() -> Self {}
  static func staticNewInstance() -> Self {}
}

// CHECK-LABEL: sil hidden @_T012dynamic_self22partialApplySelfReturnyAA7FactoryC1c_ADm1ttF : $@convention(thin) (@owned Factory, @thick Factory.Type) -> ()
func partialApplySelfReturn(c: Factory, t: Factory.Type) {
  // CHECK: function_ref @_T012dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@owned Factory) -> @owned @callee_owned () -> @owned Factory
  _ = c.newInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@owned Factory) -> @owned @callee_owned () -> @owned Factory
  _ = Factory.newInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@owned Factory) -> @owned @callee_owned () -> @owned Factory
  _ = t.newInstance
  _ = type(of: c).newInstance

  // CHECK: function_ref @_T012dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = t.classNewInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = type(of: c).classNewInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = Factory.classNewInstance

  // CHECK: function_ref @_T012dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = t.staticNewInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = type(of: c).staticNewInstance
  // CHECK: function_ref @_T012dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_owned () -> @owned Factory
  _ = Factory.staticNewInstance
}

class FactoryFactory {

  // CHECK-LABEL: sil hidden @_T012dynamic_self07FactoryC0C11newInstanceACXDyFZ : $@convention(method) (@thick FactoryFactory.Type) -> @owned FactoryFactory
  static func newInstance() -> Self {
    // CHECK: bb0(%0 : $@thick FactoryFactory.Type):

    // CHECK: [[METATYPE:%.*]] = value_metatype $@thick @dynamic_self FactoryFactory.Type.Type, %0 : $@thick FactoryFactory.Type
    // CHECK: [[ANY:%.*]] = init_existential_metatype [[METATYPE]] : $@thick @dynamic_self FactoryFactory.Type.Type, $@thick Any.Type
    let _: Any.Type = type(of: self)

    // CHECK: unreachable
  }
}

// Super call to a method returning Self
class Base {
  required init() {}

  func returnsSelf() -> Self {
    return self
  }

  static func returnsSelfStatic() -> Self {
    return self.init()
  }
}

class Derived : Base {
  // CHECK-LABEL: sil hidden @_T012dynamic_self7DerivedC9superCallyyF : $@convention(method) (@guaranteed Derived) -> ()
  // CHECK: [[SELF:%.*]] = copy_value %0
  // CHECK: [[SUPER:%.*]] = upcast [[SELF]] : $Derived to $Base
  // CHECK: [[METHOD:%.*]] = function_ref @_T012dynamic_self4BaseC11returnsSelfACXDyF
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  func superCall() {
    super.returnsSelf()
  }

  // CHECK-LABEL: sil hidden @_T012dynamic_self7DerivedC15superCallStaticyyFZ : $@convention(method) (@thick Derived.Type) -> ()
  // CHECK: [[SUPER:%.*]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
  // CHECK: [[METHOD:%.*]] = function_ref @_T012dynamic_self4BaseC17returnsSelfStaticACXDyFZ
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  static func superCallStatic() {
    super.returnsSelfStatic()
  }

  // CHECK-LABEL: sil hidden @_T012dynamic_self7DerivedC32superCallFromMethodReturningSelfACXDyF : $@convention(method) (@guaranteed Derived) -> @owned Derived
  // CHECK: [[SELF:%.*]] = copy_value %0
  // CHECK: [[SUPER:%.*]] = upcast [[SELF]] : $Derived to $Base
  // CHECK: [[METHOD:%.*]] = function_ref @_T012dynamic_self4BaseC11returnsSelfACXDyF
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  func superCallFromMethodReturningSelf() -> Self {
    super.returnsSelf()
    return self
  }

  // CHECK-LABEL: sil hidden @_T012dynamic_self7DerivedC38superCallFromMethodReturningSelfStaticACXDyFZ : $@convention(method) (@thick Derived.Type) -> @owned Derived
  // CHECK: [[SUPER:%.*]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
  // CHECK: [[METHOD:%.*]] = function_ref @_T012dynamic_self4BaseC17returnsSelfStaticACXDyFZ
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  static func superCallFromMethodReturningSelfStatic() -> Self {
    super.returnsSelfStatic()
    return self.init()
  }
}

// CHECK-LABEL: sil_witness_table hidden X: P module dynamic_self {
// CHECK: method #P.f!1: {{.*}} : @_T012dynamic_self1XCAA1PA2aDP1f{{[_0-9a-zA-Z]*}}FTW

// CHECK-LABEL: sil_witness_table hidden X: CP module dynamic_self {
// CHECK: method #CP.f!1: {{.*}} : @_T012dynamic_self1XCAA2CPA2aDP1f{{[_0-9a-zA-Z]*}}FTW
