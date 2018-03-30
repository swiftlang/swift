
// RUN: %target-swift-frontend -module-name dynamic_self -emit-silgen -enable-sil-ownership %s -disable-objc-attr-requires-foundation-module | %FileCheck %s
// RUN: %target-swift-frontend -module-name dynamic_self -emit-sil -O %s -disable-objc-attr-requires-foundation-module
// RUN: %target-swift-frontend -module-name dynamic_self -emit-ir %s -disable-objc-attr-requires-foundation-module

protocol P {
  func f() -> Self
}

protocol CP : class {
  func f() -> Self
}

class X : P, CP {
  required init(int i: Int) { }

  // CHECK-LABEL: sil hidden @$S12dynamic_self1XC1f{{[_0-9a-zA-Z]*}}F : $@convention(method) (@guaranteed X) -> @owned
  func f() -> Self { return self }

  // CHECK-LABEL: sil hidden @$S12dynamic_self1XC7factory{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (Int, @thick X.Type) -> @owned X
  // CHECK: bb0([[I:%[0-9]+]] : @trivial $Int, [[SELF:%[0-9]+]] : @trivial $@thick X.Type):
  // CHECK: [[DYNAMIC_SELF:%[0-9]+]] = unchecked_trivial_bit_cast [[SELF]] : $@thick X.Type to $@thick @dynamic_self X.Type
  // CHECK: [[STATIC_SELF:%[0-9]+]] = upcast [[DYNAMIC_SELF]] : $@thick @dynamic_self X.Type to $@thick X.Type
  // CHECK: [[CTOR:%[0-9]+]] = class_method [[STATIC_SELF]] : $@thick X.Type, #X.init!allocator.1 : (X.Type) -> (Int) -> X, $@convention(method) (Int, @thick X.Type) -> @owned X
  // CHECK: apply [[CTOR]]([[I]], [[STATIC_SELF]]) : $@convention(method) (Int, @thick X.Type) -> @owned X
  class func factory(i: Int) -> Self { return self.init(int: i) }
}

class Y : X { 
  required init(int i: Int) {
    super.init(int: i)
  }
}

class GX<T> {
  func f() -> Self { return self }
}

class GY<T> : GX<[T]> { }

// CHECK-LABEL: sil hidden @$S12dynamic_self23testDynamicSelfDispatch{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed Y) -> ()
// CHECK: bb0([[Y:%[0-9]+]] : @guaranteed $Y):
// CHECK:   [[Y_AS_X:%[0-9]+]] = upcast [[Y]] : $Y to $X
// CHECK:   [[X_F:%[0-9]+]] = class_method [[Y_AS_X]] : $X, #X.f!1 : (X) -> () -> @dynamic_self X, $@convention(method) (@guaranteed X) -> @owned X
// CHECK:   [[X_RESULT:%[0-9]+]] = apply [[X_F]]([[Y_AS_X]]) : $@convention(method) (@guaranteed X) -> @owned X
// CHECK:   [[Y_RESULT:%[0-9]+]] = unchecked_ref_cast [[X_RESULT]] : $X to $Y
// CHECK:   destroy_value [[Y_RESULT]] : $Y
func testDynamicSelfDispatch(y: Y) {
  _ = y.f()
}

// CHECK-LABEL: sil hidden @$S12dynamic_self30testDynamicSelfDispatchGeneric{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed GY<Int>) -> ()
func testDynamicSelfDispatchGeneric(gy: GY<Int>) {
  // CHECK: bb0([[GY:%[0-9]+]] : @guaranteed $GY<Int>):
  // CHECK:   [[GY_AS_GX:%[0-9]+]] = upcast [[GY]] : $GY<Int> to $GX<Array<Int>>
  // CHECK:   [[GX_F:%[0-9]+]] = class_method [[GY_AS_GX]] : $GX<Array<Int>>, #GX.f!1 : <T> (GX<T>) -> () -> @dynamic_self GX<T>, $@convention(method) <τ_0_0> (@guaranteed GX<τ_0_0>) -> @owned GX<τ_0_0>
  // CHECK:   [[GX_RESULT:%[0-9]+]] = apply [[GX_F]]<[Int]>([[GY_AS_GX]]) : $@convention(method) <τ_0_0> (@guaranteed GX<τ_0_0>) -> @owned GX<τ_0_0>
  // CHECK:   [[GY_RESULT:%[0-9]+]] = unchecked_ref_cast [[GX_RESULT]] : $GX<Array<Int>> to $GY<Int>
  // CHECK:   destroy_value [[GY_RESULT]] : $GY<Int>
  _ = gy.f()
}

// CHECK-LABEL: sil hidden @$S12dynamic_self21testArchetypeDispatch{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T where T : P> (@in_guaranteed T) -> ()
func testArchetypeDispatch<T: P>(t: T) {
  // CHECK: bb0([[T:%[0-9]+]] : @trivial $*T):
  // CHECK:   [[T_RESULT:%[0-9]+]] = alloc_stack $T
  // CHECK:   [[ARCHETYPE_F:%[0-9]+]] = witness_method $T, #P.f!1 : {{.*}} : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  // CHECK:   [[SELF_RESULT:%[0-9]+]] = apply [[ARCHETYPE_F]]<T>([[T_RESULT]], [[T]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
  _ = t.f()
}

// CHECK-LABEL: sil hidden @$S12dynamic_self23testExistentialDispatch{{[_0-9a-zA-Z]*}}F
func testExistentialDispatch(p: P) {
// CHECK: bb0([[P:%[0-9]+]] : @trivial $*P):
// CHECK:   [[PCOPY_ADDR:%[0-9]+]] = open_existential_addr immutable_access [[P]] : $*P to $*@opened([[N:".*"]]) P
// CHECK:   [[P_RESULT:%[0-9]+]] = alloc_stack $P
// CHECK:   [[P_RESULT_ADDR:%[0-9]+]] = init_existential_addr [[P_RESULT]] : $*P, $@opened([[N]]) P
// CHECK:   [[P_F_METHOD:%[0-9]+]] = witness_method $@opened([[N]]) P, #P.f!1 : {{.*}}, [[PCOPY_ADDR]]{{.*}} : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   apply [[P_F_METHOD]]<@opened([[N]]) P>([[P_RESULT_ADDR]], [[PCOPY_ADDR]]) : $@convention(witness_method: P) <τ_0_0 where τ_0_0 : P> (@in_guaranteed τ_0_0) -> @out τ_0_0
// CHECK:   destroy_addr [[P_RESULT]] : $*P
// CHECK:   dealloc_stack [[P_RESULT]] : $*P
  _ = p.f()
}

// CHECK-LABEL: sil hidden @$S12dynamic_self28testExistentialDispatchClass{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@guaranteed CP) -> ()
// CHECK: bb0([[CP:%[0-9]+]] : @guaranteed $CP):
// CHECK:   [[CP_ADDR:%[0-9]+]] = open_existential_ref [[CP]] : $CP to $@opened([[N:".*"]]) CP
// CHECK:   [[CP_F:%[0-9]+]] = witness_method $@opened([[N]]) CP, #CP.f!1 : {{.*}}, [[CP_ADDR]]{{.*}} : $@convention(witness_method: CP) <τ_0_0 where τ_0_0 : CP> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK:   [[CP_F_RESULT:%[0-9]+]] = apply [[CP_F]]<@opened([[N]]) CP>([[CP_ADDR]]) : $@convention(witness_method: CP) <τ_0_0 where τ_0_0 : CP> (@guaranteed τ_0_0) -> @owned τ_0_0
// CHECK:   [[RESULT_EXISTENTIAL:%[0-9]+]] = init_existential_ref [[CP_F_RESULT]] : $@opened([[N]]) CP : $@opened([[N]]) CP, $CP
// CHECK:   destroy_value [[RESULT_EXISTENTIAL]]
func testExistentialDispatchClass(cp: CP) {
  _ = cp.f()
}

@objc class ObjC {
  @objc func method() -> Self { return self }
}

// CHECK-LABEL: sil hidden @$S12dynamic_self21testAnyObjectDispatch1oyyXl_tF : $@convention(thin) (@guaranteed AnyObject) -> () {
func testAnyObjectDispatch(o: AnyObject) {
  // CHECK: dynamic_method_br [[O_OBJ:%[0-9]+]] : $@opened({{.*}}) AnyObject, #ObjC.method!1.foreign, bb1, bb2

  // CHECK: bb1([[METHOD:%[0-9]+]] : @trivial $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @autoreleased AnyObject):
  // CHECK:   [[O_OBJ_COPY:%.*]] = copy_value [[O_OBJ]]
  // CHECK:   [[VAR_9:%[0-9]+]] = partial_apply [callee_guaranteed] [[METHOD]]([[O_OBJ_COPY]]) : $@convention(objc_method) (@opened({{.*}}) AnyObject) -> @autoreleased AnyObject
  var _ = o.method
}
// CHECK: } // end sil function '$S12dynamic_self21testAnyObjectDispatch1oyyXl_tF'


// <rdar://problem/16270889> Dispatch through ObjC metatypes.
class ObjCInit {
  dynamic required init() { }
}

// CHECK-LABEL: sil hidden @$S12dynamic_self12testObjCInit{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@thick ObjCInit.Type) -> ()
func testObjCInit(meta: ObjCInit.Type) {
// CHECK: bb0([[THICK_META:%[0-9]+]] : @trivial $@thick ObjCInit.Type):
// CHECK:   [[OBJC_META:%[0-9]+]] = thick_to_objc_metatype [[THICK_META]] : $@thick ObjCInit.Type to $@objc_metatype ObjCInit.Type
// CHECK:   [[OBJ:%[0-9]+]] = alloc_ref_dynamic [objc] [[OBJC_META]] : $@objc_metatype ObjCInit.Type, $ObjCInit
// CHECK:   [[INIT:%[0-9]+]] = objc_method [[OBJ]] : $ObjCInit, #ObjCInit.init!initializer.1.foreign : (ObjCInit.Type) -> () -> ObjCInit, $@convention(objc_method) (@owned ObjCInit) -> @owned ObjCInit
// CHECK:   [[RESULT_OBJ:%[0-9]+]] = apply [[INIT]]([[OBJ]]) : $@convention(objc_method) (@owned ObjCInit) -> @owned ObjCInit
// CHECK:   [[RESULT:%[0-9]+]] = tuple ()
// CHECK:   return [[RESULT]] : $()
  _ = meta.init()
}

class OptionalResult {
  func foo() -> Self? { return self }
}

// CHECK-LABEL: sil hidden @$S12dynamic_self14OptionalResultC3fooACXDSgyF : $@convention(method) (@guaranteed OptionalResult) -> @owned Optional<OptionalResult> {
// CHECK: bb0([[SELF:%.*]] : @guaranteed $OptionalResult):
// CHECK-NEXT: debug_value [[SELF]] : $OptionalResult
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK-NEXT: [[T0:%.*]] = enum $Optional<OptionalResult>, #Optional.some!enumelt.1, [[SELF_COPY]] : $OptionalResult
// CHECK-NEXT: return [[T0]] : $Optional<OptionalResult>
// CHECK: } // end sil function '$S12dynamic_self14OptionalResultC3fooACXDSgyF'

class OptionalResultInheritor : OptionalResult {
  func bar() {}
}

func testOptionalResult(v : OptionalResultInheritor) {
  v.foo()?.bar()
}

// CHECK-LABEL: sil hidden @$S12dynamic_self18testOptionalResult1vyAA0dE9InheritorC_tF : $@convention(thin) (@guaranteed OptionalResultInheritor) -> () {
// CHECK: bb0([[ARG:%.*]] : @guaranteed $OptionalResultInheritor):
// CHECK:      [[CAST_ARG:%.*]] = upcast [[ARG]]
// CHECK:      [[T0:%.*]] = class_method [[CAST_ARG]] : $OptionalResult, #OptionalResult.foo!1 : (OptionalResult) -> () -> @dynamic_self OptionalResult?, $@convention(method) (@guaranteed OptionalResult) -> @owned Optional<OptionalResult>
// CHECK-NEXT: [[RES:%.*]] = apply [[T0]]([[CAST_ARG]])
// CHECK-NEXT: [[T4:%.*]] = unchecked_ref_cast [[RES]] : $Optional<OptionalResult> to $Optional<OptionalResultInheritor>

func id<T>(_ t: T) -> T { return t }

class Z {

  required init() {}

  // CHECK-LABEL: sil hidden @$S12dynamic_self1ZC23testDynamicSelfCaptures1xACXDSi_tF : $@convention(method) (Int, @guaranteed Z) -> @owned Z {
  func testDynamicSelfCaptures(x: Int) -> Self {
    // CHECK: bb0({{.*}}, [[SELF:%.*]] : @guaranteed $Z):

    // Single capture of 'self' type

    // CHECK:      [[FN:%.*]] = function_ref @$S12dynamic_self1ZC23testDynamicSelfCaptures1xACXDSi_tFyycfU_ : $@convention(thin) (@guaranteed Z) -> ()
    // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Z
    // CHECK-NEXT: partial_apply [callee_guaranteed] [[FN]]([[SELF_COPY]])
    let fn1 = { _ = self }
    fn1()

    // Capturing 'self', but it's not the last capture. Make sure it ends
    // up at the end of the list anyway

    // CHECK:      [[FN:%.*]] = function_ref @$S12dynamic_self1ZC23testDynamicSelfCaptures1xACXDSi_tFyycfU0_ : $@convention(thin) (Int, @guaranteed Z) -> ()
    // CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]] : $Z
    // CHECK-NEXT: partial_apply [callee_guaranteed] [[FN]]({{.*}}, [[SELF_COPY]])
    let fn2 = {
      _ = self
      _ = x
    }
    fn2()

    // Capturing 'self' weak, so we have to pass in a metatype explicitly
    // so that IRGen can recover metadata.

    // CHECK:      [[WEAK_SELF:%.*]] = alloc_box ${ var @sil_weak Optional<Z> }
    // CHECK:      [[FN:%.*]] = function_ref @$S12dynamic_self1ZC23testDynamicSelfCaptures1xACXDSi_tFyycfU1_ : $@convention(thin) (@guaranteed { var @sil_weak Optional<Z> }, @thick @dynamic_self Z.Type) -> ()
    // CHECK:      [[WEAK_SELF_COPY:%.*]] = copy_value [[WEAK_SELF]] : ${ var @sil_weak Optional<Z> }
    // CHECK-NEXT: [[DYNAMIC_SELF:%.*]] = metatype $@thick @dynamic_self Z.Type
    // CHECK:      partial_apply [callee_guaranteed] [[FN]]([[WEAK_SELF_COPY]], [[DYNAMIC_SELF]]) : $@convention(thin) (@guaranteed { var @sil_weak Optional<Z> }, @thick @dynamic_self Z.Type) -> ()
    let fn3 = {
      [weak self] in
      _ = self
    }
    fn3()

    // Capturing a value with a complex type involving self

    // CHECK:      [[FN:%.*]] = function_ref @$S12dynamic_self1ZC23testDynamicSelfCaptures1xACXDSi_tFyycfU2_ : $@convention(thin) (@guaranteed (Z, Z), @thick @dynamic_self Z.Type) -> ()
    let xx = (self, self)
    let fn4 = {
      _ = xx
    }
    fn4()

    return self
  }

  // Capturing metatype of dynamic self
  static func testStaticMethodDynamicSelfCaptures() -> Self {
    let fn0 = { _ = self; _ = { _ = self } }
    fn0()

    let x = self
    let fn1 = { _ = x; _ = { _ = x } }
    fn1()

    let xx = (self, self)
    let fn2 = { _ = xx; _ = { _ = xx } }
    fn2()

    return self.init()
  }

  // Make sure the actual self value has the same lowered type as the
  // substituted result of a generic function call
  func testDynamicSelfSubstitution(_ b: Bool) -> Self {
    return b ? self : id(self)
  }

  // Same for metatype of self
  static func testStaticMethodDynamicSelfSubstitution(_ b: Bool) -> Self {
    _ = (b ? self : id(self))
    return self.init()
  }
}

// Unbound reference to a method returning Self.

class Factory {
  required init() {}

  func newInstance() -> Self { return self }
  class func classNewInstance() -> Self { return self.init() }
  static func staticNewInstance() -> Self { return self.init() }
}

// CHECK-LABEL: sil hidden @$S12dynamic_self22partialApplySelfReturn1c1tyAA7FactoryC_AFmtF : $@convention(thin) (@guaranteed Factory, @thick Factory.Type) -> ()
func partialApplySelfReturn(c: Factory, t: Factory.Type) {
  // CHECK: function_ref @$S12dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@guaranteed Factory) -> @owned @callee_guaranteed () -> @owned Factory
  _ = c.newInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@guaranteed Factory) -> @owned @callee_guaranteed () -> @owned Factory
  _ = Factory.newInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC11newInstanceACXDyFTc : $@convention(thin) (@guaranteed Factory) -> @owned @callee_guaranteed () -> @owned Factory
  _ = t.newInstance
  _ = type(of: c).newInstance

  // CHECK: function_ref @$S12dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = t.classNewInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = type(of: c).classNewInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC16classNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = Factory.classNewInstance

  // CHECK: function_ref @$S12dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = t.staticNewInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = type(of: c).staticNewInstance
  // CHECK: function_ref @$S12dynamic_self7FactoryC17staticNewInstanceACXDyFZTc : $@convention(thin) (@thick Factory.Type) -> @owned @callee_guaranteed () -> @owned Factory
  _ = Factory.staticNewInstance
}

class FactoryFactory {

  // CHECK-LABEL: sil hidden @$S12dynamic_self07FactoryC0C11newInstanceACXDyFZ : $@convention(method) (@thick FactoryFactory.Type) -> @owned FactoryFactory
  static func newInstance() -> Self {
    // CHECK: bb0(%0 : @trivial $@thick FactoryFactory.Type):

    // CHECK: [[DYNAMIC_SELF:%.*]] = unchecked_trivial_bit_cast %0 : $@thick FactoryFactory.Type to $@thick @dynamic_self FactoryFactory.Type
    // CHECK: [[METATYPE:%.*]] = value_metatype $@thick @dynamic_self FactoryFactory.Type.Type, [[DYNAMIC_SELF]] : $@thick @dynamic_self FactoryFactory.Type
    // CHECK: [[ANY:%.*]] = init_existential_metatype [[METATYPE]] : $@thick @dynamic_self FactoryFactory.Type.Type, $@thick Any.Type
    let _: Any.Type = type(of: self)

    while true {}
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
  // CHECK-LABEL: sil hidden @$S12dynamic_self7DerivedC9superCallyyF : $@convention(method) (@guaranteed Derived) -> ()
  // CHECK: [[SELF:%.*]] = copy_value %0
  // CHECK: [[SUPER:%.*]] = upcast [[SELF]] : $Derived to $Base
  // CHECK: [[BORROWED_SUPER:%.*]] = begin_borrow [[SUPER]]
  // CHECK: [[METHOD:%.*]] = function_ref @$S12dynamic_self4BaseC11returnsSelfACXDyF
  // CHECK: apply [[METHOD]]([[BORROWED_SUPER]])
  // CHECK: return
  func superCall() {
    _ = super.returnsSelf()
  }

  // CHECK-LABEL: sil hidden @$S12dynamic_self7DerivedC15superCallStaticyyFZ : $@convention(method) (@thick Derived.Type) -> ()
  // CHECK: [[SUPER:%.*]] = upcast %0 : $@thick Derived.Type to $@thick Base.Type
  // CHECK: [[METHOD:%.*]] = function_ref @$S12dynamic_self4BaseC17returnsSelfStaticACXDyFZ
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  static func superCallStatic() {
    _ = super.returnsSelfStatic()
  }

  // CHECK-LABEL: sil hidden @$S12dynamic_self7DerivedC32superCallFromMethodReturningSelfACXDyF : $@convention(method) (@guaranteed Derived) -> @owned Derived
  // CHECK: [[SELF:%.*]] = copy_value %0
  // CHECK: [[SUPER:%.*]] = upcast [[SELF]] : $Derived to $Base
  // CHEcK: [[BORROWED_SUPER:%.*]] = begin_borrow [[SUPER]]
  // CHECK: [[METHOD:%.*]] = function_ref @$S12dynamic_self4BaseC11returnsSelfACXDyF
  // CHECK: apply [[METHOD]]([[BORROWED_SUPER]])
  // CHECK: return
  func superCallFromMethodReturningSelf() -> Self {
    _ = super.returnsSelf()
    return self
  }

  // CHECK-LABEL: sil hidden @$S12dynamic_self7DerivedC38superCallFromMethodReturningSelfStaticACXDyFZ : $@convention(method) (@thick Derived.Type) -> @owned Derived
  // CHECK; [[DYNAMIC_SELF:%.*]] = unchecked_trivial_bit_cast %0 : $@thick Derived.Type to $@thick @synamic_self Derived.Type
  // CHECK: [[SUPER:%.*]] = upcast [[DYNAMIC_SELF]] : $@thick @dynamic_self Derived.Type to $@thick Base.Type
  // CHECK: [[METHOD:%.*]] = function_ref @$S12dynamic_self4BaseC17returnsSelfStaticACXDyFZ
  // CHECK: apply [[METHOD]]([[SUPER]])
  // CHECK: return
  static func superCallFromMethodReturningSelfStatic() -> Self {
    _ = super.returnsSelfStatic()
    return self.init()
  }
}

class Generic<T> {
  // Examples where we have to add a special argument to capture Self's metadata
  func t1() -> Self {
    // CHECK-LABEL: sil private @$S12dynamic_self7GenericC2t1ACyxGXDyFAEXDSgycfU_ : $@convention(thin) <T> (@guaranteed <τ_0_0> { var @sil_weak Optional<Generic<τ_0_0>> } <T>, @thick @dynamic_self Generic<T>.Type) -> @owned Optional<Generic<T>>
    _ = {[weak self] in self }
    return self
  }

  func t2() -> Self {
    // CHECK-LABEL: sil private @$S12dynamic_self7GenericC2t2ACyxGXDyFAEXD_AEXDtycfU_ : $@convention(thin) <T> (@guaranteed (Generic<T>, Generic<T>), @thick @dynamic_self Generic<T>.Type) -> (@owned Generic<T>, @owned Generic<T>)
    let selves = (self, self)
    _ = { selves }
    return self
  }

  func t3() -> Self {
    // CHECK-LABEL: sil private @$S12dynamic_self7GenericC2t3ACyxGXDyFAEXDycfU_ : $@convention(thin) <T> (@guaranteed @sil_unowned Generic<T>, @thick @dynamic_self Generic<T>.Type) -> @owned Generic<T> 
    _ = {[unowned self] in self }
    return self
  }
}

// CHECK-LABEL: sil_witness_table hidden X: P module dynamic_self {
// CHECK: method #P.f!1: {{.*}} : @$S12dynamic_self1XCAA1PA2aDP1f{{[_0-9a-zA-Z]*}}FTW

// CHECK-LABEL: sil_witness_table hidden X: CP module dynamic_self {
// CHECK: method #CP.f!1: {{.*}} : @$S12dynamic_self1XCAA2CPA2aDP1f{{[_0-9a-zA-Z]*}}FTW
