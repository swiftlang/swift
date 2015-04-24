// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | FileCheck %s

import Swift // just for Optional

typealias Int = Builtin.Int64
typealias Int64 = Builtin.Int64
typealias Bool = Builtin.Int1

var zero = getInt()
func getInt() -> Int { return zero }

func standalone_function(x: Int, _ y: Int) -> Int {
  return x
}

func higher_order_function(f: (x: Int, y: Int) -> Int, _ x: Int, _ y: Int) -> Int {
  return f(x: x, y: y)
}

func higher_order_function2(f: (Int, Int) -> Int, _ x: Int, _ y: Int) -> Int {
  return f(x, y)
}

// -- Entry point BBs correspond to curried arguments in left-to-right order.
// CHECK-LABEL: sil hidden @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
func curried_function(var x: Int)(var y: Int) -> Int {
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64):
  // CHECK: [[YADDR:%[0-9]+]] = alloc_box $Builtin.Int64
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Builtin.Int64

  return standalone_function(x, y)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[Y:%[0-9]+]] = load [[YADDR]]
  // CHECK: apply [[FUNC]]([[X]], [[Y]])

  // CHECK: return
}

// -- Curried generic function needs to forward archetypes through entry points
func generic_curried_function<T, U>(x: T)(y: U) { }

// -- Curried function that returns a function uncurries to the right "natural" level
// CHECK-LABEL: sil hidden @_TF9functions33curried_function_returns_function{{.*}} :  $@convention(thin) (Builtin.Int64, Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
func curried_function_returns_function(var x:Int)(var y:Int) -> (z:Int) -> Int {
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64):
  return { z in standalone_function(standalone_function(x, y), z) }
}
// -- Local function has extra uncurry level with context
// CHECK-LABEL: sil shared @_TFF9functions33curried_function_returns_function{{.*}} : $@convention(thin) (Builtin.Int64, @owned Builtin.NativeObject, @inout Builtin.Int64, @owned Builtin.NativeObject, @inout Builtin.Int64) -> Builtin.Int64
// bb0(%0 : $Builtin.NativeObject, %1 : $*Builtin.Int64, %2 : $Builtin.NativeObject, %3 : $*Builtin.Int64, %4 : $Builtin.Int64):

struct SomeStruct {
  // -- Constructors and methods are uncurried in 'self'
  // -- Instance methods use 'method' cc

  init(x:Int, y:Int) {}

  mutating
  func method(x: Int) {}

  mutating
  func curried_method(x: Int)(y: Int) {}

  static func static_method(x: Int) {}

  static func static_curried_method(x: Int)(y: Int) {}

  func generic_method<T>(x: T) {}
}

class SomeClass {
  // -- Constructors and methods are uncurried in 'self'
  // -- Instance methods use 'method' cc

  // CHECK-LABEL: sil hidden @_TFC9functions9SomeClasscfMS0_FT1xBi64_1yBi64__S0_ : $@convention(method) (Builtin.Int64, Builtin.Int64, @owned SomeClass) -> @owned SomeClass
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $SomeClass):

  // CHECK-LABEL: sil hidden @_TFC9functions9SomeClassCfMS0_FT1xBi64_1yBi64__S0_ : $@convention(thin) (Builtin.Int64, Builtin.Int64, @thick SomeClass.Type) -> @owned SomeClass
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $@thick SomeClass.Type):
  init(x:Int, y:Int) {}

  // CHECK-LABEL: sil hidden @_TFC9functions9SomeClass6method{{.*}} : $@convention(method) (Builtin.Int64, @guaranteed SomeClass) -> () 
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $SomeClass):
  func method(x: Int) {}

  // CHECK-LABEL: sil hidden @_TFC9functions9SomeClass14curried_method{{.*}} : $@convention(method) (Builtin.Int64, Builtin.Int64, @guaranteed SomeClass) -> ()
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $SomeClass):
  func curried_method(x: Int)(y: Int) {}

  // CHECK-LABEL: sil hidden @_TZFC9functions9SomeClass13static_method{{.*}} : $@convention(thin) (Builtin.Int64, @thick SomeClass.Type) -> ()
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $@thick SomeClass.Type):
  class func static_method(x: Int) {}

  // CHECK-LABEL: sil hidden @_TZFC9functions9SomeClass21static_curried_method{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, @thick SomeClass.Type) -> ()
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $@thick SomeClass.Type):
  class func static_curried_method(x: Int)(y: Int) {}

  var someProperty: Int {
    get {
      return zero
    }
    set {}
  }

  subscript(x:Int, y:Int) -> Int {
    get {
      return zero
    }
    set {}
  }

  func generic<T>(x: T) -> T {
    return x
  }
}

func SomeClassWithBenefits() -> SomeClass.Type {
  return SomeClass.self
}

protocol SomeProtocol {
  func method(x: Int)
  static func static_method(x: Int)
}

struct ConformsToSomeProtocol : SomeProtocol {
  func method(x: Int) { }
  static func static_method(x: Int) { }
}

class SomeGeneric<T> {
  init() { }
  func method(x: T) -> T { return x }

  func generic<U>(x: U) -> U { return x }
}

// CHECK-LABEL: sil hidden @_TF9functions5calls{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, Builtin.Int64) -> ()
func calls(var i:Int, var j:Int, var k:Int) {
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $Builtin.Int64):
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Builtin.Int64
  // CHECK: [[JADDR:%[0-9]+]] = alloc_box $Builtin.Int64
  // CHECK: [[KADDR:%[0-9]+]] = alloc_box $Builtin.Int64

  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[I]], [[J]])
  standalone_function(i, j)

  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  curried_function(i)(y: j)

  // -- Paren exprs shouldn't affect the uncurrying optimization.
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  (curried_function)(i)(y: j)

  // -- Coercions and function conversions shouldn't affect the uncurrying
  //    optimization.
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  (curried_function)(i)(y: j)

  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @_TF9functions33curried_function_returns_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[FUNC2:%[0-9]+]] = apply [[FUNC1]]([[J]], [[I]])
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: apply [[FUNC2]]([[K]])
  curried_function_returns_function(i)(y: j)(z: k)

  generic_curried_function(i)(y: j)
  // -- Use of curried entry points as values.

  // CHECK: [[F1ADDR:%[0-9]+]] = alloc_box $@callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[FUNC_CURRY:%[0-9]+]] = apply [[FUNC]]([[I]])
  // CHECK: store [[FUNC_CURRY]] to [[F1ADDR]]
  var f1 = curried_function(i)
  // CHECK: [[F1:%[0-9]+]] = load [[F1ADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F1]]([[J]])
  f1(y: j)

  // CHECK: [[F2ADDR:%[0-9]+]] = alloc_box $@callee_owned (Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC]] : ${{.*}} to $@callee_owned (Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
  // CHECK: store [[FUNC_THICK]] to [[F2ADDR]]
  var f2 = curried_function
  // CHECK: [[F2:%[0-9]+]] = load [[F2ADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[F2_I:%[0-9]+]] = apply [[F2]]([[I]])
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F2_I]]([[J]])
  f2(i)(y: j)

  var gcf1: Int -> () = generic_curried_function(i)
  gcf1(j)

  // -- Curry 'self' onto struct method argument lists.

  // CHECK: [[ST_ADDR:%.*]] = alloc_box $SomeStruct
  // CHECK: [[CTOR:%.*]] = function_ref @_TFV9functions10SomeStructCfMS0_FT1xBi64_1yBi64__S0_ : $@convention(thin) (Builtin.Int64, Builtin.Int64, @thin SomeStruct.Type) -> SomeStruct
  // CHECK: [[METATYPE:%.*]] = metatype $@thin SomeStruct.Type
  // CHECK: [[I:%.*]] = load [[IADDR]]
  // CHECK: [[J:%.*]] = load [[JADDR]]
  // CHECK: apply [[CTOR]]([[I]], [[J]], [[METATYPE]]) : $@convention(thin) (Builtin.Int64, Builtin.Int64, @thin SomeStruct.Type) -> SomeStruct
  var st = SomeStruct(x: i, y: j)

  // -- Use of unapplied struct methods as values.

  // CHECK: [[THUNK:%.*]] = function_ref @_TFV9functions10SomeStruct6method{{.*}}
  // CHECK: [[THUNK_THICK:%.*]] = thin_to_thick_function [[THUNK]]
  var stm1 = SomeStruct.method
  stm1(&st)(i)
  // CHECK: [[THUNK:%.*]] = function_ref @_TFV9functions10SomeStruct14curried_method{{.*}}
  // CHECK: [[THUNK_THICK:%.*]] = thin_to_thick_function [[THUNK]]
  var stm2 = SomeStruct.curried_method
  stm2(&st)(i)(y: j)

  // -- Curry 'self' onto method argument lists dispatched using class_method.

  // CHECK: [[CADDR:%[0-9]+]] = alloc_box $SomeClass
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TFC9functions9SomeClassCfMS0_FT1xBi64_1yBi64__S0_ : $@convention(thin) (Builtin.Int64, Builtin.Int64, @thick SomeClass.Type) -> @owned SomeClass
  // CHECK: [[META:%[0-9]+]] = metatype $@thick SomeClass.Type
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[C:%[0-9]+]] = apply [[FUNC]]([[I]], [[J]], [[META]])
  var c = SomeClass(x: i, y: j)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[C]])
  c.method(i)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[C]])
  c.curried_method(i)(y: j)

  // -- Curry 'self' onto unapplied methods dispatched using class_method.
  // CHECK: [[METHOD_CURRY_THUNK:%.*]] = function_ref @_TFC9functions9SomeClass6method{{.*}}
  // CHECK: apply [[METHOD_CURRY_THUNK]]
  var cm1 = SomeClass.method(c)
  cm1(i)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[C]])
  SomeClass.method(c)(i)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[C]])
  SomeClass.curried_method(c)(i)(y: j)

  // -- Curry 'self' onto unapplied methods, after applying side effects from a Type expression.

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[SIDEFUNC:%[0-9]+]] = function_ref @_TF9functions21SomeClassWithBenefitsFT_MCS_9SomeClass : $@convention(thin) () -> @thick SomeClass.Type
  // CHECK: apply [[SIDEFUNC]]()
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[C]])
  SomeClassWithBenefits().curried_method(c)(i)(y: j)

  // -- Curry the Type onto static method argument lists.
  
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META:%[0-9]+]] : {{.*}}, #SomeClass.static_method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[META]])
  c.dynamicType.static_method(i)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META:%[0-9]+]] : {{.*}}, #SomeClass.static_curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[META]])
  c.dynamicType.static_curried_method(i)(y: j)

  // FIXME: use of curried method entry points as values
  //var m1 = c.curried_method(i)
  //m1(j)
  //var m2 = c.curried_method
  //m2(i)(j)
  //var m3 = SomeClass.curried_method
  //m3(c)(i)(j)
  //var s1 = c.Type.static_curried_method(i)
  //s1(j)
  //var s2 = c.Type.static_curried_method
  //s2(i)(j)

  // -- Curry property accesses.

  // -- FIXME: class_method-ify class getters.
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = class_method {{.*}} : $SomeClass, #SomeClass.someProperty!getter.1
  // CHECK: apply [[GETTER]]([[C]])
  i = c.someProperty

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.someProperty!setter.1 : SomeClass -> (Builtin.Int64) -> ()
  // CHECK: apply [[SETTER]]([[I]], [[C]])
  c.someProperty = i

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.subscript!getter.1 : SomeClass -> (Builtin.Int64, Builtin.Int64) -> Builtin.Int64 , $@convention(method) (Builtin.Int64, Builtin.Int64, @guaranteed SomeClass) -> Builtin.Int64
  // CHECK: apply [[GETTER]]([[J]], [[K]], [[C]])
  i = c[j, k]

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.subscript!setter.1 : SomeClass -> (Builtin.Int64, Builtin.Int64, Builtin.Int64) -> () , $@convention(method) (Builtin.Int64, Builtin.Int64, Builtin.Int64, @guaranteed SomeClass) -> () 
  // CHECK: apply [[SETTER]]([[K]], [[I]], [[J]], [[C]])
  c[i, j] = k

  // -- Curry the projected concrete value in an existential (or its Type)
  // -- onto protocol type methods dispatched using protocol_method.

  // CHECK: [[PADDR:%[0-9]+]] = alloc_box $SomeProtocol
  var p : SomeProtocol = ConformsToSomeProtocol()

  // CHECK: [[TEMP:%.*]] = alloc_stack $SomeProtocol
  // CHECK: copy_addr [[PADDR]]#1 to [initialization] [[TEMP]]#1
  // CHECK: [[PVALUE:%[0-9]+]] = open_existential_addr [[TEMP]]#1 : $*SomeProtocol to $*[[OPENED:@opened(.*) SomeProtocol]]
  // CHECK: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED]], #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[PMETHOD]]<[[OPENED]]>([[I]], [[PVALUE]])
  // CHECK: destroy_addr [[PVALUE]]
  // CHECK: deinit_existential_addr [[TEMP]]#1
  // CHECK: dealloc_stack [[TEMP]]#0
  p.method(i)

  // CHECK: [[PVALUE:%[0-9]+]] = open_existential_addr [[PADDR:%.*]] : $*SomeProtocol to $*[[OPENED:@opened(.*) SomeProtocol]]
  // CHECK: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED]], #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[PMETHOD]]<[[OPENED]]>([[I]], [[PVALUE]])
  var sp : SomeProtocol = ConformsToSomeProtocol()
  sp.method(i)

  // FIXME: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED:@opened(.*) SomeProtocol]], #SomeProtocol.static_method!1
  // FIXME: [[I:%[0-9]+]] = load [[IADDR]]
  // FIXME: apply [[PMETHOD]]([[I]], [[PMETA]])
  // Needs existential metatypes
  //p.dynamicType.static_method(i)

  // -- Use an apply or partial_apply instruction to bind type parameters of a generic.

  // CHECK: [[GADDR:%[0-9]+]] = alloc_box $SomeGeneric<Builtin.Int64>
  // CHECK: [[CTOR_GEN:%[0-9]+]] = function_ref @_TFC9functions11SomeGenericCU__fMGS0_Q__FT_GS0_Q__ : $@convention(thin) <τ_0_0> (@thick SomeGeneric<τ_0_0>.Type) -> @owned SomeGeneric<τ_0_0>
  // CHECK: [[META:%[0-9]+]] = metatype $@thick SomeGeneric<Builtin.Int64>.Type
  // CHECK: apply [[CTOR_GEN]]<Builtin.Int64>([[META]])
  var g = SomeGeneric<Builtin.Int64>()

  // CHECK: [[G:%[0-9]+]] = load [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.method!1
  // CHECK: [[TMPI:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]]#1, [[TMPI]]#1, [[G]])
  g.method(i)

  // CHECK: [[G:%[0-9]+]] = load [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.generic!1
  // CHECK: [[TMPJ:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]]#1, [[TMPJ]]#1, [[G]])
  g.generic(j)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.generic!1
  // CHECK: [[TMPK:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]]#1, [[TMPK]]#1, [[C]])
  c.generic(k)

  // FIXME: curried generic entry points
  //var gm1 = g.method
  //gm1(i)

  //var gg1 : (Int) -> Int = g.generic
  //gg1(j)

  //var cg1 : (Int) -> Int = c.generic
  //cg1(k)

  // SIL-level "thin" function values need to be able to convert to
  // "thick" function values when stored, returned, or passed as arguments.

  // CHECK: [[FADDR:%[0-9]+]] = alloc_box $@callee_owned (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: store [[FUNC_THICK]] to [[FADDR]]
  var f = standalone_function
  // CHECK: [[F:%[0-9]+]] = load [[FADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F]]([[I]], [[J]])
  f(i, j)

  // CHECK: [[HOF:%[0-9]+]] = function_ref @_TF9functions21higher_order_function{{.*}} : $@convention(thin) {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[HOF]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function(standalone_function, i, j)

  // CHECK: [[HOF2:%[0-9]+]] = function_ref @_TF9functions22higher_order_function2{{.*}} : $@convention(thin) {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%.*]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[HOF2]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function2(standalone_function, i, j)
}

// -- Curried entry points
// CHECK-LABEL: sil shared @_TF9functions16curried_function{{.*}} : $@convention(thin) (Builtin.Int64) -> @owned @callee_owned (Builtin.Int64) -> Builtin.Int64
// CHECK: bb0([[X:%[0-9]+]] : $Builtin.Int64):
// CHECK:   [[FUNC:%[0-9]+]] = function_ref @_TF9functions16curried_function{{.*}}  : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
// CHECK:   [[CLOSURE:%[0-9]+]] = partial_apply [[FUNC]]([[X]])
// CHECK:   return [[CLOSURE]]

// CHECK-LABEL: sil shared @_TF9functions24generic_curried_function{{.*}} : $@convention(thin) <T, U> (@in T) -> @owned @callee_owned (@in U) -> () {
// CHECK: bb0([[X:%.*]] : $*T):
// CHECK:   [[UNCURRIED:%.*]] = function_ref @_TF9functions24generic_curried_function{{.*}}
// CHECK:   [[CURRIED:%.*]] = partial_apply [[UNCURRIED]]<T, U>([[X]])
// CHECK:   return [[CURRIED]] : $@callee_owned (@in U) -> ()

// CHECK-LABEL: sil shared @_TFV9functions10SomeStruct6method{{.*}} : $@convention(thin) (@inout SomeStruct) -> @owned @callee_owned (Builtin.Int64) -> () {
// CHECK:   [[UNCURRIED:%.*]] = function_ref @_TFV9functions10SomeStruct6method{{.*}} : $@convention(method) (Builtin.Int64, @inout SomeStruct) -> () // user: %2
// CHECK:   [[CURRIED:%.*]] = partial_apply [[UNCURRIED]]
// CHECK:   return [[CURRIED]]

// CHECK-LABEL: sil shared @_TFC9functions9SomeClass6method{{.*}} : $@convention(thin) (@owned SomeClass) -> @owned @callee_owned (Builtin.Int64) -> ()
// CHECK: bb0(%0 : $SomeClass):
// CHECK:   class_method %0 : $SomeClass, #SomeClass.method!1 : SomeClass -> (Builtin.Int64) -> ()
// CHECK:   %2 = partial_apply %1(%0)
// CHECK:   return %2

func return_func() -> (x: Builtin.Int64, y: Builtin.Int64) -> Builtin.Int64 {
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_TF9functions19standalone_function{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: return [[FUNC_THICK]]
  return standalone_function
}

func standalone_generic<T>(x: T, y: T) -> T { return x }

// CHECK-LABEL: sil hidden @_TF9functions14return_genericFT_FT1xBi64_1yBi64__Bi64_
func return_generic() -> (x:Builtin.Int64, y:Builtin.Int64) -> Builtin.Int64 {
  // CHECK: [[GEN:%.*]] = function_ref @_TF9functions18standalone_generic{{.*}} : $@convention(thin) <τ_0_0> (@out τ_0_0, @in τ_0_0, @in τ_0_0) -> ()
  // CHECK: [[SPEC:%.*]] = partial_apply [[GEN]]<Builtin.Int64>()
  // CHECK: [[THUNK:%.*]] = function_ref  @{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, @owned @callee_owned (@out Builtin.Int64, @in Builtin.Int64, @in Builtin.Int64) -> ()) -> Builtin.Int64
  // CHECK: [[T0:%.*]] = partial_apply [[THUNK]]([[SPEC]])
  // CHECK: return [[T0]]
  return standalone_generic
}

// CHECK-LABEL: sil hidden @_TF9functions20return_generic_tuple{{.*}}
func return_generic_tuple()
-> (x: (Builtin.Int64, Builtin.Int64), y: (Builtin.Int64, Builtin.Int64)) -> (Builtin.Int64, Builtin.Int64) {
  // CHECK: [[GEN:%.*]] = function_ref @_TF9functions18standalone_generic{{.*}}  : $@convention(thin) <τ_0_0> (@out τ_0_0, @in τ_0_0, @in τ_0_0) -> ()
  // CHECK: [[SPEC:%.*]] = partial_apply [[GEN]]<(Builtin.Int64, Builtin.Int64)>()
  // CHECK: [[THUNK:%.*]] = function_ref @{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, Builtin.Int64, Builtin.Int64, @owned @callee_owned (@out (Builtin.Int64, Builtin.Int64), @in (Builtin.Int64, Builtin.Int64), @in (Builtin.Int64, Builtin.Int64)) -> ()) -> (Builtin.Int64, Builtin.Int64)
  // CHECK: [[T0:%.*]] = partial_apply [[THUNK]]([[SPEC]])
  // CHECK: return [[T0]]
  return standalone_generic
}

// CHECK-LABEL: sil hidden @_TF9functions16testNoReturnAttrFT_T_ : $@convention(thin) @noreturn () -> ()
@noreturn func testNoReturnAttr() -> () {}
// CHECK-LABEL: sil hidden @_TF9functions20testNoReturnAttrPoly{{.*}} : $@convention(thin) @noreturn <T> (@in T) -> ()
@noreturn func testNoReturnAttrPoly<T>(x: T) -> () {}

// CHECK-LABEL: sil hidden @_TF9functions21testNoReturnAttrParam{{.*}} : $@convention(thin) (@owned @noreturn @callee_owned () -> ()) -> ()
func testNoReturnAttrParam(fptr: @noreturn ()->()) -> () {}

// CHECK-LABEL: sil hidden [transparent] @_TF9functions15testTransparent{{.*}} : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
@transparent func testTransparent(x: Bool) -> Bool {
  return x
}

// CHECK-LABEL: sil hidden @_TF9functions16applyTransparent{{.*}} : $@convention(thin) (Builtin.Int1) -> Builtin.Int1 {
func applyTransparent(x: Bool) -> Bool {
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF9functions15testTransparent{{.*}} : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
  // CHECK: apply [[FUNC]]({{%[0-9]+}}) : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
  return testTransparent(x)
}

// CHECK-LABEL: sil hidden @_TF9functions11funcToBlock{{.*}} : $@convention(thin) (@owned @callee_owned () -> ()) -> @owned @convention(block) () -> ()
// CHECK:         [[BLOCK_STORAGE:%.*]] = alloc_stack $@block_storage
// CHECK:         [[BLOCK:%.*]] = init_block_storage_header [[BLOCK_STORAGE]]
// CHECK:         [[COPY:%.*]] = copy_block [[BLOCK]] : $@convention(block) () -> ()
// CHECK:         return [[COPY]]
func funcToBlock(x: () -> ()) -> @convention(block) () -> () {
  return x
}

// CHECK-LABEL: sil hidden @_TF9functions11blockToFunc{{.*}} : $@convention(thin) (@owned @convention(block) () -> ()) -> @owned @callee_owned () -> ()
// CHECK:         [[COPIED:%.*]] = copy_block %0
// CHECK:         [[THUNK:%.*]] = function_ref @_TTRXFdCb__dT__XFo__dT__
// CHECK:         [[FUNC:%.*]] = partial_apply [[THUNK]]([[COPIED]])
// CHECK:         return [[FUNC]]
func blockToFunc(x: @convention(block) () -> ()) -> () -> () {
  return x
}

// CHECK-LABEL: sil hidden [noinline] @_TF9functions15noinline_calleeFT_T_ : $@convention(thin) () -> ()
@inline(never)
func noinline_callee() {}

// CHECK-LABEL: sil hidden [always_inline] @_TF9functions20always_inline_calleeFT_T_ : $@convention(thin) () -> ()
@inline(__always)
func always_inline_callee() {}

protocol AlwaysInline {
  func alwaysInlined()
}

// CHECK-LABEL: sil hidden [always_inline] @_TFV9functions19AlwaysInlinedMember13alwaysInlinedfS0_FT_T_ : $@convention(method) (AlwaysInlinedMember) -> () {

// protocol witness for functions.AlwaysInline.alwaysInlined <A : functions.AlwaysInline>(functions.AlwaysInline.Self)() -> () in conformance functions.AlwaysInlinedMember : functions.AlwaysInline in functions
// CHECK-LABEL: sil hidden [transparent] [thunk] [always_inline] @_TTWV9functions19AlwaysInlinedMemberS_12AlwaysInlineS_FS1_13alwaysInlinedUS1___fQPS1_FT_T_ : $@convention(witness_method) (@in_guaranteed AlwaysInlinedMember) -> () {
struct AlwaysInlinedMember : AlwaysInline {
  @inline(__always)
  func alwaysInlined() {}
}

// CHECK-LABEL: sil hidden [_semantics "foo"] @_TF9functions9semanticsFT_T_ : $@convention(thin) () -> ()
@_semantics("foo")
func semantics() {}


// <rdar://problem/17828355> curried final method on a class crashes in irgen
final class r17828355Class {
  func method(x : Int) {
    var a : r17828355Class
    var fn = a.method  // currying a final method.
  }
}

// The curry thunk for the method should not include a class_method instruction.
// CHECK-LABEL: sil shared @_TFC9functions14r17828355Class6methodFS0_FBi64_T_
// CHECK-NEXT: bb0(%0 : $r17828355Class):
// CHECK-NEXT: // function_ref functions.r17828355Class.method (functions.r17828355Class)(Builtin.Int64) -> ()
// CHECK-NEXT:  %1 = function_ref @_TFC9functions14r17828355Class6methodfS0_FBi64_T_ : $@convention(method) (Builtin.Int64, @guaranteed r17828355Class) -> ()
// CHECK-NEXT:  partial_apply %1(%0) : $@convention(method) (Builtin.Int64, @guaranteed r17828355Class) -> ()
// CHECK-NEXT:  return



// <rdar://problem/19981118> Swift 1.2 beta 2: Closures nested in @noescape closures copy, rather than reference, captured vars.
func noescapefunc(@noescape f: () -> ()) {}
func escapefunc(f : () -> ()) {}

func testNoescape() {
  // "a" must be captured by-box into noescapefunc because the inner closure
  // could escape it.
  var a = 0
  noescapefunc {
    escapefunc { a = 42 }
  }
  println(a)
}

// CHECK-LABEL: functions.testNoescape () -> ()
// CHECK-NEXT: sil hidden @_TF9functions12testNoescapeFT_T_ : $@convention(thin) () -> ()
// CHECK: function_ref functions.(testNoescape () -> ()).(closure #1)
// CHECK-NEXT: function_ref @_TFF9functions12testNoescapeFT_T_U_FT_T_ : $@convention(thin) (@owned Builtin.NativeObject, @inout Int) -> ()

// Despite being a noescape closure, this needs to capture 'a' by-box so it can
// be passed to the capturing closure.closure
// CHECK: functions.(testNoescape () -> ()).(closure #1)
// CHECK-NEXT: sil shared @_TFF9functions12testNoescapeFT_T_U_FT_T_ : $@convention(thin) (@owned Builtin.NativeObject, @inout Int) -> () {



func testNoescape2() {
  // "a" must be captured by-box into noescapefunc because the inner closure
  // could escape it.  This also checks for when the outer closure captures it
  // in a way that could be used with escape: the union of the two requirements
  // doesn't allow a by-address capture.
  var a = 0
  noescapefunc {
    escapefunc { a = 42 }
    println(a)
  }
  println(a)
}

// CHECK-LABEL: sil hidden @_TF9functions13testNoescape2FT_T_ : $@convention(thin) () -> () {

// CHECK: // functions.(testNoescape2 () -> ()).(closure #1)
// CHECK-NEXT: sil shared @_TFF9functions13testNoescape2FT_T_U_FT_T_ : $@convention(thin) (@owned Builtin.NativeObject, @inout Int) -> () {

// CHECK: // functions.(testNoescape2 () -> ()).(closure #1).(closure #1)
// CHECK-NEXT: sil shared @_TFFF9functions13testNoescape2FT_T_U_FT_T_U_FT_T_ : $@convention(thin) (@owned Builtin.NativeObject, @inout Int) -> () {

