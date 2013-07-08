// RUN: %swift -parse-as-library -emit-sil %s | FileCheck %s

func standalone_function(x:Int, y:Int) -> Int {
  return x
}

func higher_order_function(f:(x:Int, y:Int) -> Int, x:Int, y:Int) -> Int {
  return f(x, y)
}

func higher_order_function2(f:(Int, Int) -> Int, x:Int, y:Int) -> Int {
  return f(x, y)
}

// -- Entry point BBs correspond to curried arguments in left-to-right order.
// CHECK: sil @_T9functions16curried_functionfT1xSi_FT1ySi_Si : $[thin] ((y : Int64), (x : Int64)) -> Int64
func curried_function(x:Int)(y:Int) -> Int {
  // CHECK: bb0(%0 : $Int64, %1 : $Int64):
  // CHECK: [[YADDR:%[0-9]+]] = alloc_var stack $Int64
  // CHECK: [[XADDR:%[0-9]+]] = alloc_var stack $Int64

  return standalone_function(x, y)
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[Y:%[0-9]+]] = load [[YADDR]]
  // CHECK: apply [[FUNC]]([[X]], [[Y]])

  // CHECK: return
}

// -- Curried entry points
// CHECK: sil @_T9functions16curried_functionFT1xSi_FT1ySi_Si : $[thin] (x : Int64) -> (y : Int64) -> Int64
// CHECK: bb0([[X:%[0-9]+]] : $Int64):
// CHECK:   [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionfT1xSi_FT1ySi_Si  : $[thin] ((y : Int64), (x : Int64)) -> Int64
// CHECK:   [[CLOSURE:%[0-9]+]] = partial_apply [[FUNC]]([[X]])
// CHECK:   return [[CLOSURE]]

// -- Curried function that returns a function uncurries to the right "natural" level
// CHECK: sil @_T9functions33curried_function_returns_functionfT1xSi_FT1ySi_FT1zSi_Si :  $[thin] ((y : Int64), (x : Int64)) -> (z : Int64) -> Int64
func curried_function_returns_function(x:Int)(y:Int) -> (z:Int) -> Int {
  // CHECK: bb0(%0 : $Int64, %1 : $Int64):
  return { |z| standalone_function(standalone_function(x, y), z) }
}
// -- Local function has extra uncurry level with context
// CHECK: sil internal @closure0 : $[thin] ((z : Int64), (Builtin.ObjectPointer, [byref] Int64, Builtin.ObjectPointer, [byref] Int64)) -> Int64
// bb0(%0 : $Builtin.ObjectPointer, %1 : $*Int64, %2 : $Builtin.ObjectPointer, %3 : $*Int64, %4 : $Int64):

class SomeClass {
  // -- Constructors and methods are uncurried in 'this'
  // -- Instance methods use 'method' cc

  // CHECK: sil @_TC9functions9SomeClassCfMS0_FT1xSi1ySi_S0_ : $[thin] ((x : Int64, y : Int64), SomeClass.metatype) -> SomeClass
  // CHECK: bb0(%0 : $Int64, %1 : $Int64, %2 : $SomeClass.metatype):
  // CHECK: sil @_TC9functions9SomeClasscfMS0_FT1xSi1ySi_S0_ : $[cc(method), thin] ((x : Int64, y : Int64), SomeClass) -> SomeClass
  // CHECK: bb0(%0 : $Int64, %1 : $Int64, %2 : $SomeClass):
  constructor(x:Int, y:Int) {}

  // CHECK: sil @_TC9functions9SomeClass6methodfS0_FT1xSi_T_ : $[cc(method), thin] ((x : Int64), SomeClass) -> () 
  // CHECK: bb0(%0 : $Int64, %1 : $SomeClass):
  func method(x:Int) {}

  // CHECK: sil @_TC9functions9SomeClass14curried_methodfS0_fT1xSi_FT1ySi_T_ : $[cc(method), thin] ((y : Int64), (x : Int64), SomeClass) -> ()
  // CHECK: bb0(%0 : $Int64, %1 : $Int64, %2 : $SomeClass):
  func curried_method(x:Int)(y:Int) {}

  // CHECK: sil @_TC9functions9SomeClass13static_methodfMS0_FT1xSi_T_ : $[thin] ((x : Int64), SomeClass.metatype) -> ()
  // CHECK: bb0(%0 : $Int64, %1 : $SomeClass.metatype):
  static func static_method(x:Int) {}

  // CHECK: sil @_TC9functions9SomeClass21static_curried_methodfMS0_fT1xSi_FT1ySi_T_ : $[thin] ((y : Int64), (x : Int64), SomeClass.metatype) -> ()
  // CHECK: bb0(%0 : $Int64, %1 : $Int64, %2 : $SomeClass.metatype):
  static func static_curried_method(x:Int)(y:Int) {}

  var someProperty : Int {
  get:
    return 0
  set:
  }

  subscript(x:Int, y:Int) -> Int {
  get:
    return 0
  set:
  }

  func generic<T>(x:T) -> T {
    return x
  }
}

func SomeClassWithBenefits() -> SomeClass.metatype {
  return SomeClass
}

protocol SomeProtocol {
  func method(x:Int)
  static func static_method(x:Int)
}

struct ConformsToSomeProtocol : SomeProtocol {
  func method(x : Int) { }
  static func static_method(x : Int) { }
}

class SomeGeneric<T> {
  func method(x:T) -> T { return x }

  func generic<U>(x:U) -> U { return x }
}

// CHECK: sil @_T9functions5callsFT1iSi1jSi1kSi_T_ : $[thin] (i : Int64, j : Int64, k : Int64) -> ()
func calls(i:Int, j:Int, k:Int) {
  // CHECK: bb0(%0 : $Int64, %1 : $Int64, %2 : $Int64):
  // CHECK: [[IADDR:%[0-9]+]] = alloc_var stack $Int64
  // CHECK: [[JADDR:%[0-9]+]] = alloc_var stack $Int64
  // CHECK: [[KADDR:%[0-9]+]] = alloc_var stack $Int64

  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[I]], [[J]])
  standalone_function(i, j)

  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionfT1xSi_FT1ySi_Si : $[thin] ((y : Int64), (x : Int64)) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  curried_function(i)(j)

  // -- Paren exprs shouldn't affect the uncurrying optimization.
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionfT1xSi_FT1ySi_Si : $[thin] ((y : Int64), (x : Int64)) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  (curried_function)(i)(j)

  // -- Coercions and function conversions shouldn't affect the uncurrying
  //    optimization.
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionfT1xSi_FT1ySi_Si : $[thin] ((y : Int64), (x : Int64)) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[FUNC]]([[J]], [[I]])
  (curried_function as (x:Int) -> (y:Int) -> Int)(i)(j)

  // CHECK: [[FUNC1:%[0-9]+]] = function_ref @_T9functions33curried_function_returns_functionfT1xSi_FT1ySi_FT1zSi_Si : $[thin] ((y : Int64), (x : Int64)) -> (z : Int64) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[FUNC2:%[0-9]+]] = apply [[FUNC1]]([[J]], [[I]])
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: apply [[FUNC2]]([[K]])
  curried_function_returns_function(i)(j)(k)

  // -- Use of curried entry points as values.

  // CHECK: [[F1ADDR:%[0-9]+]] = alloc_var stack $(y : Int64) -> Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionFT1xSi_FT1ySi_Si : $[thin] (x : Int64) -> (y : Int64) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[FUNC_CURRY:%[0-9]+]] = apply [[FUNC]]([[I]])
  // CHECK: store [[FUNC_CURRY]] to [[F1ADDR]]
  var f1 = curried_function(i)
  // CHECK: [[F1:%[0-9]+]] = load [[F1ADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F1]]([[J]])
  f1(j)

  // CHECK: [[F2ADDR:%[0-9]+]] = alloc_var stack $(x : Int64) -> (y : Int64) -> Int64
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T9functions16curried_functionFT1xSi_FT1ySi_Si : $[thin] (x : Int64) -> (y : Int64) -> Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC]] : ${{.*}} to $(x : Int64) -> (y : Int64) -> Int64
  // CHECK: store [[FUNC_THICK]] to [[F2ADDR]]
  var f2 = curried_function
  // CHECK: [[F2:%[0-9]+]] = load [[F2ADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[F2_I:%[0-9]+]] = apply [[F2]]([[I]])
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F2_I]]([[J]])
  f2(i)(j)

  // CHECK: [[CADDR:%[0-9]+]] = alloc_var stack $SomeClass
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TC9functions9SomeClassCfMS0_FT1xSi1ySi_S0_ : $[thin] ((x : Int64, y : Int64), SomeClass.metatype) -> SomeClass
  // CHECK: [[META:%[0-9]+]] = metatype $SomeClass.metatype
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[C:%[0-9]+]] = apply [[FUNC]]([[I]], [[J]], [[META]])
  var c = SomeClass(i, j)

  // -- Curry 'this' onto method argument lists dispatched using class_method.

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
  c.curried_method(i)(j)

  // -- Curry 'this' onto unapplied methods dispatched using class_method.

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
  SomeClass.curried_method(c)(i)(j)

  // -- Curry 'this' onto unapplied methods, after applying side effects from a metatype expression.

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[SIDEFUNC:%[0-9]+]] = function_ref @_T9functions21SomeClassWithBenefitsFT_MCS_9SomeClass : $[thin] () -> SomeClass.metatype
  // CHECK: apply [[SIDEFUNC]]()
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[C]])
  SomeClassWithBenefits().curried_method(c)(i)(j)

  // -- Curry the metatype onto static method argument lists.
  
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META:%[0-9]+]] : {{.*}}, #SomeClass.static_method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[META]])
  typeof(c).static_method(i)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META:%[0-9]+]] : {{.*}}, #SomeClass.static_curried_method!2
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD]]([[J]], [[I]], [[META]])
  typeof(c).static_curried_method(i)(j)

  // FIXME: use of curried method entry points as values
  //var m1 = c.curried_method(i)
  //m1(j)
  //var m2 = c.curried_method
  //m2(i)(j)
  //var m3 = SomeClass.curried_method
  //m3(c)(i)(j)
  //var s1 = c.metatype.static_curried_method(i)
  //s1(j)
  //var s2 = c.metatype.static_curried_method
  //s2(i)(j)

  // -- Curry property accesses.

  // -- FIXME: class_method-ify class getters.
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TC9functions9SomeClass12somePropertySig : $[cc(method), thin] ((), SomeClass) -> Int64
  // CHECK: apply [[GETTER]]([[C]])
  i = c.someProperty

  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_TC9functions9SomeClass12somePropertySis : $[cc(method), thin] ((value : Int64), SomeClass) -> ()
  // CHECK: apply [[SETTER]]([[I]], [[C]])
  c.someProperty = i

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = function_ref @_TC9functions9SomeClass11__subscriptFT1xSi1ySi_Sig : $[cc(method), thin] ((), (x : Int64, y : Int64), SomeClass) -> Int64
  // CHECK: apply [[GETTER]]([[J]], [[K]], [[C]])
  i = c[j, k]

  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = function_ref @_TC9functions9SomeClass11__subscriptFT1xSi1ySi_Sis : $[cc(method), thin] ((value : Int64), (x : Int64, y : Int64), SomeClass) -> ()
  // CHECK: apply [[SETTER]]([[K]], [[I]], [[J]], [[C]])
  c[i, j] = k

  // -- Curry the projected concrete value in an existential (or its metatype)
  // -- onto protocol type methods dispatched using protocol_method.

  // CHECK: [[PADDR:%[0-9]+]] = alloc_var stack $SomeProtocol
  var p : SomeProtocol = ConformsToSomeProtocol()

  // CHECK: [[PVALUE:%[0-9]+]] = project_existential [[PADDR]]
  // CHECK: [[PMETHOD:%[0-9]+]] = protocol_method [[PADDR]] : {{.*}}, #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[PMETHOD]]([[I]], [[PVALUE]])
  p.method(i)

  // CHECK: [[PVALUE:%[0-9]+]] = project_existential [[PADDR:%.*]] :
  // CHECK: [[PMETHOD:%[0-9]+]] = protocol_method [[PADDR]] : {{.*}}, #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[PMETHOD]]([[I]], [[PVALUE]])
  // -- Protocol methods do not consume 'this' container
  // CHECK: destroy_addr [[PADDR]]
  (ConformsToSomeProtocol() as SomeProtocol).method(i)

  // CHECK: [[PMETHOD:%[0-9]+]] = protocol_method [[PMETA:%[0-9]+]] : {{.*}}, #SomeProtocol.static_method!1
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[PMETHOD]]([[I]], [[PMETA]])
  typeof(p).static_method(i)

  // -- Use a specialize instruction to bind type parameters of a generic.

  // CHECK: [[GADDR:%[0-9]+]] = alloc_var stack $SomeGeneric<Int64>
  // CHECK: [[CTOR_GEN:%[0-9]+]] = function_ref @_TC9functions11SomeGenericCU__fMGS0_Q__FT_GS0_Q__ : $[thin] <T> ((), SomeGeneric<T>.metatype) -> SomeGeneric<T>
  // CHECK: [[CTOR_SPEC:%[0-9]+]] = specialize [[CTOR_GEN]], $[thin] ((), SomeGeneric<Int64>.metatype) -> SomeGeneric<Int64>, T = Int64
  // CHECK: [[META:%[0-9]+]] = metatype $SomeGeneric<Int64>.metatype
  // CHECK: apply [[CTOR_SPEC]]([[META]])
  var g = SomeGeneric<Int64>()

  // CHECK: [[G:%[0-9]+]] = load [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.method!1
  // CHECK: [[METHOD_SPEC:%[0-9]+]] = specialize [[METHOD_GEN]], $[cc(method), thin] ((x : Int64), SomeGeneric<Int64>) -> Int64, T = Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: apply [[METHOD_SPEC]]([[I]], [[G]])
  g.method(i)

  // CHECK: [[G:%[0-9]+]] = load [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.generic!1
  // CHECK: [[METHOD_SPEC:%[0-9]+]] = specialize [[METHOD_GEN]], $[cc(method), thin] ((x : Int64), SomeGeneric<Int64>) -> Int64, T = Int64, U = Int
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[METHOD_SPEC]]([[J]], [[G]])
  g.generic(j)

  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.generic!1
  // CHECK: [[METHOD_SPEC:%[0-9]+]] = specialize [[METHOD_GEN]], $[cc(method), thin] ((x : Int64), SomeClass) -> Int64, T = Int
  // CHECK: [[K:%[0-9]+]] = load [[KADDR]]
  // CHECK: apply [[METHOD_SPEC]]([[K]], [[C]])
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

  // CHECK: [[FADDR:%[0-9]+]] = alloc_var stack $(x : Int64, y : Int64) -> Int64
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: store [[FUNC_THICK]] to [[FADDR]]
  var f = standalone_function
  // CHECK: [[F:%[0-9]+]] = load [[FADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: apply [[F]]([[I]], [[J]])
  f(i, j)

  // CHECK: [[HOF:%[0-9]+]] = function_ref @_T9functions21higher_order_functionFT1fFT1xSi1ySi_Si1xSi1ySi_Si : $[thin] {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: apply [[HOF]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function(standalone_function, i, j)

  // CHECK: [[HOF2:%[0-9]+]] = function_ref @_T9functions22higher_order_function2FT1fFTSiSi_Si1xSi1ySi_Si : $[thin] {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[FUNC_CONV:%[0-9]+]] = convert_function [[FUNC_THIN]] : ${{.*}} to $[thin] (Int64, Int64) -> Int64
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [[JADDR]]
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_CONV]]
  // CHECK: apply [[HOF2]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function2(standalone_function, i, j)
}

func return_func() -> (x:Int64, y:Int64) -> Int64 {
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T9functions19standalone_functionFT1xSi1ySi_Si : $[thin] (x : Int64, y : Int64) -> Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: return [[FUNC_THICK]]
  return standalone_function
}

func standalone_generic<T>(x:T, y:T) -> T { return x }

// CHECK: sil @_T9functions14return_genericFT_FT1xSi1ySi_Si
func return_generic() -> (x:Int64, y:Int64) -> Int64 {
  // CHECK: [[GEN:%.*]] = function_ref @_T9functions18standalone_genericU__FT1xQ_1yQ__Q_ : $[thin] <T> (x : T, y : T) -> T
  // CHECK: [[SPEC:%.*]] = specialize [[GEN]], $[thin] (x : Int64, y : Int64) -> Int64, T = Int64
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[SPEC]] : ${{.*}} to $(x : Int64, y : Int64) -> Int64
  // CHECK: return [[THICK]]
  return standalone_generic
}

// CHECK: sil @_T9functions20return_generic_tupleFT_FT1xTSiSi_1yTSiSi__TSiSi_
func return_generic_tuple()
-> (x:(Int64, Int64), y:(Int64, Int64)) -> (Int64, Int64) {
  // CHECK: [[GEN:%.*]] = function_ref @_T9functions18standalone_genericU__FT1xQ_1yQ__Q_  : $[thin] <T> (x : T, y : T) -> T
  // CHECK: [[SPEC:%.*]] = specialize [[GEN]], $[thin] (x : (Int64, Int64), y : (Int64, Int64)) -> (Int64, Int64), T = (Int64, Int64)
  // CHECK: [[THICK:%.*]] = thin_to_thick_function [[SPEC]] : ${{.*}} to $(x : (Int64, Int64), y : (Int64, Int64)) -> (Int64, Int64)
  // CHECK: return [[THICK]]
  return standalone_generic
}
