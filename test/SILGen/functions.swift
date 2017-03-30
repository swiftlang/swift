// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-stdlib -parse-as-library -emit-silgen %s | %FileCheck %s

import Swift // just for Optional

func markUsed<T>(_ t: T) {}

typealias Int = Builtin.Int64
typealias Int64 = Builtin.Int64
typealias Bool = Builtin.Int1

var zero = getInt()
func getInt() -> Int { return zero }

func standalone_function(_ x: Int, _ y: Int) -> Int {
  return x
}

func higher_order_function(_ f: (_ x: Int, _ y: Int) -> Int, _ x: Int, _ y: Int) -> Int {
  return f(x, y)
}

func higher_order_function2(_ f: (Int, Int) -> Int, _ x: Int, _ y: Int) -> Int {
  return f(x, y)
}

struct SomeStruct {
  // -- Constructors and methods are uncurried in 'self'
  // -- Instance methods use 'method' cc

  init(x:Int, y:Int) {}

  mutating
  func method(_ x: Int) {}

  static func static_method(_ x: Int) {}

  func generic_method<T>(_ x: T) {}
}

class SomeClass {
  // -- Constructors and methods are uncurried in 'self'
  // -- Instance methods use 'method' cc

  // CHECK-LABEL: sil hidden @_T09functions9SomeClassC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Builtin.Int64, Builtin.Int64, @thick SomeClass.Type) -> @owned SomeClass
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $@thick SomeClass.Type):

  // CHECK-LABEL: sil hidden @_T09functions9SomeClassC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (Builtin.Int64, Builtin.Int64, @owned SomeClass) -> @owned SomeClass
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $SomeClass):
  init(x:Int, y:Int) {}

  // CHECK-LABEL: sil hidden @_T09functions9SomeClassC6method{{[_0-9a-zA-Z]*}}F : $@convention(method) (Builtin.Int64, @guaranteed SomeClass) -> () 
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $SomeClass):
  func method(_ x: Int) {}

  // CHECK-LABEL: sil hidden @_T09functions9SomeClassC13static_method{{[_0-9a-zA-Z]*}}FZ : $@convention(method) (Builtin.Int64, @thick SomeClass.Type) -> ()
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $@thick SomeClass.Type):
  class func static_method(_ x: Int) {}

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

  func generic<T>(_ x: T) -> T {
    return x
  }
}

func SomeClassWithBenefits() -> SomeClass.Type {
  return SomeClass.self
}

protocol SomeProtocol {
  func method(_ x: Int)
  static func static_method(_ x: Int)
}

struct ConformsToSomeProtocol : SomeProtocol {
  func method(_ x: Int) { }
  static func static_method(_ x: Int) { }
}

class SomeGeneric<T> {
  init() { }
  func method(_ x: T) -> T { return x }

  func generic<U>(_ x: U) -> U { return x }
}

// CHECK-LABEL: sil hidden @_T09functions5calls{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64, Builtin.Int64) -> ()
func calls(_ i:Int, j:Int, k:Int) {
  var i = i
  var j = j
  var k = k
  // CHECK: bb0(%0 : $Builtin.Int64, %1 : $Builtin.Int64, %2 : $Builtin.Int64):
  // CHECK: [[IBOX:%[0-9]+]] = alloc_box ${ var Builtin.Int64 }
  // CHECK: [[IADDR:%.*]] = project_box [[IBOX]]
  // CHECK: [[JBOX:%[0-9]+]] = alloc_box ${ var Builtin.Int64 }
  // CHECK: [[JADDR:%.*]] = project_box [[JBOX]]
  // CHECK: [[KBOX:%[0-9]+]] = alloc_box ${ var Builtin.Int64 }
  // CHECK: [[KADDR:%.*]] = project_box [[KBOX]]

  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T09functions19standalone_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: apply [[FUNC]]([[I]], [[J]])
  standalone_function(i, j)

  // -- Curry 'self' onto struct method argument lists.

  // CHECK: [[ST_ADDR:%.*]] = alloc_box ${ var SomeStruct }
  // CHECK: [[CTOR:%.*]] = function_ref @_T09functions10SomeStructV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Builtin.Int64, Builtin.Int64, @thin SomeStruct.Type) -> SomeStruct
  // CHECK: [[METATYPE:%.*]] = metatype $@thin SomeStruct.Type
  // CHECK: [[I:%.*]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%.*]] = load [trivial] [[JADDR]]
  // CHECK: apply [[CTOR]]([[I]], [[J]], [[METATYPE]]) : $@convention(method) (Builtin.Int64, Builtin.Int64, @thin SomeStruct.Type) -> SomeStruct
  var st = SomeStruct(x: i, y: j)

  // -- Use of unapplied struct methods as values.

  // CHECK: [[THUNK:%.*]] = function_ref @_T09functions10SomeStructV6method{{[_0-9a-zA-Z]*}}F
  // CHECK: [[THUNK_THICK:%.*]] = thin_to_thick_function [[THUNK]]
  var stm1 = SomeStruct.method
  stm1(&st)(i)

  // -- Curry 'self' onto method argument lists dispatched using class_method.

  // CHECK: [[CBOX:%[0-9]+]] = alloc_box ${ var SomeClass }
  // CHECK: [[CADDR:%.*]] = project_box [[CBOX]]
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T09functions9SomeClassC{{[_0-9a-zA-Z]*}}fC : $@convention(method) (Builtin.Int64, Builtin.Int64, @thick SomeClass.Type) -> @owned SomeClass
  // CHECK: [[META:%[0-9]+]] = metatype $@thick SomeClass.Type
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: [[C:%[0-9]+]] = apply [[FUNC]]([[I]], [[J]], [[META]])
  var c = SomeClass(x: i, y: j)

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.method!1
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[C]])
  // CHECK: destroy_value [[C]]
  c.method(i)

  // -- Curry 'self' onto unapplied methods dispatched using class_method.
  // CHECK: [[METHOD_CURRY_THUNK:%.*]] = function_ref @_T09functions9SomeClassC6method{{[_0-9a-zA-Z]*}}F
  // CHECK: apply [[METHOD_CURRY_THUNK]]
  var cm1 = SomeClass.method(c)
  cm1(i)

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.method!1
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[C]])
  // CHECK: destroy_value [[C]]
  SomeClass.method(c)(i)

  // -- Curry the Type onto static method argument lists.
  
  // CHECK: [[C:%[0-9]+]] = load_borrow [[CADDR]]
  // CHECK: [[META:%.*]] = value_metatype $@thick SomeClass.Type, [[C]]
  // CHECK: [[METHOD:%[0-9]+]] = class_method [[META]] : {{.*}}, #SomeClass.static_method!1
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: apply [[METHOD]]([[I]], [[META]])
  type(of: c).static_method(i)

  // -- Curry property accesses.

  // -- FIXME: class_method-ify class getters.
  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = class_method {{.*}} : $SomeClass, #SomeClass.someProperty!getter.1
  // CHECK: apply [[GETTER]]([[C]])
  // CHECK: destroy_value [[C]]
  i = c.someProperty

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.someProperty!setter.1 : (SomeClass) -> (Builtin.Int64) -> ()
  // CHECK: apply [[SETTER]]([[I]], [[C]])
  // CHECK: destroy_value [[C]]
  c.someProperty = i

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: [[K:%[0-9]+]] = load [trivial] [[KADDR]]
  // CHECK: [[GETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.subscript!getter.1 : (SomeClass) -> (Builtin.Int64, Builtin.Int64) -> Builtin.Int64, $@convention(method) (Builtin.Int64, Builtin.Int64, @guaranteed SomeClass) -> Builtin.Int64
  // CHECK: apply [[GETTER]]([[J]], [[K]], [[C]])
  // CHECK: destroy_value [[C]]
  i = c[j, k]

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: [[K:%[0-9]+]] = load [trivial] [[KADDR]]
  // CHECK: [[SETTER:%[0-9]+]] = class_method [[C]] : $SomeClass, #SomeClass.subscript!setter.1 : (SomeClass) -> (Builtin.Int64, Builtin.Int64, Builtin.Int64) -> (), $@convention(method) (Builtin.Int64, Builtin.Int64, Builtin.Int64, @guaranteed SomeClass) -> ()
  // CHECK: apply [[SETTER]]([[K]], [[I]], [[J]], [[C]])
  // CHECK: destroy_value [[C]]
  c[i, j] = k

  // -- Curry the projected concrete value in an existential (or its Type)
  // -- onto protocol type methods dispatched using protocol_method.

  // CHECK: [[PBOX:%[0-9]+]] = alloc_box ${ var SomeProtocol }
  // CHECK: [[PADDR:%.*]] = project_box [[PBOX]]
  var p : SomeProtocol = ConformsToSomeProtocol()

  // CHECK: [[TEMP:%.*]] = alloc_stack $SomeProtocol
  // CHECK: copy_addr [[PADDR]] to [initialization] [[TEMP]]
  // CHECK: [[PVALUE:%[0-9]+]] = open_existential_addr immutable_access [[TEMP]] : $*SomeProtocol to $*[[OPENED:@opened(.*) SomeProtocol]]
  // CHECK: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED]], #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: apply [[PMETHOD]]<[[OPENED]]>([[I]], [[PVALUE]])
  // CHECK: destroy_addr [[TEMP]]
  // CHECK: dealloc_stack [[TEMP]]
  p.method(i)

  // CHECK: [[PVALUE:%[0-9]+]] = open_existential_addr immutable_access [[PADDR:%.*]] : $*SomeProtocol to $*[[OPENED:@opened(.*) SomeProtocol]]
  // CHECK: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED]], #SomeProtocol.method!1
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: apply [[PMETHOD]]<[[OPENED]]>([[I]], [[PVALUE]])
  var sp : SomeProtocol = ConformsToSomeProtocol()
  sp.method(i)

  // FIXME: [[PMETHOD:%[0-9]+]] = witness_method $[[OPENED:@opened(.*) SomeProtocol]], #SomeProtocol.static_method!1
  // FIXME: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // FIXME: apply [[PMETHOD]]([[I]], [[PMETA]])
  // Needs existential metatypes
  //type(of: p).static_method(i)

  // -- Use an apply or partial_apply instruction to bind type parameters of a generic.

  // CHECK: [[GBOX:%[0-9]+]] = alloc_box ${ var SomeGeneric<Builtin.Int64> }
  // CHECK: [[GADDR:%.*]] = project_box [[GBOX]]
  // CHECK: [[CTOR_GEN:%[0-9]+]] = function_ref @_T09functions11SomeGenericC{{[_0-9a-zA-Z]*}}fC : $@convention(method) <τ_0_0> (@thick SomeGeneric<τ_0_0>.Type) -> @owned SomeGeneric<τ_0_0>
  // CHECK: [[META:%[0-9]+]] = metatype $@thick SomeGeneric<Builtin.Int64>.Type
  // CHECK: apply [[CTOR_GEN]]<Builtin.Int64>([[META]])
  var g = SomeGeneric<Builtin.Int64>()

  // CHECK: [[G:%[0-9]+]] = load [copy] [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.method!1
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPI:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]], [[TMPI]], [[G]])
  // CHECK: destroy_value [[G]]
  g.method(i)

  // CHECK: [[G:%[0-9]+]] = load [copy] [[GADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[G]] : {{.*}}, #SomeGeneric.generic!1
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPJ:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]], [[TMPJ]], [[G]])
  // CHECK: destroy_value [[G]]
  g.generic(j)

  // CHECK: [[C:%[0-9]+]] = load [copy] [[CADDR]]
  // CHECK: [[METHOD_GEN:%[0-9]+]] = class_method [[C]] : {{.*}}, #SomeClass.generic!1
  // CHECK: [[TMPR:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: [[TMPK:%.*]] = alloc_stack $Builtin.Int64
  // CHECK: apply [[METHOD_GEN]]<{{.*}}>([[TMPR]], [[TMPK]], [[C]])
  // CHECK: destroy_value [[C]]
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

  // CHECK: [[FBOX:%[0-9]+]] = alloc_box ${ var @callee_owned (Builtin.Int64, Builtin.Int64) -> Builtin.Int64 }
  // CHECK: [[FADDR:%.*]] = project_box [[FBOX]]
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T09functions19standalone_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: store [[FUNC_THICK]] to [init] [[FADDR]]
  var f = standalone_function
  // CHECK: [[F:%[0-9]+]] = load [copy] [[FADDR]]
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: apply [[F]]([[I]], [[J]])
  f(i, j)

  // CHECK: [[HOF:%[0-9]+]] = function_ref @_T09functions21higher_order_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T09functions19standalone_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: apply [[HOF]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function(standalone_function, i, j)

  // CHECK: [[HOF2:%[0-9]+]] = function_ref @_T09functions22higher_order_function2{{[_0-9a-zA-Z]*}}F : $@convention(thin) {{.*}}
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T09functions19standalone_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%.*]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: [[I:%[0-9]+]] = load [trivial] [[IADDR]]
  // CHECK: [[J:%[0-9]+]] = load [trivial] [[JADDR]]
  // CHECK: apply [[HOF2]]([[FUNC_THICK]], [[I]], [[J]])
  higher_order_function2(standalone_function, i, j)
}

// -- Curried entry points
// CHECK-LABEL: sil shared [thunk] @_T09functions10SomeStructV6method{{[_0-9a-zA-Z]*}}FTc : $@convention(thin) (@inout SomeStruct) -> @owned @callee_owned (Builtin.Int64) -> () {
// CHECK:   [[UNCURRIED:%.*]] = function_ref @_T09functions10SomeStructV6method{{[_0-9a-zA-Z]*}}F : $@convention(method) (Builtin.Int64, @inout SomeStruct) -> (){{.*}} // user: %2
// CHECK:   [[CURRIED:%.*]] = partial_apply [[UNCURRIED]]
// CHECK:   return [[CURRIED]]

// CHECK-LABEL: sil shared [thunk] @_T09functions9SomeClassC6method{{[_0-9a-zA-Z]*}}FTc : $@convention(thin) (@owned SomeClass) -> @owned @callee_owned (Builtin.Int64) -> ()
// CHECK: bb0(%0 : $SomeClass):
// CHECK:   class_method %0 : $SomeClass, #SomeClass.method!1 : (SomeClass) -> (Builtin.Int64) -> ()
// CHECK:   %2 = partial_apply %1(%0)
// CHECK:   return %2

func return_func() -> (_ x: Builtin.Int64, _ y: Builtin.Int64) -> Builtin.Int64 {
  // CHECK: [[FUNC_THIN:%[0-9]+]] = function_ref @_T09functions19standalone_function{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int64, Builtin.Int64) -> Builtin.Int64
  // CHECK: [[FUNC_THICK:%[0-9]+]] = thin_to_thick_function [[FUNC_THIN]]
  // CHECK: return [[FUNC_THICK]]
  return standalone_function
}

func standalone_generic<T>(_ x: T, y: T) -> T { return x }

// CHECK-LABEL: sil hidden @_T09functions14return_genericBi64_Bi64__Bi64_tcyF
func return_generic() -> (_ x:Builtin.Int64, _ y:Builtin.Int64) -> Builtin.Int64 {
  // CHECK: [[GEN:%.*]] = function_ref @_T09functions18standalone_generic{{[_0-9a-zA-Z]*}}F : $@convention(thin) <τ_0_0> (@in τ_0_0, @in τ_0_0) -> @out τ_0_0
  // CHECK: [[SPEC:%.*]] = partial_apply [[GEN]]<Builtin.Int64>()
  // CHECK: [[THUNK:%.*]] = function_ref  @{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, @owned @callee_owned (@in Builtin.Int64, @in Builtin.Int64) -> @out Builtin.Int64) -> Builtin.Int64
  // CHECK: [[T0:%.*]] = partial_apply [[THUNK]]([[SPEC]])
  // CHECK: return [[T0]]
  return standalone_generic
}

// CHECK-LABEL: sil hidden @_T09functions20return_generic_tuple{{[_0-9a-zA-Z]*}}F
func return_generic_tuple()
-> (_ x: (Builtin.Int64, Builtin.Int64), _ y: (Builtin.Int64, Builtin.Int64)) -> (Builtin.Int64, Builtin.Int64) {
  // CHECK: [[GEN:%.*]] = function_ref @_T09functions18standalone_generic{{[_0-9a-zA-Z]*}}F  : $@convention(thin) <τ_0_0> (@in τ_0_0, @in τ_0_0) -> @out τ_0_0
  // CHECK: [[SPEC:%.*]] = partial_apply [[GEN]]<(Builtin.Int64, Builtin.Int64)>()
  // CHECK: [[THUNK:%.*]] = function_ref @{{.*}} : $@convention(thin) (Builtin.Int64, Builtin.Int64, Builtin.Int64, Builtin.Int64, @owned @callee_owned (@in (Builtin.Int64, Builtin.Int64), @in (Builtin.Int64, Builtin.Int64)) -> @out (Builtin.Int64, Builtin.Int64)) -> (Builtin.Int64, Builtin.Int64)
  // CHECK: [[T0:%.*]] = partial_apply [[THUNK]]([[SPEC]])
  // CHECK: return [[T0]]
  return standalone_generic
}

// CHECK-LABEL: sil hidden @_T09functions16testNoReturnAttrs5NeverOyF : $@convention(thin) () -> Never
func testNoReturnAttr() -> Never {}
// CHECK-LABEL: sil hidden @_T09functions20testNoReturnAttrPoly{{[_0-9a-zA-Z]*}}F : $@convention(thin) <T> (@in T) -> Never
func testNoReturnAttrPoly<T>(_ x: T) -> Never {}

// CHECK-LABEL: sil hidden @_T09functions21testNoReturnAttrParam{{[_0-9a-zA-Z]*}}F : $@convention(thin) (@owned @callee_owned () -> Never) -> ()
func testNoReturnAttrParam(_ fptr: () -> Never) -> () {}

// CHECK-LABEL: sil hidden [transparent] @_T09functions15testTransparent{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
@_transparent func testTransparent(_ x: Bool) -> Bool {
  return x
}

// CHECK-LABEL: sil hidden @_T09functions16applyTransparent{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int1) -> Builtin.Int1 {
func applyTransparent(_ x: Bool) -> Bool {
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T09functions15testTransparent{{[_0-9a-zA-Z]*}}F : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
  // CHECK: apply [[FUNC]]({{%[0-9]+}}) : $@convention(thin) (Builtin.Int1) -> Builtin.Int1
  return testTransparent(x)
}

// CHECK-LABEL: sil hidden [noinline] @_T09functions15noinline_calleeyyF : $@convention(thin) () -> ()
@inline(never)
func noinline_callee() {}

// CHECK-LABEL: sil hidden [always_inline] @_T09functions20always_inline_calleeyyF : $@convention(thin) () -> ()
@inline(__always)
func always_inline_callee() {}

// CHECK-LABEL: sil [serialized] [always_inline] @_T09functions27public_always_inline_calleeyyF : $@convention(thin) () -> ()
@inline(__always)
public func public_always_inline_callee() {}

protocol AlwaysInline {
  func alwaysInlined()
}

// CHECK-LABEL: sil hidden [always_inline] @_T09functions19AlwaysInlinedMemberV06alwaysC0{{[_0-9a-zA-Z]*}}F : $@convention(method) (AlwaysInlinedMember) -> () {

// protocol witness for functions.AlwaysInline.alwaysInlined <A : functions.AlwaysInline>(functions.AlwaysInline.Self)() -> () in conformance functions.AlwaysInlinedMember : functions.AlwaysInline in functions
// CHECK-LABEL: sil hidden [transparent] [thunk] [always_inline] @_T09functions19AlwaysInlinedMemberVAA0B6InlineA2aDP06alwaysC0{{[_0-9a-zA-Z]*}}FTW : $@convention(witness_method) (@in_guaranteed AlwaysInlinedMember) -> () {
struct AlwaysInlinedMember : AlwaysInline {
  @inline(__always)
  func alwaysInlined() {}
}

// CHECK-LABEL: sil hidden [_semantics "foo"] @_T09functions9semanticsyyF : $@convention(thin) () -> ()
@_semantics("foo")
func semantics() {}


// <rdar://problem/17828355> curried final method on a class crashes in irgen
final class r17828355Class {
  func method(_ x : Int) {
    var a : r17828355Class
    var fn = a.method  // currying a final method.
  }
}

// The curry thunk for the method should not include a class_method instruction.
// CHECK-LABEL: sil shared [thunk] @_T09functions14r17828355ClassC6method
// CHECK: bb0(%0 : $r17828355Class):
// CHECK-NEXT: // function_ref functions.r17828355Class.method (Builtin.Int64) -> ()
// CHECK-NEXT:  %1 = function_ref @_T09functions14r17828355ClassC6method{{[_0-9a-zA-Z]*}}F : $@convention(method) (Builtin.Int64, @guaranteed r17828355Class) -> ()
// CHECK-NEXT:  partial_apply %1(%0) : $@convention(method) (Builtin.Int64, @guaranteed r17828355Class) -> ()
// CHECK-NEXT:  return



// <rdar://problem/19981118> Swift 1.2 beta 2: Closures nested in closures copy, rather than reference, captured vars.
func noescapefunc(f: () -> ()) {}
func escapefunc(_ f : @escaping () -> ()) {}

func testNoescape() {
  // "a" must be captured by-box into noescapefunc because the inner closure
  // could escape it.
  var a = 0
  noescapefunc {
    escapefunc { a = 42 }
  }
  markUsed(a)
}

// CHECK-LABEL: functions.testNoescape () -> ()
// CHECK-NEXT: sil hidden @_T09functions12testNoescapeyyF : $@convention(thin) () -> ()
// CHECK: function_ref functions.(testNoescape () -> ()).(closure #1)
// CHECK-NEXT: function_ref @_T09functions12testNoescapeyyFyycfU_ : $@convention(thin) (@owned { var Int }) -> ()

// Despite being a noescape closure, this needs to capture 'a' by-box so it can
// be passed to the capturing closure.closure
// CHECK: functions.(testNoescape () -> ()).(closure #1)
// CHECK-NEXT: sil shared @_T09functions12testNoescapeyyFyycfU_ : $@convention(thin) (@owned { var Int }) -> () {



func testNoescape2() {
  // "a" must be captured by-box into noescapefunc because the inner closure
  // could escape it.  This also checks for when the outer closure captures it
  // in a way that could be used with escape: the union of the two requirements
  // doesn't allow a by-address capture.
  var a = 0
  noescapefunc {
    escapefunc { a = 42 }
    markUsed(a)
  }
  markUsed(a)
}

// CHECK-LABEL: sil hidden @_T09functions13testNoescape2yyF : $@convention(thin) () -> () {

// CHECK: // functions.(testNoescape2 () -> ()).(closure #1)
// CHECK-NEXT: sil shared @_T09functions13testNoescape2yyFyycfU_ : $@convention(thin) (@owned { var Int }) -> () {

// CHECK: // functions.(testNoescape2 () -> ()).(closure #1).(closure #1)
// CHECK-NEXT: sil shared @_T09functions13testNoescape2yyFyycfU_yycfU_ : $@convention(thin) (@owned { var Int }) -> () {

enum PartialApplyEnumPayload<T, U> {
  case Left(T)
  case Right(U)
}

struct S {}
struct C {}

func partialApplyEnumCases(_ x: S, y: C) {
  let left = PartialApplyEnumPayload<S, C>.Left
  let left2 = left(S())

  let right = PartialApplyEnumPayload<S, C>.Right
  let right2 = right(C())
}

// CHECK-LABEL: sil shared [transparent] [thunk] @_T09functions23PartialApplyEnumPayloadO4Left{{[_0-9a-zA-Z]*}}F
// CHECK:         [[UNCURRIED:%.*]] = function_ref @_T09functions23PartialApplyEnumPayloadO4Left{{[_0-9a-zA-Z]*}}F
// CHECK:         [[CLOSURE:%.*]] = partial_apply [[UNCURRIED]]<T, U>(%0)
// CHECK:         return [[CLOSURE]]

// CHECK-LABEL: sil shared [transparent] [thunk] @_T09functions23PartialApplyEnumPayloadO5Right{{[_0-9a-zA-Z]*}}F
// CHECK:         [[UNCURRIED:%.*]] = function_ref @_T09functions23PartialApplyEnumPayloadO5Right{{[_0-9a-zA-Z]*}}F
// CHECK:         [[CLOSURE:%.*]] = partial_apply [[UNCURRIED]]<T, U>(%0)
// CHECK:         return [[CLOSURE]]
