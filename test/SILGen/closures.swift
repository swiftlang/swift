// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | %FileCheck %s

import Swift

var zero = 0

// <rdar://problem/15921334>
// CHECK-LABEL: sil hidden @_TF8closures46return_local_generic_function_without_captures{{.*}} : $@convention(thin) <A, R> () -> @owned @callee_owned (@in A) -> @out R {
func return_local_generic_function_without_captures<A, R>() -> (A) -> R {
  func f(_: A) -> R {
    Builtin.int_trap()
  }
  // CHECK:  [[FN:%.*]] = function_ref @_TFF8closures46return_local_generic_function_without_captures{{.*}} : $@convention(thin) <τ_0_0, τ_0_1> (@in τ_0_0) -> @out τ_0_1
  // CHECK:  [[FN_WITH_GENERIC_PARAMS:%.*]] = partial_apply [[FN]]<A, R>() : $@convention(thin) <τ_0_0, τ_0_1> (@in τ_0_0) -> @out τ_0_1
  // CHECK:  return [[FN_WITH_GENERIC_PARAMS]] : $@callee_owned (@in A) -> @out R
  return f
}

func return_local_generic_function_with_captures<A, R>(_ a: A) -> (A) -> R {
  func f(_: A) -> R {
    _ = a
  }

  return f
}

// CHECK-LABEL: sil hidden @_TF8closures17read_only_captureFSiSi : $@convention(thin) (Int) -> Int {
func read_only_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK:   [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
  // SEMANTIC ARC TODO: This is incorrect. We need to do the project_box on the copy.
  // CHECK:   [[PROJECT:%.*]] = project_box [[XBOX]]
  // CHECK:   store [[X]] to [trivial] [[PROJECT]]

  func cap() -> Int {
    return x
  }

  return cap()
  // CHECK:   [[XBOX_COPY:%.*]] = copy_value [[XBOX]]
  // SEMANTIC ARC TODO: See above. This needs to happen on the copy_valued box.
  // CHECK:   mark_function_escape [[PROJECT]]
  // CHECK:   [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures17read_only_capture.*]] : $@convention(thin) (@owned { var Int }) -> Int
  // CHECK:   [[RET:%[0-9]+]] = apply [[CAP]]([[XBOX_COPY]])
  // CHECK:   destroy_value [[XBOX]]
  // CHECK:   return [[RET]]
}
// CHECK:   } // end sil function '_TF8closures17read_only_captureFSiSi'

// CHECK: sil shared @[[CAP_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : ${ var Int }):
// CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK: [[X:%[0-9]+]] = load [trivial] [[XADDR]]
// CHECK: destroy_value [[XBOX]]
// CHECK: return [[X]]
// } // end sil function '[[CAP_NAME]]'

// SEMANTIC ARC TODO: This is a place where we have again project_box too early.
// CHECK-LABEL: sil hidden @_TF8closures16write_to_captureFSiSi : $@convention(thin) (Int) -> Int {
func write_to_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK:   [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[XBOX_PB:%.*]] = project_box [[XBOX]]
  // CHECK:   store [[X]] to [trivial] [[XBOX_PB]]
  // CHECK:   [[X2BOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[X2BOX_PB:%.*]] = project_box [[X2BOX]]
  // CHECK:   copy_addr [[XBOX_PB]] to [initialization] [[X2BOX_PB]]
  // CHECK:   [[X2BOX_COPY:%.*]] = copy_value [[X2BOX]]
  // SEMANTIC ARC TODO: This next mark_function_escape should be on a projection from X2BOX_COPY.
  // CHECK:   mark_function_escape [[X2BOX_PB]]
  var x2 = x

  func scribble() {
    x2 = zero
  }

  scribble()
  // CHECK:   [[SCRIB:%[0-9]+]] = function_ref @[[SCRIB_NAME:_TFF8closures16write_to_capture.*]] : $@convention(thin) (@owned { var Int }) -> ()
  // CHECK:   apply [[SCRIB]]([[X2BOX_COPY]])
  // SEMANTIC ARC TODO: This should load from X2BOX_COPY project. There is an
  // issue here where after a copy_value, we need to reassign a projection in
  // some way.
  // CHECK:   [[RET:%[0-9]+]] = load [trivial] [[X2BOX_PB]]
  // CHECK:   destroy_value [[X2BOX]]
  // CHECK:   destroy_value [[XBOX]]
  // CHECK:   return [[RET]]
  return x2
}
// CHECK:  } // end sil function '_TF8closures16write_to_captureFSiSi'

// CHECK: sil shared @[[SCRIB_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : ${ var Int }):
// CHECK:   [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK:   copy_addr {{%[0-9]+}} to [[XADDR]]
// CHECK:   destroy_value [[XBOX]]
// CHECK:   return
// CHECK: } // end sil function '[[SCRIB_NAME]]'

// CHECK-LABEL: sil hidden @_TF8closures21multiple_closure_refs
func multiple_closure_refs(_ x: Int) -> (() -> Int, () -> Int) {
  var x = x
  func cap() -> Int {
    return x
  }

  return (cap, cap)
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures21multiple_closure_refs.*]] : $@convention(thin) (@owned { var Int }) -> Int
  // CHECK: [[CAP_CLOSURE_1:%[0-9]+]] = partial_apply [[CAP]]
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures21multiple_closure_refs.*]] : $@convention(thin) (@owned { var Int }) -> Int
  // CHECK: [[CAP_CLOSURE_2:%[0-9]+]] = partial_apply [[CAP]]
  // CHECK: [[RET:%[0-9]+]] = tuple ([[CAP_CLOSURE_1]] : {{.*}}, [[CAP_CLOSURE_2]] : {{.*}})
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @_TF8closures18capture_local_funcFSiFT_FT_Si : $@convention(thin) (Int) -> @owned @callee_owned () -> @owned @callee_owned () -> Int {
func capture_local_func(_ x: Int) -> () -> () -> Int {
  // CHECK: bb0([[ARG:%.*]] : $Int):
  var x = x
  // CHECK:   [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[XBOX_PB:%.*]] = project_box [[XBOX]]
  // CHECK:   store [[ARG]] to [trivial] [[XBOX_PB]]

  func aleph() -> Int { return x }

  func beth() -> () -> Int { return aleph }
  // CHECK: [[BETH_REF:%.*]] = function_ref @[[BETH_NAME:_TFF8closures18capture_local_funcFSiFT_FT_SiL_4bethfT_FT_Si]] : $@convention(thin) (@owned { var Int }) -> @owned @callee_owned () -> Int
  // CHECK: [[XBOX_COPY:%.*]] = copy_value [[XBOX]]
  // SEMANTIC ARC TODO: This is incorrect. This should be a project_box from XBOX_COPY.
  // CHECK: mark_function_escape [[XBOX_PB]]
  // CHECK: [[BETH_CLOSURE:%[0-9]+]] = partial_apply [[BETH_REF]]([[XBOX_COPY]])

  return beth
  // CHECK: destroy_value [[XBOX]]
  // CHECK: return [[BETH_CLOSURE]]
}
// CHECK: } // end sil function '_TF8closures18capture_local_funcFSiFT_FT_Si'

// CHECK: sil shared @[[ALEPH_NAME:_TFF8closures18capture_local_funcFSiFT_FT_SiL_5alephfT_Si]] : $@convention(thin) (@owned { var Int }) -> Int {
// CHECK: bb0([[XBOX:%[0-9]+]] : ${ var Int }):

// CHECK: sil shared @[[BETH_NAME]] : $@convention(thin) (@owned { var Int }) -> @owned @callee_owned () -> Int {
// CHECK: bb0([[XBOX:%[0-9]+]] : ${ var Int }):
// CHECK:   [[XBOX_PB:%.*]] = project_box [[XBOX]]
// CHECK:   [[ALEPH_REF:%[0-9]+]] = function_ref @[[ALEPH_NAME]] : $@convention(thin) (@owned { var Int }) -> Int
// CHECK:   [[XBOX_COPY:%.*]] = copy_value [[XBOX]]
// SEMANTIC ARC TODO: This should be on a PB from XBOX_COPY.
// CHECK:   mark_function_escape [[XBOX_PB]]
// CHECK:   [[ALEPH_CLOSURE:%[0-9]+]] = partial_apply [[ALEPH_REF]]([[XBOX_COPY]])
// CHECK:   destroy_value [[XBOX]]
// CHECK:   return [[ALEPH_CLOSURE]]
// CHECK: } // end sil function '[[BETH_NAME]]'

// CHECK-LABEL: sil hidden @_TF8closures22anon_read_only_capture
func anon_read_only_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]

  return ({ x })()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures22anon_read_only_capture.*]] : $@convention(thin) (@inout_aliasable Int) -> Int
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON]]([[PB]])
  // -- cleanup
  // CHECK: destroy_value [[XBOX]]
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [trivial] [[XADDR]]
// CHECK: return [[X]]

// CHECK-LABEL: sil hidden @_TF8closures21small_closure_capture
func small_closure_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]

  return { x }()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures21small_closure_capture.*]] : $@convention(thin) (@inout_aliasable Int) -> Int
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON]]([[PB]])
  // -- cleanup
  // CHECK: destroy_value [[XBOX]]
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [trivial] [[XADDR]]
// CHECK: return [[X]]


// CHECK-LABEL: sil hidden @_TF8closures35small_closure_capture_with_argument
func small_closure_capture_with_argument(_ x: Int) -> (_ y: Int) -> Int {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box ${ var Int }

  return { x + $0 }
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures35small_closure_capture_with_argument.*]] : $@convention(thin) (Int, @owned { var Int }) -> Int
  // CHECK: [[XBOX_COPY:%.*]] = copy_value [[XBOX]]
  // CHECK: [[ANON_CLOSURE_APP:%[0-9]+]] = partial_apply [[ANON]]([[XBOX_COPY]])
  // -- return
  // CHECK: destroy_value [[XBOX]]
  // CHECK: return [[ANON_CLOSURE_APP]]
}
// CHECK: sil shared @[[CLOSURE_NAME]] : $@convention(thin) (Int, @owned { var Int }) -> Int
// CHECK: bb0([[DOLLAR0:%[0-9]+]] : $Int, [[XBOX:%[0-9]+]] : ${ var Int }):
// CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK: [[PLUS:%[0-9]+]] = function_ref @_TFsoi1pFTSiSi_Si{{.*}}
// CHECK: [[LHS:%[0-9]+]] = load [trivial] [[XADDR]]
// CHECK: [[RET:%[0-9]+]] = apply [[PLUS]]([[LHS]], [[DOLLAR0]])
// CHECK: destroy_value [[XBOX]]
// CHECK: return [[RET]]

// CHECK-LABEL: sil hidden @_TF8closures24small_closure_no_capture
func small_closure_no_capture() -> (_ y: Int) -> Int {
  // CHECK:   [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures24small_closure_no_captureFT_FSiSiU_FSiSi]] : $@convention(thin) (Int) -> Int
  // CHECK:   [[ANON_THICK:%[0-9]+]] = thin_to_thick_function [[ANON]] : ${{.*}} to $@callee_owned (Int) -> Int
  // CHECK:   return [[ANON_THICK]]
  return { $0 }
}
// CHECK: sil shared @[[CLOSURE_NAME]] : $@convention(thin) (Int) -> Int
// CHECK: bb0([[YARG:%[0-9]+]] : $Int):

// CHECK-LABEL: sil hidden @_TF8closures17uncaptured_locals{{.*}} :
func uncaptured_locals(_ x: Int) -> (Int, Int) {
  var x = x
  // -- locals without captures are stack-allocated
  // CHECK: bb0([[XARG:%[0-9]+]] : $Int):
  // CHECK:   [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK:   [[PB:%.*]] = project_box [[XADDR]]
  // CHECK:   store [[XARG]] to [trivial] [[PB]]

  var y = zero
  // CHECK:   [[YADDR:%[0-9]+]] = alloc_box ${ var Int }
  return (x, y)

}

class SomeClass {
  var x : Int = zero

  init() {
    x = { self.x }()   // Closing over self.
  }
}

// Closures within destructors <rdar://problem/15883734>
class SomeGenericClass<T> {
  deinit {
    var i: Int = zero
    // CHECK: [[C1REF:%[0-9]+]] = function_ref @_TFFC8closures16SomeGenericClassdU_FT_Si : $@convention(thin) (@inout_aliasable Int) -> Int
    // CHECK: apply [[C1REF]]([[IBOX:%[0-9]+]]) : $@convention(thin) (@inout_aliasable Int) -> Int
    var x = { i + zero } ()

    // CHECK: [[C2REF:%[0-9]+]] = function_ref @_TFFC8closures16SomeGenericClassdU0_FT_Si : $@convention(thin) () -> Int
    // CHECK: apply [[C2REF]]() : $@convention(thin) () -> Int
    var y = { zero } ()

    // CHECK: [[C3REF:%[0-9]+]] = function_ref @_TFFC8closures16SomeGenericClassdU1_FT_T_ : $@convention(thin) <τ_0_0> () -> ()
    // CHECK: apply [[C3REF]]<T>() : $@convention(thin) <τ_0_0> () -> ()
    var z = { _ = T.self } ()
  }

  // CHECK-LABEL: sil shared @_TFFC8closures16SomeGenericClassdU_FT_Si : $@convention(thin) (@inout_aliasable Int) -> Int

  // CHECK-LABEL: sil shared @_TFFC8closures16SomeGenericClassdU0_FT_Si : $@convention(thin) () -> Int

  // CHECK-LABEL: sil shared @_TFFC8closures16SomeGenericClassdU1_FT_T_ : $@convention(thin) <T> () -> ()
}

// This is basically testing that the constraint system ranking
// function conversions as worse than others, and therefore performs
// the conversion within closures when possible.
class SomeSpecificClass : SomeClass {}
func takesSomeClassGenerator(_ fn : () -> SomeClass) {}
func generateWithConstant(_ x : SomeSpecificClass) {
  takesSomeClassGenerator({ x })
}

// CHECK-LABEL: sil shared @_TFF8closures20generateWithConstantFCS_17SomeSpecificClassT_U_FT_CS_9SomeClass : $@convention(thin) (@owned SomeSpecificClass) -> @owned SomeClass {
// CHECK: bb0([[T0:%.*]] : $SomeSpecificClass):
// CHECK:   debug_value [[T0]] : $SomeSpecificClass, let, name "x", argno 1
// CHECK:   [[BORROWED_T0:%.*]] = begin_borrow [[T0]]
// CHECK:   [[T0_COPY:%.*]] = copy_value [[BORROWED_T0]]
// CHECK:   [[T0_COPY_CASTED:%.*]] = upcast [[T0_COPY]] : $SomeSpecificClass to $SomeClass
// CHECK:   end_borrow [[BORROWED_T0]] from [[T0]]
// CHECK:   destroy_value [[T0]]
// CHECK:   return [[T0_COPY_CASTED]]
// CHECK: } // end sil function '_TFF8closures20generateWithConstantFCS_17SomeSpecificClassT_U_FT_CS_9SomeClass'


// Check the annoying case of capturing 'self' in a derived class 'init'
// method. We allocate a mutable box to deal with 'self' potentially being
// rebound by super.init, but 'self' is formally immutable and so is captured
// by value. <rdar://problem/15599464>
class Base {}

class SelfCapturedInInit : Base {
  var foo : () -> SelfCapturedInInit

  // CHECK-LABEL: sil hidden @_TFC8closures18SelfCapturedInInitc{{.*}} : $@convention(method) (@owned SelfCapturedInInit) -> @owned SelfCapturedInInit {
  // CHECK:         [[VAL:%.*]] = load_borrow {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load_borrow {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load [copy] {{%.*}} : $*SelfCapturedInInit
  // CHECK:         partial_apply {{%.*}}([[VAL]]) : $@convention(thin) (@owned SelfCapturedInInit) -> @owned SelfCapturedInInit
  override init() {
    super.init()
    foo = { self }
  }
}

func takeClosure(_ fn: () -> Int) -> Int { return fn() }

class TestCaptureList {
  var x = zero

  func testUnowned() {
    let aLet = self
    takeClosure { aLet.x }
    takeClosure { [unowned aLet] in aLet.x }
    takeClosure { [weak aLet] in aLet!.x }

    var aVar = self
    takeClosure { aVar.x }
    takeClosure { [unowned aVar] in aVar.x }
    takeClosure { [weak aVar] in aVar!.x }

    takeClosure { self.x }
    takeClosure { [unowned self] in self.x }
    takeClosure { [weak self] in self!.x }

    takeClosure { [unowned newVal = TestCaptureList()] in newVal.x }
    takeClosure { [weak newVal = TestCaptureList()] in newVal!.x }
  }
}

class ClassWithIntProperty { final var x = 42 }

func closeOverLetLValue() {
  let a : ClassWithIntProperty
  a = ClassWithIntProperty()
  
  takeClosure { a.x }
}

// The let property needs to be captured into a temporary stack slot so that it
// is loadable even though we capture the value.
// CHECK-LABEL: sil shared @_TFF8closures18closeOverLetLValueFT_T_U_FT_Si : $@convention(thin) (@owned ClassWithIntProperty) -> Int {
// CHECK: bb0([[ARG:%.*]] : $ClassWithIntProperty):
// CHECK:   [[TMP_CLASS_ADDR:%.*]] = alloc_stack $ClassWithIntProperty, let, name "a", argno 1
// CHECK:   store [[ARG]] to [init] [[TMP_CLASS_ADDR]] : $*ClassWithIntProperty
// CHECK:   [[LOADED_CLASS:%.*]] = load [copy] [[TMP_CLASS_ADDR]] : $*ClassWithIntProperty
// CHECK:   [[BORROWED_LOADED_CLASS:%.*]] = begin_borrow [[LOADED_CLASS]]
// CHECK:   [[INT_IN_CLASS_ADDR:%.*]] = ref_element_addr [[BORROWED_LOADED_CLASS]] : $ClassWithIntProperty, #ClassWithIntProperty.x
// CHECK:   [[INT_IN_CLASS:%.*]] = load [trivial] [[INT_IN_CLASS_ADDR]] : $*Int
// CHECK:   end_borrow [[BORROWED_LOADED_CLASS]] from [[LOADED_CLASS]]
// CHECK:   destroy_value [[LOADED_CLASS]]
// CHECK:   destroy_addr [[TMP_CLASS_ADDR]] : $*ClassWithIntProperty
// CHECK:   dealloc_stack %1 : $*ClassWithIntProperty
// CHECK:   return [[INT_IN_CLASS]]
// CHECK: } // end sil function '_TFF8closures18closeOverLetLValueFT_T_U_FT_Si'



// Use an external function so inout deshadowing cannot see its body.
@_silgen_name("takesNoEscapeClosure")
func takesNoEscapeClosure(fn : () -> Int)

struct StructWithMutatingMethod {
  var x = 42

  mutating func mutatingMethod() {
    // This should not capture the refcount of the self shadow.
    takesNoEscapeClosure { x = 42; return x }
  }
}

// Check that the address of self is passed in, but not the refcount pointer.

// CHECK-LABEL: sil hidden @_TFV8closures24StructWithMutatingMethod14mutatingMethod
// CHECK: bb0(%0 : $*StructWithMutatingMethod):
// CHECK: [[CLOSURE:%[0-9]+]] = function_ref @_TFFV8closures24StructWithMutatingMethod14mutatingMethod{{.*}} : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int
// CHECK: partial_apply [[CLOSURE]](%0) : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int

// Check that the closure body only takes the pointer.
// CHECK-LABEL: sil shared @_TFFV8closures24StructWithMutatingMethod14mutatingMethod{{.*}} : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int {
// CHECK:       bb0(%0 : $*StructWithMutatingMethod):

class SuperBase {
  func boom() {}
}
class SuperSub : SuperBase {
  override func boom() {}

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1afT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1a.*]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   apply [[INNER]]([[SELF_COPY]])
  // CHECK: } // end sil function '_TFC8closures8SuperSub1afT_T_'
  func a() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> () {
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[CLASS_METHOD:%.*]] = class_method [[BORROWED_ARG]] : $SuperSub, #SuperSub.boom!1
    // CHECK:   = apply [[CLASS_METHOD]]([[BORROWED_ARG]])
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
    // CHECK:   [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
    // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
    // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]])
    // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
    // => SEMANTIC SIL TODO: This is a result of a conversion not forwarding from the upcast.
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    func a1() {
      self.boom()
      super.boom()
    }
    a1()
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1bfT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1b.*]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   = apply [[INNER]]([[SELF_COPY]])
  // CHECK: } // end sil function '_TFC8closures8SuperSub1bfT_T_'
  func b() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> () {
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_2:_TFFFC8closures8SuperSub1b.*]] : $@convention(thin) (@owned SuperSub) -> ()
    // CHECK:   = apply [[INNER]]([[ARG_COPY]])
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    func b1() {
      // CHECK: sil shared @[[INNER_FUNC_2]] : $@convention(thin) (@owned SuperSub) -> () {
      // CHECK: bb0([[ARG:%.*]] : $SuperSub):
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[CLASS_METHOD:%.*]] = class_method [[BORROWED_ARG]] : $SuperSub, #SuperSub.boom!1
      // CHECK:   = apply [[CLASS_METHOD]]([[BORROWED_ARG]]) : $@convention(method) (@guaranteed SuperSub) -> ()
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
      // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
      // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
      // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]]) : $@convention(method) (@guaranteed SuperBase)
      // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
      // CHECK:   destroy_value [[ARG_COPY]]
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   destroy_value [[ARG]]
      // CHECK: } // end sil function '[[INNER_FUNC_2]]'
      func b2() {
        self.boom()
        super.boom()
      }
      b2()
    }
    b1()
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1cfT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1c.*]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[SELF_COPY]])
  // CHECK:   [[BORROWED_PA:%.*]] = begin_borrow [[PA]]
  // CHECK:   [[PA_COPY:%.*]] = copy_value [[BORROWED_PA]]
  // CHECK:   apply [[PA_COPY]]()
  // CHECK:   end_borrow [[BORROWED_PA]] from [[PA]]
  // CHECK:   destroy_value [[PA]]
  // CHECK: } // end sil function '_TFC8closures8SuperSub1cfT_T_'
  func c() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> ()
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[CLASS_METHOD:%.*]] = class_method [[BORROWED_ARG]] : $SuperSub, #SuperSub.boom!1
    // CHECK:   = apply [[CLASS_METHOD]]([[BORROWED_ARG]]) : $@convention(method) (@guaranteed SuperSub) -> ()
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
    // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
    // CHECK:   [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
    // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
    // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]])
    // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
    // CHECK:   destroy_value [[ARG_COPY]]
    // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    let c1 = { () -> Void in
      self.boom()
      super.boom()
    }
    c1()
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1dfT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1d.*]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[SELF_COPY]])
  // CHECK:   [[BORROWED_PA:%.*]] = begin_borrow [[PA]]
  // CHECK:   [[PA_COPY:%.*]] = copy_value [[BORROWED_PA]]
  // CHECK:   apply [[PA_COPY]]()
  // CHECK:   end_borrow [[BORROWED_PA]] from [[PA]]
  // CHECK:   destroy_value [[PA]]
  // CHECK: } // end sil function '_TFC8closures8SuperSub1dfT_T_'
  func d() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> () {
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_2:_TFFFC8closures8SuperSub1d.*]] : $@convention(thin) (@owned SuperSub) -> ()
    // CHECK:   = apply [[INNER]]([[ARG_COPY]])
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    let d1 = { () -> Void in
      // CHECK: sil shared @[[INNER_FUNC_2]] : $@convention(thin) (@owned SuperSub) -> () {
      // CHECK: bb0([[ARG:%.*]] : $SuperSub):
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
      // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
      // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
      // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]])
      // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
      // CHECK:   destroy_value [[ARG_COPY]]
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   destroy_value [[ARG]]
      // CHECK: } // end sil function '[[INNER_FUNC_2]]'
      func d2() {
        super.boom()
      }
      d2()
    }
    d1()
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1efT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK: [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK: [[INNER:%.*]] = function_ref @[[INNER_FUNC_NAME1:_TFFC8closures8SuperSub1e.*]] : $@convention(thin)
  // CHECK: = apply [[INNER]]([[SELF_COPY]])
  // CHECK: } // end sil function '_TFC8closures8SuperSub1efT_T_'
  func e() {
    // CHECK: sil shared @[[INNER_FUNC_NAME1]] : $@convention(thin)
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_NAME2:_TFFFC8closures8SuperSub1e.*]] : $@convention(thin)
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[ARG_COPY]])
    // CHECK:   [[BORROWED_PA:%.*]] = begin_borrow [[PA]]
    // CHECK:   [[PA_COPY:%.*]] = copy_value [[BORROWED_PA]]
    // CHECK:   apply [[PA_COPY]]() : $@callee_owned () -> ()
    // CHECK:   end_borrow [[BORROWED_PA]] from [[PA]]
    // CHECK:   destroy_value [[PA]]
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_NAME1]]'
    func e1() {
      // CHECK: sil shared @[[INNER_FUNC_NAME2]] : $@convention(thin)
      // CHECK: bb0([[ARG:%.*]] : $SuperSub):
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
      // CHECK:   [[ARG_COPY_SUPERCAST:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
      // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   [[BORROWED_ARG_COPY_SUPERCAST:%.*]] = begin_borrow [[ARG_COPY_SUPERCAST]]
      // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPERCAST]])
      // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPERCAST]] from [[ARG_COPY_SUPERCAST]]
      // CHECK:   destroy_value [[ARG_COPY]]
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   destroy_value [[ARG]]
      // CHECK:   return
      // CHECK: } // end sil function '[[INNER_FUNC_NAME2]]'
      let e2 = {
        super.boom()
      }
      e2()
    }
    e1()
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1ffT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1fFT_T_U_FT_T_]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[SELF_COPY]])
  // CHECK:   destroy_value [[PA]]
  // CHECK: } // end sil function '_TFC8closures8SuperSub1ffT_T_'
  func f() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> () {
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[TRY_APPLY_AUTOCLOSURE:%.*]] = function_ref @_TFsoi2qqurFzTGSqx_KzT_x_x : $@convention(thin) <τ_0_0> (@in Optional<τ_0_0>, @owned @callee_owned () -> (@out τ_0_0, @error Error)) -> (@out τ_0_0, @error Error)
    // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_2:_TFFFC8closures8SuperSub1fFT_T_U_FT_T_u_KzT_T_]] : $@convention(thin) (@owned SuperSub) -> @error Error
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[ARG_COPY]])
    // CHECK:   [[REABSTRACT_PA:%.*]] = partial_apply {{.*}}([[PA]])
    // CHECK:   try_apply [[TRY_APPLY_AUTOCLOSURE]]<()>({{.*}}, {{.*}}, [[REABSTRACT_PA]]) : $@convention(thin) <τ_0_0> (@in Optional<τ_0_0>, @owned @callee_owned () -> (@out τ_0_0, @error Error)) -> (@out τ_0_0, @error Error), normal [[NORMAL_BB:bb1]], error [[ERROR_BB:bb2]]
    // CHECK: [[NORMAL_BB]]{{.*}}
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    let f1 = {
      // CHECK: sil shared [transparent] @[[INNER_FUNC_2]]
      // CHECK: bb0([[ARG:%.*]] : $SuperSub):
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
      // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
      // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
      // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]]) : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
      // CHECK:   destroy_value [[ARG_COPY]]
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   destroy_value [[ARG]]
      // CHECK: } // end sil function '[[INNER_FUNC_2]]'
      nil ?? super.boom()
    }
  }

  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1gfT_T_ : $@convention(method) (@guaranteed SuperSub) -> () {
  // CHECK: bb0([[SELF:%.*]] : $SuperSub):
  // CHECK:   [[SELF_COPY:%.*]] = copy_value [[SELF]]
  // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_1:_TFFC8closures8SuperSub1g.*]] : $@convention(thin) (@owned SuperSub) -> ()
  // CHECK:   = apply [[INNER]]([[SELF_COPY]])
  // CHECK: } // end sil function '_TFC8closures8SuperSub1gfT_T_'
  func g() {
    // CHECK: sil shared @[[INNER_FUNC_1]] : $@convention(thin) (@owned SuperSub) -> ()
    // CHECK: bb0([[ARG:%.*]] : $SuperSub):
    // CHECK:   [[TRY_APPLY_FUNC:%.*]] = function_ref @_TFsoi2qqurFzTGSqx_KzT_x_x : $@convention(thin) <τ_0_0> (@in Optional<τ_0_0>, @owned @callee_owned () -> (@out τ_0_0, @error Error)) -> (@out τ_0_0, @error Error)
    // CHECK:   [[INNER:%.*]] = function_ref @[[INNER_FUNC_2:_TFFFC8closures8SuperSub1g.*]] : $@convention(thin) (@owned SuperSub) -> @error Error
    // CHECK:   [[ARG_COPY:%.*]] = copy_value [[ARG]]
    // CHECK:   [[PA:%.*]] = partial_apply [[INNER]]([[ARG_COPY]])
    // CHECK:   [[REABSTRACT_PA:%.*]] = partial_apply {{%.*}}([[PA]]) : $@convention(thin) (@owned @callee_owned () -> @error Error) -> (@out (), @error Error)
    // CHECK:   try_apply [[TRY_APPLY_FUNC]]<()>({{.*}}, {{.*}}, [[REABSTRACT_PA]]) : $@convention(thin) <τ_0_0> (@in Optional<τ_0_0>, @owned @callee_owned () -> (@out τ_0_0, @error Error)) -> (@out τ_0_0, @error Error), normal [[NORMAL_BB:bb1]], error [[ERROR_BB:bb2]]
    // CHECK: [[NORMAL_BB]]{{.*}}
    // CHECK:   destroy_value [[ARG]]
    // CHECK: } // end sil function '[[INNER_FUNC_1]]'
    func g1() {
      // CHECK: sil shared [transparent] @[[INNER_FUNC_2]] : $@convention(thin) (@owned SuperSub) -> @error Error {
      // CHECK: bb0([[ARG:%.*]] : $SuperSub):
      // CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
      // CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
      // CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $SuperSub to $SuperBase
      // CHECK:   [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
      // CHECK:   = apply [[SUPER_METHOD]]([[BORROWED_ARG_COPY_SUPER]])
      // CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
      // CHECK:   destroy_value [[ARG_COPY]]
      // CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
      // CHECK:   destroy_value [[ARG]]
      // CHECK: } // end sil function '[[INNER_FUNC_2]]'
      nil ?? super.boom()
    }
    g1()
  }
}

// CHECK-LABEL: sil hidden @_TFC8closures24UnownedSelfNestedCapture13nestedCapture{{.*}} : $@convention(method) (@guaranteed UnownedSelfNestedCapture) -> ()
// -- We enter with an assumed strong +1.
// CHECK:  bb0([[SELF:%.*]] : $UnownedSelfNestedCapture):
// CHECK:         [[OUTER_SELF_CAPTURE:%.*]] = alloc_box ${ var @sil_unowned UnownedSelfNestedCapture }
// CHECK:         [[PB:%.*]] = project_box [[OUTER_SELF_CAPTURE]]
// -- strong +2
// CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK:         [[UNOWNED_SELF:%.*]] = ref_to_unowned [[SELF_COPY]] :
// -- TODO: A lot of fussy r/r traffic and owned/unowned conversions here.
// -- strong +2, unowned +1
// CHECK:         unowned_retain [[UNOWNED_SELF]]
// CHECK:         store [[UNOWNED_SELF]] to [init] [[PB]]
// SEMANTIC ARC TODO: This destroy_value should probably be /after/ the load from PB on the next line.
// CHECK:         destroy_value [[SELF_COPY]]
// CHECK:         [[UNOWNED_SELF:%.*]] = load [take] [[PB]]
// -- strong +2, unowned +1
// CHECK:         strong_retain_unowned [[UNOWNED_SELF]]
// CHECK:         [[SELF:%.*]] = unowned_to_ref [[UNOWNED_SELF]]
// CHECK:         [[UNOWNED_SELF2:%.*]] = ref_to_unowned [[SELF]]
// -- strong +2, unowned +2
// CHECK:         unowned_retain [[UNOWNED_SELF2]]
// -- strong +1, unowned +2
// CHECK:         destroy_value [[SELF]]
// -- closure takes unowned ownership
// CHECK:         [[OUTER_CLOSURE:%.*]] = partial_apply {{%.*}}([[UNOWNED_SELF2]])
// -- call consumes closure
// -- strong +1, unowned +1
// CHECK:         [[INNER_CLOSURE:%.*]] = apply [[OUTER_CLOSURE]]
// CHECK:         [[CONSUMED_RESULT:%.*]] = apply [[INNER_CLOSURE]]()
// CHECK:         destroy_value [[CONSUMED_RESULT]]
// -- destroy_values unowned self in box
// -- strong +1, unowned +0
// CHECK:         destroy_value [[OUTER_SELF_CAPTURE]]
// -- strong +0, unowned +0
// CHECK: } // end sil function '_TFC8closures24UnownedSelfNestedCapture13nestedCapture{{.*}}'

// -- outer closure
// -- strong +0, unowned +1
// CHECK: sil shared @[[OUTER_CLOSURE_FUN:_TFFC8closures24UnownedSelfNestedCapture13nestedCaptureFT_T_U_FT_FT_S0_]] : $@convention(thin) (@owned @sil_unowned UnownedSelfNestedCapture) -> @owned @callee_owned () -> @owned UnownedSelfNestedCapture {
// CHECK: bb0([[CAPTURED_SELF:%.*]] : $@sil_unowned UnownedSelfNestedCapture):
// -- strong +0, unowned +2
// CHECK:         [[CAPTURED_SELF_COPY:%.*]] = copy_value [[CAPTURED_SELF]] :
// -- closure takes ownership of unowned ref
// CHECK:         [[INNER_CLOSURE:%.*]] = partial_apply {{%.*}}([[CAPTURED_SELF_COPY]])
// -- strong +0, unowned +1 (claimed by closure)
// CHECK:         destroy_value [[CAPTURED_SELF]]
// CHECK:         return [[INNER_CLOSURE]]
// CHECK: } // end sil function '[[OUTER_CLOSURE_FUN]]'

// -- inner closure
// -- strong +0, unowned +1
// CHECK: sil shared @[[INNER_CLOSURE_FUN:_TFFFC8closures24UnownedSelfNestedCapture13nestedCapture.*]] : $@convention(thin) (@owned @sil_unowned UnownedSelfNestedCapture) -> @owned UnownedSelfNestedCapture {
// CHECK: bb0([[CAPTURED_SELF:%.*]] : $@sil_unowned UnownedSelfNestedCapture):
// -- strong +1, unowned +1
// CHECK:         strong_retain_unowned [[CAPTURED_SELF:%.*]] :
// CHECK:         [[SELF:%.*]] = unowned_to_ref [[CAPTURED_SELF]]
// -- strong +1, unowned +0 (claimed by return)
// CHECK:         destroy_value [[CAPTURED_SELF]]
// CHECK:         return [[SELF]]
// CHECK: } // end sil function '[[INNER_CLOSURE_FUN]]'
class UnownedSelfNestedCapture {
  func nestedCapture() {
    {[unowned self] in { self } }()()
  }
}

// Check that capturing 'self' via a 'super' call also captures the generic
// signature if the base class is concrete and the derived class is generic

class ConcreteBase {
  func swim() {}
}

// CHECK-LABEL: sil shared @_TFFC8closures14GenericDerived4swimFT_T_U_FT_T_ : $@convention(thin) <Ocean> (@owned GenericDerived<Ocean>) -> ()
// CHECK: bb0([[ARG:%.*]] : $GenericDerived<Ocean>):
// CHECK:   [[BORROWED_ARG:%.*]] = begin_borrow [[ARG]]
// CHECK:   [[ARG_COPY:%.*]] = copy_value [[BORROWED_ARG]]
// CHECK:   [[ARG_COPY_SUPER:%.*]] = upcast [[ARG_COPY]] : $GenericDerived<Ocean> to $ConcreteBase
// CHECK:   [[METHOD:%.*]] = function_ref @_TFC8closures12ConcreteBase4swimfT_T_
// CHECK:   [[BORROWED_ARG_COPY_SUPER:%.*]] = begin_borrow [[ARG_COPY_SUPER]]
// CHECK:   apply [[METHOD]]([[BORROWED_ARG_COPY_SUPER]]) : $@convention(method) (@guaranteed ConcreteBase) -> ()
// CHECK:   end_borrow [[BORROWED_ARG_COPY_SUPER]] from [[ARG_COPY_SUPER]]
// CHECK:   destroy_value [[ARG_COPY]]
// CHECK:   end_borrow [[BORROWED_ARG]] from [[ARG]]
// CHECK:   destroy_value [[ARG]]
// CHECK: } // end sil function '_TFFC8closures14GenericDerived4swimFT_T_U_FT_T_'

class GenericDerived<Ocean> : ConcreteBase {
  override func swim() {
    withFlotationAid {
      super.swim()
    }
  }

  func withFlotationAid(_ fn: () -> ()) {}
}

// Don't crash on this
func r25993258_helper(_ fn: (inout Int, Int) -> ()) {}
func r25993258() {
  r25993258_helper { _ in () }
}

// rdar://29810997
//
// Using a let from a closure in an init was causing the type-checker
// to produce invalid AST: 'self.fn' was an l-value, but 'self' was already
// loaded to make an r-value.  This was ultimately because CSApply was
// building the member reference correctly in the context of the closure,
// where 'fn' is not settable, but CSGen / CSSimplify was processing it
// in the general DC of the constraint system, i.e. the init, where
// 'fn' *is* settable.
func r29810997_helper(_ fn: (Int) -> Int) -> Int { return fn(0) }
struct r29810997 {
    private let fn: (Int) -> Int
    private var x: Int

    init(function: @escaping (Int) -> Int) {
        fn = function
        x = r29810997_helper { fn($0) }
    }
}

//   DI will turn this into a direct capture of the specific stored property.
// CHECK-LABEL: sil hidden @_TF8closures16r29810997_helperFFSiSiSi : $@convention(thin) (@owned @callee_owned (Int) -> Int) -> Int
