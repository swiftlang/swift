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

// CHECK-LABEL: sil hidden @_TF8closures17read_only_capture
func read_only_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  func cap() -> Int {
    return x
  }

  return cap()
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures17read_only_capture.*]] : $@convention(thin) (@owned @box Int) -> Int
  // CHECK: [[RET:%[0-9]+]] = apply [[CAP]]([[XBOX]])
  // CHECK: release [[XBOX]]
  // CHECK: return [[RET]]
}

// CHECK: sil shared @[[CAP_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $@box Int):
// CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]

// CHECK-LABEL: sil hidden @_TF8closures16write_to_capture
func write_to_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[X2BOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[PB:%.*]] = project_box [[X2BOX]]
  var x2 = x

  func scribble() {
    x2 = zero
  }

  scribble()
  // CHECK: [[SCRIB:%[0-9]+]] = function_ref @[[SCRIB_NAME:_TFF8closures16write_to_capture.*]] : $@convention(thin) (@owned @box Int) -> ()
  // CHECK: apply [[SCRIB]]([[X2BOX]])
  // CHECK: [[RET:%[0-9]+]] = load [[PB]]
  // CHECK: release [[X2BOX]]
  // CHECK: release [[XBOX]]
  // CHECK: return [[RET]]
  return x2
}

// CHECK: sil shared @[[SCRIB_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $@box Int):
// CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK: copy_addr {{%[0-9]+}} to [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return

// CHECK-LABEL: sil hidden @_TF8closures21multiple_closure_refs
func multiple_closure_refs(_ x: Int) -> (() -> Int, () -> Int) {
  var x = x
  func cap() -> Int {
    return x
  }

  return (cap, cap)
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures21multiple_closure_refs.*]] : $@convention(thin) (@owned @box Int) -> Int
  // CHECK: [[CAP_CLOSURE_1:%[0-9]+]] = partial_apply [[CAP]]
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures21multiple_closure_refs.*]] : $@convention(thin) (@owned @box Int) -> Int
  // CHECK: [[CAP_CLOSURE_2:%[0-9]+]] = partial_apply [[CAP]]
  // CHECK: [[RET:%[0-9]+]] = tuple ([[CAP_CLOSURE_1]] : {{.*}}, [[CAP_CLOSURE_2]] : {{.*}})
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden @_TF8closures18capture_local_func
func capture_local_func(_ x: Int) -> () -> () -> Int {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  func aleph() -> Int { return x }

  func beth() -> () -> Int { return aleph }
  // CHECK: [[BETH_REF:%[0-9]+]] = function_ref @[[BETH_NAME:_TFF8closures18capture_local_funcFSiFT_FT_SiL_4bethfT_FT_Si]] : $@convention(thin) (@owned @box Int) -> @owned @callee_owned () -> Int
  // CHECK: [[BETH_CLOSURE:%[0-9]+]] = partial_apply [[BETH_REF]]([[XBOX]])

  return beth
  // CHECK: release [[XBOX]]
  // CHECK: return [[BETH_CLOSURE]]
}
// CHECK: sil shared @[[ALEPH_NAME:_TFF8closures18capture_local_funcFSiFT_FT_SiL_5alephfT_Si]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $@box Int):

// CHECK: sil shared @[[BETH_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $@box Int):
// CHECK: [[ALEPH_REF:%[0-9]+]] = function_ref @[[ALEPH_NAME]] : $@convention(thin) (@owned @box Int) -> Int
// CHECK: [[ALEPH_CLOSURE:%[0-9]+]] = partial_apply [[ALEPH_REF]]([[XBOX]])
// CHECK: return [[ALEPH_CLOSURE]]

// CHECK-LABEL: sil hidden @_TF8closures22anon_read_only_capture
func anon_read_only_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]

  return ({ x })()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures22anon_read_only_capture.*]] : $@convention(thin) (@inout_aliasable Int) -> Int
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON]]([[PB]])
  // -- cleanup
  // CHECK: release [[XBOX]]
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: return [[X]]

// CHECK-LABEL: sil hidden @_TF8closures21small_closure_capture
func small_closure_capture(_ x: Int) -> Int {
  var x = x
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[PB:%.*]] = project_box [[XBOX]]

  return { x }()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures21small_closure_capture.*]] : $@convention(thin) (@inout_aliasable Int) -> Int
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON]]([[PB]])
  // -- cleanup
  // CHECK: release [[XBOX]]
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: return [[X]]


// CHECK-LABEL: sil hidden @_TF8closures35small_closure_capture_with_argument
func small_closure_capture_with_argument(_ x: Int) -> (_ y: Int) -> Int {
  var x = x
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  return { x + $0 }
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures35small_closure_capture_with_argument.*]] : $@convention(thin) (Int, @owned @box Int) -> Int
  // CHECK: retain [[XBOX]]
  // CHECK: [[ANON_CLOSURE_APP:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]])
  // -- return
  // CHECK: release [[XBOX]]
  // CHECK: return [[ANON_CLOSURE_APP]]
}
// CHECK: sil shared @[[CLOSURE_NAME]] : $@convention(thin) (Int, @owned @box Int) -> Int
// CHECK: bb0([[DOLLAR0:%[0-9]+]] : $Int, [[XBOX:%[0-9]+]] : $@box Int):
// CHECK: [[XADDR:%[0-9]+]] = project_box [[XBOX]]
// CHECK: [[PLUS:%[0-9]+]] = function_ref @_TFsoi1pFTSiSi_Si{{.*}}
// CHECK: [[LHS:%[0-9]+]] = load [[XADDR]]
// CHECK: [[RET:%[0-9]+]] = apply [[PLUS]]([[LHS]], [[DOLLAR0]])
// CHECK: release [[XBOX]]
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
  // CHECK:   [[XADDR:%[0-9]+]] = alloc_box $Int
  // CHECK:   [[PB:%.*]] = project_box [[XADDR]]
  // CHECK:   store [[XARG]] to [[PB]]

  var y = zero
  // CHECK:   [[YADDR:%[0-9]+]] = alloc_box $Int
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
// CHECK-LABEL: sil shared @_TFF8closures20generateWithConstant
// CHECK:    bb0([[T0:%.*]] : $SomeSpecificClass):
// CHECK-NEXT: debug_value %0 : $SomeSpecificClass, let, name "x", argno 1
// CHECK-NEXT: [[T1:%.*]] = upcast [[T0]] : $SomeSpecificClass to $SomeClass
// CHECK-NEXT: return [[T1]]


// Check the annoying case of capturing 'self' in a derived class 'init'
// method. We allocate a mutable box to deal with 'self' potentially being
// rebound by super.init, but 'self' is formally immutable and so is captured
// by value. <rdar://problem/15599464>
class Base {}

class SelfCapturedInInit : Base {
  var foo : () -> SelfCapturedInInit

  // CHECK-LABEL: sil hidden @_TFC8closures18SelfCapturedInInitc{{.*}} : $@convention(method) (@owned SelfCapturedInInit) -> @owned SelfCapturedInInit {
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         strong_retain [[VAL]] : $SelfCapturedInInit
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
// CHECK-LABEL: sil shared @_TFF8closures18closeOverLetLValueFT_T_U_FT_Si
// CHECK: bb0(%0 : $ClassWithIntProperty):
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $ClassWithIntProperty, let, name "a", argno 1
// CHECK-NEXT: store %0 to [[TMP]] : $*ClassWithIntProperty
// CHECK-NEXT: {{.*}} = load [[TMP]] : $*ClassWithIntProperty
// CHECK-NEXT: {{.*}} = ref_element_addr {{.*}} : $ClassWithIntProperty, #ClassWithIntProperty.x
// CHECK-NEXT: {{.*}} = load {{.*}} : $*Int
// CHECK-NEXT: destroy_addr [[TMP]] : $*ClassWithIntProperty
// CHECK-NEXT: dealloc_stack %1 : $*ClassWithIntProperty
// CHECK-NEXT:  return



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
// CHECK-NEXT: %1 = alloc_box $StructWithMutatingMethod, var, name "self", argno 1
// CHECK-NEXT: %2 = project_box %1
// CHECK-NEXT: copy_addr %0 to [initialization] %2 : $*StructWithMutatingMethod
// CHECK: [[CLOSURE:%[0-9]+]] = function_ref @_TFFV8closures24StructWithMutatingMethod14mutatingMethod{{.*}} : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int
// CHECK: partial_apply [[CLOSURE]](%2) : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int

// Check that the closure body only takes the pointer.
// CHECK-LABEL: sil shared @_TFFV8closures24StructWithMutatingMethod14mutatingMethod{{.*}} : $@convention(thin) (@inout_aliasable StructWithMutatingMethod) -> Int {
// CHECK:       bb0(%0 : $*StructWithMutatingMethod):

class SuperBase {
  func boom() {}
}
class SuperSub : SuperBase {
  override func boom() {}
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1a
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1a
  // CHECK: = apply [[INNER]](%0)
  // CHECK: return
  func a() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1a
    // CHECK: [[CLASS_METHOD:%.*]] = class_method %0 : $SuperSub, #SuperSub.boom!1
    // CHECK: = apply [[CLASS_METHOD]](%0)
    // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
    // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
    // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
    // CHECK: return
    func a1() {
      self.boom()
      super.boom()
    }
    a1()
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1b
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1b
  // CHECK: = apply [[INNER]](%0)
  // CHECK: return
  func b() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1b
    // CHECK: [[INNER:%.*]] = function_ref @_TFFFC8closures8SuperSub1b
    // CHECK: = apply [[INNER]](%0)
    // CHECK: return
    func b1() {
      // CHECK-LABEL: sil shared @_TFFFC8closures8SuperSub1b
      // CHECK: [[CLASS_METHOD:%.*]] = class_method %0 : $SuperSub, #SuperSub.boom!1
      // CHECK: = apply [[CLASS_METHOD]](%0)
      // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
      // CHECK: [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
      // CHECK: return
      func b2() {
        self.boom()
        super.boom()
      }
      b2()
    }
    b1()
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1c
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1c
  // CHECK: = partial_apply [[INNER]](%0)
  // CHECK: return
  func c() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1c
    // CHECK: [[CLASS_METHOD:%.*]] = class_method %0 : $SuperSub, #SuperSub.boom!1
    // CHECK: = apply [[CLASS_METHOD]](%0)
    // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
    // CHECK: [[SUPER_METHOD:%[0-9]+]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
    // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
    // CHECK: return
    let c1 = { () -> Void in
      self.boom()
      super.boom()
    }
    c1()
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1d
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1d
  // CHECK: = partial_apply [[INNER]](%0)
  // CHECK: return
  func d() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1d
    // CHECK: [[INNER:%.*]] = function_ref @_TFFFC8closures8SuperSub1d
    // CHECK: = apply [[INNER]](%0)
    // CHECK: return
    let d1 = { () -> Void in
      // CHECK-LABEL: sil shared @_TFFFC8closures8SuperSub1d
      // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
      // CHECK: [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
      // CHECK: return
      func d2() {
        super.boom()
      }
      d2()
    }
    d1()
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1e
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1e
  // CHECK: = apply [[INNER]](%0)
  // CHECK: return
  func e() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1e
    // CHECK: [[INNER:%.*]] = function_ref @_TFFFC8closures8SuperSub1e
    // CHECK: = partial_apply [[INNER]](%0)
    // CHECK: return
    func e1() {
      // CHECK-LABEL: sil shared @_TFFFC8closures8SuperSub1e
      // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
      // CHECK: [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
      // CHECK: return
      let e2 = {
        super.boom()
      }
      e2()
    }
    e1()
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1f
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1f
  // CHECK: = partial_apply [[INNER]](%0)
  // CHECK: return
  func f() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1f
    // CHECK: [[INNER:%.*]] = function_ref @_TFFFC8closures8SuperSub1f
    // CHECK: = partial_apply [[INNER]](%0)
    // CHECK: return
    let f1 = {
      // CHECK-LABEL: sil shared [transparent] @_TFFFC8closures8SuperSub1f
      // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
      // CHECK: [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
      // CHECK: return
      nil ?? super.boom()
    }
  }
  
  // CHECK-LABEL: sil hidden @_TFC8closures8SuperSub1g
  // CHECK: [[INNER:%.*]] = function_ref @_TFFC8closures8SuperSub1g
  // CHECK: = apply [[INNER]](%0)
  // CHECK: return
  func g() {
    // CHECK-LABEL: sil shared @_TFFC8closures8SuperSub1g
    // CHECK: [[INNER:%.*]] = function_ref @_TFFFC8closures8SuperSub1g
    // CHECK: = partial_apply [[INNER]](%0)
    // CHECK: return
    func g1() {
      // CHECK-LABEL: sil shared [transparent] @_TFFFC8closures8SuperSub1g
      // CHECK: [[SUPER:%.*]] = upcast %0 : $SuperSub to $SuperBase
      // CHECK: [[SUPER_METHOD:%.*]] = function_ref @_TFC8closures9SuperBase4boomfT_T_ : $@convention(method) (@guaranteed SuperBase) -> ()
      // CHECK: = apply [[SUPER_METHOD]]([[SUPER]])
      // CHECK: return
      nil ?? super.boom()
    }
    g1()
  }
}

// CHECK-LABEL: sil hidden @_TFC8closures24UnownedSelfNestedCapture13nestedCapture{{.*}} : $@convention(method) (@guaranteed UnownedSelfNestedCapture) -> ()
// CHECK:         [[OUTER_SELF_CAPTURE:%.*]] = alloc_box $@sil_unowned UnownedSelfNestedCapture
// CHECK:         [[PB:%.*]] = project_box [[OUTER_SELF_CAPTURE]]
// CHECK:         [[UNOWNED_SELF:%.*]] = ref_to_unowned [[SELF_PARAM:%.*]] :
// -- TODO: A lot of fussy r/r traffic and owned/unowned conversions here.
// -- strong +1, unowned +1
// CHECK:         unowned_retain [[UNOWNED_SELF]]
// CHECK:         store [[UNOWNED_SELF]] to [[PB]]
// CHECK:         [[UNOWNED_SELF:%.*]] = load [[PB]]
// -- strong +2, unowned +1
// CHECK:         strong_retain_unowned [[UNOWNED_SELF]]
// CHECK:         [[SELF:%.*]] = unowned_to_ref [[UNOWNED_SELF]]
// CHECK:         [[UNOWNED_SELF2:%.*]] = ref_to_unowned [[SELF]]
// -- strong +2, unowned +2
// CHECK:         unowned_retain [[UNOWNED_SELF2]]
// -- strong +1, unowned +2
// CHECK:         strong_release [[SELF]]
// -- closure takes unowned ownership
// CHECK:         [[OUTER_CLOSURE:%.*]] = partial_apply {{%.*}}([[UNOWNED_SELF2]])
// -- call consumes closure
// -- strong +1, unowned +1
// CHECK:         [[INNER_CLOSURE:%.*]] = apply [[OUTER_CLOSURE]]
// CHECK:         [[CONSUMED_RESULT:%.*]] = apply [[INNER_CLOSURE]]()
// CHECK:         strong_release [[CONSUMED_RESULT]]
// -- releases unowned self in box
// -- strong +1, unowned +0
// CHECK:         strong_release [[OUTER_SELF_CAPTURE]]
// -- strong +0, unowned +0
// CHECK:         return

// -- outer closure
// -- strong +0, unowned +1
// CHECK-LABEL: sil shared @_TFFC8closures24UnownedSelfNestedCapture13nestedCapture
// -- strong +0, unowned +2
// CHECK:         unowned_retain [[CAPTURED_SELF:%.*]] :
// -- closure takes ownership of unowned ref
// CHECK:         [[INNER_CLOSURE:%.*]] = partial_apply {{%.*}}([[CAPTURED_SELF]])
// -- strong +0, unowned +1 (claimed by closure)
// CHECK:         unowned_release [[CAPTURED_SELF]]
// CHECK:         return [[INNER_CLOSURE]]

// -- inner closure
// -- strong +0, unowned +1
// CHECK-LABEL: sil shared @_TFFFC8closures24UnownedSelfNestedCapture13nestedCapture
// -- strong +1, unowned +1
// CHECK:         strong_retain_unowned [[CAPTURED_SELF:%.*]] :
// CHECK:         [[SELF:%.*]] = unowned_to_ref [[CAPTURED_SELF]]
// -- strong +1, unowned +0 (claimed by return)
// CHECK:         unowned_release [[CAPTURED_SELF]]
// CHECK:         return [[SELF]]
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
// CHECK:         [[SUPER:%.*]] = upcast %0 : $GenericDerived<Ocean> to $ConcreteBase
// CHECK:         [[METHOD:%.*]] = function_ref @_TFC8closures12ConcreteBase4swimfT_T_
// CHECK:         apply [[METHOD]]([[SUPER]]) : $@convention(method) (@guaranteed ConcreteBase) -> ()

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
