// RUN: %target-swift-frontend -parse-stdlib -parse-as-library -emit-silgen %s | FileCheck %s

import Swift

var zero = 0

// <rdar://problem/15921334>
// CHECK-LABEL: sil hidden @_TF8closures46return_local_generic_function_without_capturesU___FT_FQ_Q0_ : $@thin <A, R> () -> @owned @callee_owned (@out R, @in A) -> () {
func return_local_generic_function_without_captures<A, R>() -> A -> R {
  func f(_: A) -> R {
    Builtin.int_trap()
  }
  // CHECK:  [[FN:%.*]] = function_ref @_TFF8closures46return_local_generic_function_without_capturesU___FT_FQ_Q0_L_1fFQ_Q0_ : $@thin <τ_0_0, τ_0_1> (@out τ_0_1, @in τ_0_0) -> ()
  // CHECK:  [[FN_WITH_GENERIC_PARAMS:%.*]] = partial_apply [[FN]]<A, R>() : $@thin <τ_0_0, τ_0_1> (@out τ_0_1, @in τ_0_0) -> ()
  // CHECK:  return [[FN_WITH_GENERIC_PARAMS]] : $@callee_owned (@out R, @in A) -> ()
  return f
}

// CHECK-LABEL: sil hidden @_TF8closures17read_only_capture
func read_only_capture(var x: Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  func cap() -> Int {
    return x
  }
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures17read_only_capture.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> Int
  // CHECK: [[CAP_CLOSURE:%[0-9]+]] = partial_apply [[CAP]]([[XBOX]]#0, [[XBOX]]#1)

  return cap()
  // CHECK: retain [[CAP_CLOSURE]]
  // CHECK: [[RET:%[0-9]+]] = apply [[CAP_CLOSURE]]()
  // CHECK: release [[CAP_CLOSURE]]
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}

// CHECK: sil shared @[[CAP_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]

// CHECK-LABEL: sil hidden @_TF8closures16write_to_capture
func write_to_capture(var x: Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int
  // CHECK: [[X2BOX:%[0-9]+]] = alloc_box $Int
  var x2 = x

  func scribble() {
    x2 = zero
  }
  // -- FIXME: [[SCRIB:%[0-9]+]] = function_ref @_TFL8closures16write_to_capture
  // CHECK: [[SCRIB:%[0-9]+]] = function_ref @[[SCRIB_NAME:_TFF8closures16write_to_capture.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> ()
  // CHECK: [[SCRIB_CLOSURE:%[0-9]+]] = partial_apply [[SCRIB]]([[X2BOX]]#0, [[X2BOX]]#1)

  scribble()
  // CHECK: retain [[SCRIB_CLOSURE]]
  // CHECK: apply [[SCRIB_CLOSURE]]()
  // CHECK: [[RET:%[0-9]+]] = load [[X2BOX]]#1
  // CHECK: release [[SCRIB_CLOSURE]]
  // CHECK: release [[X2BOX]]#0
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
  return x2
}

// CHECK: sil shared @[[SCRIB_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):
// CHECK: copy_addr {{%[0-9]+}} to [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return

// CHECK-LABEL: sil hidden @_TF8closures21multiple_closure_refs
func multiple_closure_refs(var x: Int) -> (() -> Int, () -> Int) {
  func cap() -> Int {
    return x
  }
  // CHECK: [[CAP:%[0-9]+]] = function_ref @[[CAP_NAME:_TFF8closures21multiple_closure_refs.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> Int
  // CHECK: [[CAP_CLOSURE:%[0-9]+]] = partial_apply [[CAP]]

  return (cap, cap)
  // CHECK: retain [[CAP_CLOSURE]]
  // CHECK: [[RET:%[0-9]+]] = tuple ([[CAP_CLOSURE]] : {{.*}}, [[CAP_CLOSURE]] : {{.*}})
  // CHECK: return [[RET]]
}

/* TODO: Full support for references between local functions
// C/HECK-LABEL: sil hidden @_TF8closures18capture_local_func
func capture_local_func(var x: Int) -> () -> () -> Int {
  // C/HECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  func aleph() -> Int { return x }
  // C/HECK: [[ALEPH_REF:%[0-9]+]] = function_ref @[[ALEPH_NAME:_TFF8closures18capture_local_func.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> Int
  // C/HECK: [[ALEPH_CLOSURE:%[0-9]+]] = partial_apply [[ALEPH_REF]]([[XBOX]]#0, [[XBOX]]#1)

  func beth() -> () -> Int { return aleph }
  // C/HECK: [[BETH_REF:%[0-9]+]] = function_ref @[[BETH_NAME:_TFF8closures18capture_local_func.*]] : $@thin (@owned @callee_owned () -> Int) -> @owned @callee_owned () -> Int
  // C/HECK: [[BETH_CLOSURE:%[0-9]+]] = partial_apply [[BETH_REF]]([[ALEPH_CLOSURE]])

  return beth
  // C/HECK: release [[ALEPH_CLOSURE]]
  // C/HECK: release [[XBOX]]#0
  // C/HECK: return [[BETH_CLOSURE]]
}
// C/HECK: sil shared @[[ALEPH_NAME]]
// C/HECK: bb0([[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):

// C/HECK: sil shared @[[BETH_NAME]]
// C/HECK: bb0([[ALEPH:%[0-9]+]] : $@callee_owned () -> Int):
// C/HECK: return [[ALEPH]]
   */

// CHECK-LABEL: sil hidden @_TF8closures22anon_read_only_capture
func anon_read_only_capture(var x: Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  return ({ x })()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures22anon_read_only_capture.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> Int
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON_CLOSURE]]()
  // -- cleanup
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]

// CHECK-LABEL: sil hidden @_TF8closures21small_closure_capture
func small_closure_capture(var x: Int) -> Int {
  // CHECK: bb0([[X:%[0-9]+]] : $Int):
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  return { x }()
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures21small_closure_capture.*]] : $@thin (@owned Builtin.NativeObject, @inout Int) -> Int
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // -- apply expression
  // CHECK: [[RET:%[0-9]+]] = apply [[ANON_CLOSURE]]()
  // -- cleanup
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[RET]]
}
// CHECK: sil shared @[[CLOSURE_NAME]]
// CHECK: bb0([[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[X:%[0-9]+]] = load [[XADDR]]
// CHECK: release [[XBOX]]
// CHECK: return [[X]]


// CHECK-LABEL: sil hidden @_TF8closures35small_closure_capture_with_argument
func small_closure_capture_with_argument(var x: Int) -> (y: Int) -> Int {
  // CHECK: [[XBOX:%[0-9]+]] = alloc_box $Int

  return { x + $0 }
  // -- func expression
  // CHECK: [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures35small_closure_capture_with_argument.*]] : $@thin (Int, @owned Builtin.NativeObject, @inout Int) -> Int
  // CHECK: retain [[XBOX]]#0
  // CHECK: [[ANON_CLOSURE_APP:%[0-9]+]] = partial_apply [[ANON]]([[XBOX]]#0, [[XBOX]]#1)
  // -- return
  // CHECK: release [[XBOX]]#0
  // CHECK: return [[ANON_CLOSURE]]
}
// CHECK: sil shared @[[CLOSURE_NAME]] : $@thin (Int, @owned Builtin.NativeObject, @inout Int) -> Int
// CHECK: bb0([[DOLLAR0:%[0-9]+]] : $Int, [[XBOX:%[0-9]+]] : $Builtin.NativeObject, [[XADDR:%[0-9]+]] : $*Int):
// CHECK: [[PLUS:%[0-9]+]] = function_ref @_TFSsoi1pFTSiSi_Si{{.*}}
// CHECK: [[LHS:%[0-9]+]] = load [[XADDR]]
// CHECK: [[RET:%[0-9]+]] = apply [transparent] [[PLUS]]([[LHS]], [[DOLLAR0]])
// CHECK: release [[XBOX]]
// CHECK: return [[RET]]

// CHECK-LABEL: sil hidden @_TF8closures24small_closure_no_capture
func small_closure_no_capture() -> (y: Int) -> Int {
  // CHECK:   [[ANON:%[0-9]+]] = function_ref @[[CLOSURE_NAME:_TFF8closures24small_closure_no_captureFT_FT1ySi_SiU_FSiSi]] : $@thin (Int) -> Int
  // CHECK:   [[ANON_THICK:%[0-9]+]] = thin_to_thick_function [[ANON]] : ${{.*}} to $@callee_owned (Int) -> Int
  // CHECK:   return [[ANON_THICK]]
  return { $0 }
}
// CHECK: sil shared @[[CLOSURE_NAME]] : $@thin (Int) -> Int
// CHECK: bb0([[YARG:%[0-9]+]] : $Int):

// CHECK-LABEL: sil hidden @_TF8closures17uncaptured_locals{{.*}} :
func uncaptured_locals(var x: Int) -> (Int, Int) {
  // -- locals without captures are stack-allocated
  // CHECK: bb0([[XARG:%[0-9]+]] : $Int):
  // CHECK:   [[XADDR:%[0-9]+]] = alloc_box $Int
  // CHECK:   store [[XARG]] to [[XADDR]]

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
    // CHECK: [[C1REF:%[0-9]+]] = function_ref @_TFFC8closures16SomeGenericClassd{{.*}} : $@thin <τ_0_0> (@owned Builtin.NativeObject, @inout Int) -> Int
    // CHECK: [[C1SPEC:%[0-9]+]] = partial_apply [[C1REF]]<T>([[IBOX:%[0-9]+]]#0, [[IBOX]]#1) : $@thin <τ_0_0> (@owned Builtin.NativeObject, @inout Int) -> Int
    // CHECK: apply [[C1SPEC]]() : $@callee_owned () -> Int
    var x = { i + zero } ()

    // CHECK: [[C2REF:%[0-9]+]] = function_ref @_TFFC8closures16SomeGenericClassdU0_FT{{.*}} : $@thin <τ_0_0> () -> Int
    // CHECK: [[C2SPEC:%[0-9]+]] = partial_apply [[C2REF]]<T>() : $@thin <τ_0_0> () -> Int
    // CHECK: apply [[C2SPEC]]() : $@callee_owned () -> Int
    var y = { zero } ()
  }

  // CHECK-LABEL: sil shared @_TFFC8closures16SomeGenericClassdU{{.*}} : $@thin <T> (@owned Builtin.NativeObject, @inout Int) -> Int

  // CHECK-LABEL: sil shared @_TFFC8closures16SomeGenericClassdU0_FT{{.*}} : $@thin <T> () -> Int
}

// This is basically testing that the constraint system ranking
// function conversions as worse than others, and therefore performs
// the conversion within closures when possible.
class SomeSpecificClass : SomeClass {}
func takesSomeClassGenerator(fn : () -> SomeClass) {}
func generateWithConstant(x : SomeSpecificClass) {
  takesSomeClassGenerator({ x })
}
// CHECK-LABEL: sil shared @_TFF8closures20generateWithConstant
// CHECK:    bb0([[T0:%.*]] : $SomeSpecificClass):
// CHECK-NEXT: [[T1:%.*]] = upcast [[T0]] : $SomeSpecificClass to $SomeClass
// CHECK-NEXT: return [[T1]]


// Check the annoying case of capturing 'self' in a derived class 'init'
// method. We allocate a mutable box to deal with 'self' potentially being
// rebound by super.init, but 'self' is formally immutable and so is captured
// by value. <rdar://problem/15599464>
class Base {}

class SelfCapturedInInit : Base {
  var foo : () -> SelfCapturedInInit

  // CHECK-LABEL: sil hidden @_TFC8closures18SelfCapturedInInitcfMS0_FT_S0_ : $@cc(method) @thin (@owned SelfCapturedInInit) -> @owned SelfCapturedInInit {
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         [[VAL:%.*]] = load {{%.*}} : $*SelfCapturedInInit
  // CHECK:         strong_retain [[VAL]] : $SelfCapturedInInit
  // CHECK:         partial_apply {{%.*}}([[VAL]]) : $@thin (@owned SelfCapturedInInit) -> @owned SelfCapturedInInit
  override init() {
    super.init()
    foo = { self }
  }
}

func takeClosure(fn: () -> Int) -> Int { return fn() }

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
// CHECK-NEXT: bb0(%0 : $ClassWithIntProperty):
// CHECK-NEXT: [[TMP:%.*]] = alloc_stack $ClassWithIntProperty  // let a
// CHECK-NEXT: store %0 to [[TMP]]#1 : $*ClassWithIntProperty
// CHECK-NEXT: {{.*}} = load [[TMP]]#1 : $*ClassWithIntProperty
// CHECK-NEXT: {{.*}} = ref_element_addr {{.*}} : $ClassWithIntProperty, #ClassWithIntProperty.x
// CHECK-NEXT: {{.*}} = load {{.*}} : $*Int
// CHECK-NEXT: destroy_addr [[TMP]]#1 : $*ClassWithIntProperty
// CHECK-NEXT: dealloc_stack %1#0 : $*@local_storage ClassWithIntProperty
// CHECK-NEXT:  return



// Use an external function so inout deshadowing cannot see its body.
@asmname("takesNoEscapeClosure")
func takesNoEscapeClosure(@noescape fn : () -> Int)

struct StructWithMutatingMethod {
  var x = 42

  mutating func mutatingMethod() {
    // This should not capture the refcount of the self shadow.
    takesNoEscapeClosure { x = 42; return x }
  }
}

// Check that the address of self is passed in, but not the refcount pointer.

// CHECK-LABEL: sil hidden @_TFV8closures24StructWithMutatingMethod14mutatingMethodfRS0_FT_T_
// CHECK-NEXT: bb0(%0 : $*StructWithMutatingMethod):
// CHECK-NEXT: %1 = alloc_box $StructWithMutatingMethod  // var self // users: %2, %5, %7, %8
// CHECK-NEXT: copy_addr %0 to [initialization] %1#1 : $*StructWithMutatingMethod // id: %2
// CHECK: [[CLOSURE:%[0-9]+]] = function_ref @_TFFV8closures24StructWithMutatingMethod14mutatingMethodFRS0_FT_T_U_FT_Si : $@noescape @thin (@inout StructWithMutatingMethod) -> Int
// CHECK: partial_apply [[CLOSURE]](%1#1) : $@noescape @thin (@inout StructWithMutatingMethod) -> Int

// Check that the closure body only takes the pointer.
// CHECK-LABEL: sil shared @_TFFV8closures24StructWithMutatingMethod14mutatingMethodFRS0_FT_T_U_FT_Si : $@noescape @thin (@inout StructWithMutatingMethod) -> Int {
// CHECK-NEXT:  bb0(%0 : $*StructWithMutatingMethod):
