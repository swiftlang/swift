// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-as-library -emit-silgen -disable-objc-attr-requires-foundation-module %s | %FileCheck %s

var zero: Int = 0

func use(_: Int) {}
func use(_: Double) {}
func getInt() -> Int { return zero }

// CHECK-LABEL: sil hidden  @{{.*}}physical_tuple_lvalue
// CHECK: bb0(%0 : $Int):
func physical_tuple_lvalue(_ c: Int) {
  var x : (Int, Int)
  // CHECK: [[BOX:%[0-9]+]] = alloc_box ${ var (Int, Int) }
  // CHECK: [[XADDR1:%.*]] = project_box [[BOX]]
  // CHECK: [[XADDR:%[0-9]+]] = mark_uninitialized [var] [[XADDR1]]
  x.1 = c
  // CHECK: [[X_1:%[0-9]+]] = tuple_element_addr [[XADDR]] : {{.*}}, 1
  // CHECK: assign %0 to [[X_1]]
}

func tuple_rvalue() -> (Int, Int) {}

// CHECK-LABEL: sil hidden  @{{.*}}physical_tuple_rvalue
func physical_tuple_rvalue() -> Int {
  return tuple_rvalue().1
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF10properties12tuple_rvalue
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[FUNC]]()
  // CHECK: [[RET:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden  @_TF10properties16tuple_assignment
func tuple_assignment(_ a: inout Int, b: inout Int) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int, [[B_ADDR:%[0-9]+]] : $*Int):
  // CHECK: [[B:%[0-9]+]] = load [trivial] [[B_ADDR]]
  // CHECK: [[A:%[0-9]+]] = load [trivial] [[A_ADDR]]
  // CHECK: assign [[B]] to [[A_ADDR]]
  // CHECK: assign [[A]] to [[B_ADDR]]
  (a, b) = (b, a)
}

// CHECK-LABEL: sil hidden  @_TF10properties18tuple_assignment_2
func tuple_assignment_2(_ a: inout Int, b: inout Int, xy: (Int, Int)) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int, [[B_ADDR:%[0-9]+]] : $*Int, [[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Int):
  (a, b) = xy
  // CHECK: [[XY2:%[0-9]+]] = tuple ([[X]] : $Int, [[Y]] : $Int)
  // CHECK: [[X:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 0
  // CHECK: [[Y:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 1
  // CHECK: assign [[X]] to [[A_ADDR]]
  // CHECK: assign [[Y]] to [[B_ADDR]]
}

class Ref {
  var x, y : Int
  var ref : Ref

  var z: Int { get {} set {} }

  var val_prop: Val { get {} set {} }

  subscript(i: Int) -> Float { get {} set {} }

  init(i: Int) {
    x = i
    y = i
    ref = self
  }
}

class RefSubclass : Ref {
  var w : Int

  override init (i: Int) {
    w = i
    super.init(i: i)
  }
}

struct Val {
  var x, y : Int
  var ref : Ref

  var z: Int { get {} set {} }

  var z_tuple: (Int, Int) { get {} set {} }

  subscript(i: Int) -> Float { get {} set {} }
}

// CHECK-LABEL: sil hidden  @_TF10properties22physical_struct_lvalue
func physical_struct_lvalue(_ c: Int) {
  var v : Val
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box ${ var Val }
  v.y = c
  // CHECK: assign %0 to [[X_1]]
}

// CHECK-LABEL: sil hidden  @_TF10properties21physical_class_lvalue
 func physical_class_lvalue(_ r: Ref, a: Int) {
    r.y = a

   // CHECK: [[FN:%[0-9]+]] = class_method %0 : $Ref, #Ref.y!setter.1
   // CHECK: apply [[FN]](%1, %0) : $@convention(method) (Int, @guaranteed Ref) -> ()
   // CHECK: destroy_value %0 : $Ref
  }


// CHECK-LABEL: sil hidden  @_TF10properties24physical_subclass_lvalue
func physical_subclass_lvalue(_ r: RefSubclass, a: Int) {
  // CHECK: bb0([[ARG1:%.*]] : $RefSubclass, [[ARG2:%.*]] : $Int):
  r.y = a
  // CHECK: [[ARG1_COPY:%.*]] = copy_value [[ARG1]] : $RefSubclass
  // CHECK: [[R_SUP:%[0-9]+]] = upcast [[ARG1_COPY]] : $RefSubclass to $Ref
  // CHECK: [[FN:%[0-9]+]] = class_method [[R_SUP]] : $Ref, #Ref.y!setter.1 : (Ref) -> (Int) -> () , $@convention(method) (Int, @guaranteed Ref) -> ()
  // CHECK: apply [[FN]]([[ARG2]], [[R_SUP]]) :
  // CHECK: destroy_value [[R_SUP]]
  r.w = a

  // CHECK: [[FN:%[0-9]+]] = class_method [[ARG1]] : $RefSubclass, #RefSubclass.w!setter.1
  // CHECK: apply [[FN]](%1, [[ARG1]]) : $@convention(method) (Int, @guaranteed RefSubclass) -> ()
  // CHECK: destroy_value [[ARG1]]
}
  


func struct_rvalue() -> Val {}

// CHECK-LABEL: sil hidden  @_TF10properties22physical_struct_rvalue
func physical_struct_rvalue() -> Int {
  return struct_rvalue().y
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF10properties13struct_rvalueFT_VS_3Val
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]()
  // CHECK: [[RET:%[0-9]+]] = struct_extract [[STRUCT]] : $Val, #Val.y
  // CHECK: return [[RET]]
}

func class_rvalue() -> Ref {}

// CHECK-LABEL: sil hidden  @_TF10properties21physical_class_rvalue
func physical_class_rvalue() -> Int {
  return class_rvalue().y
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_TF10properties12class_rvalueFT_CS_3Ref
  // CHECK: [[CLASS:%[0-9]+]] = apply [[FUNC]]()

  // CHECK: [[FN:%[0-9]+]] = class_method [[CLASS]] : $Ref, #Ref.y!getter.1
  // CHECK: [[RET:%[0-9]+]] = apply [[FN]]([[CLASS]])
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden  @_TF10properties18logical_struct_get
func logical_struct_get() -> Int {
  return struct_rvalue().z
  // CHECK: [[GET_RVAL:%[0-9]+]] = function_ref @_TF10properties13struct_rvalue
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[GET_RVAL]]()
  // CHECK: [[GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg1z
  // CHECK: [[VALUE:%[0-9]+]] = apply [[GET_METHOD]]([[STRUCT]])
  // CHECK: return [[VALUE]]
}

// CHECK-LABEL: sil hidden  @_TF10properties18logical_struct_set
func logical_struct_set(_ value: inout Val, z: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z:%[0-9]+]] : $Int):
  value.z = z
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals1z
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden  @_TF10properties27logical_struct_in_tuple_set
func logical_struct_in_tuple_set(_ value: inout (Int, Val), z: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*(Int, Val), [[Z:%[0-9]+]] : $Int):
  value.1.z = z
  // CHECK: [[VAL_1:%[0-9]+]] = tuple_element_addr [[VAL]] : {{.*}}, 1
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals1z
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL_1]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden  @_TF10properties29logical_struct_in_reftype_set
func logical_struct_in_reftype_set(_ value: inout Val, z1: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z1:%[0-9]+]] : $Int):
  value.ref.val_prop.z_tuple.1 = z1
  // -- val.ref
  // CHECK: [[VAL_REF_ADDR:%[0-9]+]] = struct_element_addr [[VAL]] : $*Val, #Val.ref
  // CHECK: [[VAL_REF:%[0-9]+]] = load [copy] [[VAL_REF_ADDR]]
  // -- getters and setters
  // -- val.ref.val_prop
  // CHECK: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: [[VAL_REF_VAL_PROP_TEMP:%.*]] = alloc_stack $Val
  // CHECK: [[T0:%.*]] = address_to_pointer [[VAL_REF_VAL_PROP_TEMP]] : $*Val to $Builtin.RawPointer
  // CHECK: [[MAT_VAL_PROP_METHOD:%[0-9]+]] = class_method {{.*}} : $Ref, #Ref.val_prop!materializeForSet.1 : (Ref) -> (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer) -> (Builtin.RawPointer, Builtin.RawPointer?)
  // CHECK: [[MAT_RESULT:%[0-9]+]] = apply [[MAT_VAL_PROP_METHOD]]([[T0]], [[STORAGE]], [[VAL_REF]])
  // CHECK: [[T0:%.*]] = tuple_extract [[MAT_RESULT]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>), 0
  // CHECK: [[OPT_CALLBACK:%.*]] = tuple_extract [[MAT_RESULT]] : $(Builtin.RawPointer, Optional<Builtin.RawPointer>), 1  
  // CHECK: [[T1:%[0-9]+]] = pointer_to_address [[T0]] : $Builtin.RawPointer to [strict] $*Val
  // CHECK: [[VAL_REF_VAL_PROP_MAT:%.*]] = mark_dependence [[T1]] : $*Val on [[VAL_REF]]
  // CHECK: [[V_R_VP_Z_TUPLE_MAT:%[0-9]+]] = alloc_stack $(Int, Int)
  // CHECK: [[LD:%[0-9]+]] = load [copy] [[VAL_REF_VAL_PROP_MAT]]
  // -- val.ref.val_prop.z_tuple
  // CHECK: [[GET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg7z_tupleT
  // CHECK: [[A0:%.*]] = tuple_element_addr [[V_R_VP_Z_TUPLE_MAT]] : {{.*}}, 0
  // CHECK: [[A1:%.*]] = tuple_element_addr [[V_R_VP_Z_TUPLE_MAT]] : {{.*}}, 1
  // CHECK: [[V_R_VP_Z_TUPLE:%[0-9]+]] = apply [[GET_Z_TUPLE_METHOD]]([[LD]])
  // CHECK: [[T0:%.*]] = tuple_extract [[V_R_VP_Z_TUPLE]] : {{.*}}, 0
  // CHECK: [[T1:%.*]] = tuple_extract [[V_R_VP_Z_TUPLE]] : {{.*}}, 1
  // CHECK: store [[T0]] to [trivial] [[A0]]
  // CHECK: store [[T1]] to [trivial] [[A1]]
  // -- write to val.ref.val_prop.z_tuple.1
  // CHECK: [[V_R_VP_Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[V_R_VP_Z_TUPLE_MAT]] : {{.*}}, 1
  // CHECK: assign [[Z1]] to [[V_R_VP_Z_TUPLE_1]]
  // -- writeback to val.ref.val_prop.z_tuple
  // CHECK: [[WB_V_R_VP_Z_TUPLE:%[0-9]+]] = load [trivial] [[V_R_VP_Z_TUPLE_MAT]]
  // CHECK: [[SET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals7z_tupleT
  // CHECK: apply [[SET_Z_TUPLE_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL_REF_VAL_PROP_MAT]])
  // -- writeback to val.ref.val_prop
  // CHECK: switch_enum [[OPT_CALLBACK]] : $Optional<Builtin.RawPointer>, case #Optional.some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.none!enumelt: [[CONT:bb[0-9]+]]
  // CHECK: [[WRITEBACK]]([[CALLBACK_ADDR:%.*]] : $Builtin.RawPointer):
  // CHECK: [[CALLBACK:%.*]] = pointer_to_thin_function [[CALLBACK_ADDR]] : $Builtin.RawPointer to $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Ref, @thick Ref.Type) -> ()
  // CHECK: [[REF_MAT:%.*]] = alloc_stack $Ref
  // CHECK: store [[VAL_REF]] to [init] [[REF_MAT]]
  // CHECK: [[T0:%.*]] = metatype $@thick Ref.Type
  // CHECK: [[T1:%.*]] = address_to_pointer [[VAL_REF_VAL_PROP_MAT]]
  // CHECK: apply [[CALLBACK]]([[T1]], [[STORAGE]], [[REF_MAT]], [[T0]])
  // CHECK: br [[CONT]]
  // CHECK: [[CONT]]:
  // -- cleanup
  // CHECK: dealloc_stack [[V_R_VP_Z_TUPLE_MAT]]
  // CHECK: dealloc_stack [[VAL_REF_VAL_PROP_TEMP]]
  // -- don't need to write back to val.ref because it's a ref type
}

func reftype_rvalue() -> Ref {}

// CHECK-LABEL: sil hidden  @_TF10properties18reftype_rvalue_set
func reftype_rvalue_set(_ value: Val) {
  reftype_rvalue().val_prop = value
}

// CHECK-LABEL: sil hidden  @_TF10properties27tuple_in_logical_struct_set
func tuple_in_logical_struct_set(_ value: inout Val, z1: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z1:%[0-9]+]] : $Int):
  value.z_tuple.1 = z1
  // CHECK: [[Z_TUPLE_MATERIALIZED:%[0-9]+]] = alloc_stack $(Int, Int)
  // CHECK: [[VAL1:%[0-9]+]] = load [copy] [[VAL]]
  // CHECK: [[Z_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg7z_tupleT
  // CHECK: [[A0:%.*]] = tuple_element_addr [[Z_TUPLE_MATERIALIZED]] : {{.*}}, 0
  // CHECK: [[A1:%.*]] = tuple_element_addr [[Z_TUPLE_MATERIALIZED]] : {{.*}}, 1
  // CHECK: [[Z_TUPLE:%[0-9]+]] = apply [[Z_GET_METHOD]]([[VAL1]])
  // CHECK: [[T0:%.*]] = tuple_extract [[Z_TUPLE]] : {{.*}}, 0
  // CHECK: [[T1:%.*]] = tuple_extract [[Z_TUPLE]] : {{.*}}, 1
  // CHECK: store [[T0]] to [trivial] [[A0]]
  // CHECK: store [[T1]] to [trivial] [[A1]]
  // CHECK: [[Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[Z_TUPLE_MATERIALIZED]] : {{.*}}, 1
  // CHECK: assign [[Z1]] to [[Z_TUPLE_1]]
  // CHECK: [[Z_TUPLE_MODIFIED:%[0-9]+]] = load [trivial] [[Z_TUPLE_MATERIALIZED]]
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals7z_tupleT
  // CHECK: apply [[Z_SET_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL]])
  // CHECK: dealloc_stack [[Z_TUPLE_MATERIALIZED]]
  // CHECK: return
}

var global_prop : Int {
  // CHECK-LABEL: sil hidden  @_TF10propertiesg11global_prop
  get {
    return zero
  }
  // CHECK-LABEL: sil hidden  @_TF10propertiess11global_prop
  set {
    use(newValue)
  }
}

// CHECK-LABEL: sil hidden  @_TF10properties18logical_global_get
func logical_global_get() -> Int {
  return global_prop
  // CHECK: [[GET:%[0-9]+]] = function_ref @_TF10propertiesg11global_prop
  // CHECK: [[VALUE:%[0-9]+]] = apply [[GET]]()
  // CHECK: return [[VALUE]]
}

// CHECK-LABEL: sil hidden  @_TF10properties18logical_global_set
func logical_global_set(_ x: Int) {
  global_prop = x
  // CHECK: [[SET:%[0-9]+]] = function_ref @_TF10propertiess11global_prop
  // CHECK: apply [[SET]](%0)
}

// CHECK-LABEL: sil hidden  @_TF10properties17logical_local_get
func logical_local_get(_ x: Int) -> Int {
  var prop : Int {
    get {
      return x
    }
  }
  // CHECK: [[GET_REF:%[0-9]+]] = function_ref [[PROP_GET_CLOSURE:@_TFF10properties17logical_local_get]]
  // CHECK: apply [[GET_REF]](%0)
  return prop
}
// CHECK-: sil shared [[PROP_GET_CLOSURE]]
// CHECK: bb0(%{{[0-9]+}} : $Int):

// CHECK-LABEL: sil hidden  @_TF10properties26logical_local_captured_get
func logical_local_captured_get(_ x: Int) -> Int {
  var prop : Int {
    get {
      return x
    }
  }
  func get_prop() -> Int {
    return prop
  }

  return get_prop()
  // CHECK: [[FUNC_REF:%[0-9]+]] = function_ref @_TFF10properties26logical_local_captured_get
  // CHECK: apply [[FUNC_REF]](%0)
}
// CHECK: sil shared @_TFF10properties26logical_local_captured_get
// CHECK: bb0(%{{[0-9]+}} : $Int):

func inout_arg(_ x: inout Int) {}

// CHECK-LABEL: sil hidden  @_TF10properties14physical_inout
func physical_inout(_ x: Int) {
  var x = x
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PB:%.*]] = project_box [[XADDR]]
  inout_arg(&x)
  // CHECK: [[INOUT_ARG:%[0-9]+]] = function_ref @_TF10properties9inout_arg
  // CHECK: apply [[INOUT_ARG]]([[PB]])
}


/* TODO check writeback to more complex logical prop, check that writeback
 * reuses temporaries */

// CHECK-LABEL: sil hidden  @_TF10properties17val_subscript_get
// CHECK: bb0([[VVAL:%[0-9]+]] : $Val, [[I:%[0-9]+]] : $Int):
func val_subscript_get(_ v: Val, i: Int) -> Float {
  return v[i]
  // CHECK: [[SUBSCRIPT_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg9subscript
  // CHECK: [[RET:%[0-9]+]] = apply [[SUBSCRIPT_GET_METHOD]]([[I]], [[VVAL]]) : $@convention(method) (Int, @guaranteed Val)
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden  @_TF10properties17val_subscript_set
// CHECK: bb0(%0 : $Val, [[I:%[0-9]+]] : $Int, [[X:%[0-9]+]] : $Float):
func val_subscript_set(_ v: Val, i: Int, x: Float) {
  var v = v
  v[i] = x
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box ${ var Val }
  // CHECK: [[PB:%.*]] = project_box [[VADDR]]
  // CHECK: [[SUBSCRIPT_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals9subscript
  // CHECK: apply [[SUBSCRIPT_SET_METHOD]]([[X]], [[I]], [[PB]])
}

struct Generic<T> {
  var mono_phys:Int
  var mono_log: Int { get {} set {} }
  var typevar_member:T

  subscript(x: Int) -> Float { get {} set {} }

  subscript(x: T) -> T { get {} set {} }

  // CHECK-LABEL: sil hidden  @_TFV10properties7Generic19copy_typevar_member
  mutating
  func copy_typevar_member(_ x: Generic<T>) {
    typevar_member = x.typevar_member
  }
}

// CHECK-LABEL: sil hidden  @_TF10properties21generic_mono_phys_get
func generic_mono_phys_get<T>(_ g: Generic<T>) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #Generic.mono_phys
}

// CHECK-LABEL: sil hidden  @_TF10properties20generic_mono_log_get
func generic_mono_log_get<T>(_ g: Generic<T>) -> Int {
  return g.mono_log
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg8mono_log
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties20generic_mono_log_set
func generic_mono_log_set<T>(_ g: Generic<T>, x: Int) {
  var g = g
  g.mono_log = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Generics8mono_log
  // CHECK: apply [[GENERIC_SET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties26generic_mono_subscript_get
func generic_mono_subscript_get<T>(_ g: Generic<T>, i: Int) -> Float {
  return g[i]
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg9subscript
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @{{.*}}generic_mono_subscript_set
func generic_mono_subscript_set<T>(_ g: inout Generic<T>, i: Int, x: Float) {
  g[i] = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Generics9subscript
  // CHECK: apply [[GENERIC_SET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @{{.*}}bound_generic_mono_phys_get
func bound_generic_mono_phys_get(_ g: inout Generic<UnicodeScalar>, x: Int) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #Generic.mono_phys
}

// CHECK-LABEL: sil hidden  @_TF10properties26bound_generic_mono_log_get
func bound_generic_mono_log_get(_ g: Generic<UnicodeScalar>, x: Int) -> Int {
  return g.mono_log
// CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg8mono_log
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties22generic_subscript_type
func generic_subscript_type<T>(_ g: Generic<T>, i: T, x: T) -> T {
  var g = g
  g[i] = x
  return g[i]
}

/*TODO: archetype and existential properties and subscripts */

struct StaticProperty {
  static var foo: Int {
    get {
      return zero
    }
    set {}
  }
}

// CHECK-LABEL: sil hidden @_TF10properties10static_get
// CHECK:   function_ref @_TZFV10properties14StaticPropertyg3foo{{.*}} : $@convention(method) (@thin StaticProperty.Type) -> Int
func static_get() -> Int {
  return StaticProperty.foo
}

// CHECK-LABEL: sil hidden @_TF10properties10static_set
// CHECK:   function_ref @_TZFV10properties14StaticPropertys3foo{{.*}} : $@convention(method) (Int, @thin StaticProperty.Type) -> ()
func static_set(_ x: Int) {
  StaticProperty.foo = x
}

func takeInt(_ a : Int) {}

protocol ForceAccessors {
  var a: Int { get set }
}

struct DidSetWillSetTests: ForceAccessors {
  var a: Int {
    willSet(newA) {
      // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.willset
      // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestsw1a
      // CHECK: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug_value %0
      // CHECK-NEXT: debug_value_addr %1 : $*DidSetWillSetTests

      takeInt(a)

      // CHECK-NEXT: // function_ref properties.takeInt
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: [[FIELDPTR:%.*]] = struct_element_addr %1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [trivial] [[FIELDPTR]] : $*Int
      // CHECK-NEXT: apply [[TAKEINTFN]]([[A]]) : $@convention(thin) (Int) -> ()

      takeInt(newA)

      // CHECK-NEXT: // function_ref properties.takeInt (Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: apply [[TAKEINTFN]](%0) : $@convention(thin) (Int) -> ()
    }

    didSet {
      // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.didset
      // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestsW1a
      // CHECK: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug
      // CHECK-NEXT: debug_value_addr %1 : $*DidSetWillSetTests

      takeInt(a)

      // CHECK-NEXT: // function_ref properties.takeInt (Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr %1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [trivial] [[AADDR]] : $*Int
      // CHECK-NEXT: apply [[TAKEINTFN]]([[A]]) : $@convention(thin) (Int) -> ()

      a = zero  // reassign, but don't infinite loop.

      // CHECK-NEXT: // function_ref properties.zero.unsafeMutableAddressor : Swift.Int
      // CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @_TF10propertiesau4zero
      // CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
      // CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to [strict] $*Int
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr %1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: copy_addr [[ZEROADDR]] to [[AADDR]] : $*Int
    }
  }

  init(x : Int) {
    // Accesses to didset/willset variables are direct in init methods and dtors.
    a = x
    a = x
  }

  // These are the synthesized getter and setter for the willset/didset variable.

  // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.getter
  // CHECK-NEXT: sil hidden [transparent] @_TFV10properties18DidSetWillSetTestsg1aSi
  // CHECK: bb0(%0 : $DidSetWillSetTests):
  // CHECK-NEXT:   debug_value %0
  // CHECK-NEXT:   %2 = struct_extract %0 : $DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT:   return %2 : $Int{{.*}}                      // id: %3

  // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.setter
  // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestss1aSi
  // CHECK: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
  // CHECK-NEXT: debug_value %0
  // CHECK-NEXT: debug_value_addr %1

  // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr %1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT: [[OLDVAL:%.*]] = load [trivial] [[AADDR]] : $*Int
  // CHECK-NEXT: debug_value [[OLDVAL]] : $Int, let, name "tmp"

  // CHECK-NEXT: // function_ref {{.*}}.DidSetWillSetTests.a.willset : Swift.Int
  // CHECK-NEXT: [[WILLSETFN:%.*]] = function_ref @_TFV10properties18DidSetWillSetTestsw1a
  // CHECK-NEXT:  apply [[WILLSETFN]](%0, %1) : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
  // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr %1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT: assign %0 to [[AADDR]] : $*Int
  // CHECK-NEXT: // function_ref {{.*}}.DidSetWillSetTests.a.didset : Swift.Int
  // CHECK-NEXT: [[DIDSETFN:%.*]] = function_ref @_TFV10properties18DidSetWillSetTestsW1a{{.*}} : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
  // CHECK-NEXT: apply [[DIDSETFN]]([[OLDVAL]], %1) : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()

  // CHECK-LABEL: sil hidden @_TFV10properties18DidSetWillSetTestsC
  // CHECK: bb0(%0 : $Int, %1 : $@thin DidSetWillSetTests.Type):
  // CHECK:        [[SELF:%.*]] = mark_uninitialized [rootself]
  // CHECK:        [[P1:%.*]] = struct_element_addr [[SELF]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT:   assign %0 to [[P1]]
  // CHECK:        [[P2:%.*]] = struct_element_addr [[SELF]] : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT:   assign %0 to [[P2]]
}


// Test global observing properties.

var global_observing_property : Int = zero {
  didSet {
    takeInt(global_observing_property)
  }
}

func force_global_observing_property_setter() {
  let x = global_observing_property
  global_observing_property = x
}

// The property is initialized with "zero".
// CHECK-LABEL: sil private @globalinit_{{.*}}_func1 : $@convention(thin) () -> () {
// CHECK: bb0:
// CHECK-NEXT: alloc_global @_Tv10properties25global_observing_propertySi
// CHECK-NEXT: %1 = global_addr @_Tv10properties25global_observing_propertySi : $*Int
// CHECK: properties.zero.unsafeMutableAddressor
// CHECK: return

// The didSet implementation needs to call takeInt.

// CHECK-LABEL: sil hidden @_TF10propertiesW25global_observing_property
// CHECK: function_ref properties.takeInt
// CHECK-NEXT: function_ref @_TF10properties7takeInt

// The setter needs to call didSet implementation.

// CHECK-LABEL: sil hidden @_TF10propertiess25global_observing_property
// CHECK: function_ref properties.global_observing_property.unsafeMutableAddressor
// CHECK-NEXT:  function_ref @_TF10propertiesau25global_observing_property
// CHECK: function_ref properties.global_observing_property.didset
// CHECK-NEXT: function_ref @_TF10propertiesW25global_observing_property


// Test local observing properties.

func local_observing_property(_ arg: Int) {
  var localproperty: Int = arg {
    didSet {
      takeInt(localproperty)
    }
  }
  
  takeInt(localproperty)
  localproperty = arg
}

// This is the local_observing_property function itself.  First alloc and 
// initialize the property to the argument value.

// CHECK-LABEL: sil hidden @{{.*}}local_observing_property
// CHECK: bb0([[ARG:%[0-9]+]] : $Int)
// CHECK: [[BOX:%[0-9]+]] = alloc_box ${ var Int }
// CHECK: [[PB:%.*]] = project_box [[BOX]]
// CHECK: store [[ARG]] to [trivial] [[PB]]




// <rdar://problem/16006333> observing properties don't work in @objc classes
@objc
class ObservingPropertyInObjCClass {
  var bounds: Int {
    willSet {}
    didSet {}
  }

  init(b: Int) { bounds = b }
}



// Superclass init methods should not get direct access to be class properties.
// rdar://16151899

class rdar16151899Base {
  var x: Int = zero {
  willSet {
    use(x)
  }
  }
}

class rdar16151899Derived : rdar16151899Base {
    // CHECK-LABEL: sil hidden @_TFC10properties19rdar16151899Derivedc
    override init() {
        super.init()
        // CHECK: upcast {{.*}} : $rdar16151899Derived to $rdar16151899Base
        // CHECK: function_ref @_TFC10properties16rdar16151899BasecfT_S0_ : $@convention(method) (@owned rdar16151899Base) -> @owned rdar16151899Base

        // This should not be a direct access, it should call the setter in the
        // base.
        x = zero
        
        // CHECK:  [[BASEPTR:%[0-9]+]] = upcast {{.*}} : $rdar16151899Derived to $rdar16151899Base
        // CHECK: load{{.*}}Int
        // CHECK-NEXT: [[SETTER:%[0-9]+]] = class_method {{.*}} : $rdar16151899Base, #rdar16151899Base.x!setter.1 : (rdar16151899Base)
        // CHECK-NEXT: apply [[SETTER]]({{.*}}, [[BASEPTR]]) 
    }
}


func propertyWithDidSetTakingOldValue() {
  var p : Int = zero {
    didSet(oldValue) {
      // access to oldValue
      use(oldValue)
      // and newValue.
      use(p)
    }
  }

  p = zero
}

// CHECK: // properties.(propertyWithDidSetTakingOldValue () -> ()).(p #1).setter : Swift.Int
// CHECK-NEXT: sil {{.*}} @_TFF10properties32propertyWithDidSetTakingOldValue
// CHECK: bb0([[ARG1:%.*]] : $Int, [[ARG2:%.*]] : ${ var Int }):
// CHECK-NEXT:  debug_value [[ARG1]] : $Int, let, name "newValue", argno 1
// CHECK-NEXT:  [[ARG2_PB:%.*]] = project_box [[ARG2]]
// CHECK-NEXT:  debug_value_addr [[ARG2_PB]] : $*Int, var, name "p", argno 2
// CHECK-NEXT:  [[ARG2_PB_VAL:%.*]] = load [trivial] [[ARG2_PB]] : $*Int
// CHECK-NEXT:  debug_value [[ARG2_PB_VAL]] : $Int
// CHECK-NEXT:  assign [[ARG1]] to [[ARG2_PB]] : $*Int
// CHECK-NEXT:  [[ARG2_COPY:%.*]] = copy_value [[ARG2]] : ${ var Int }
// SEMANTIC ARC TODO: Another case where we need to put the mark_function_escape on a new projection after a copy.
// CHECK-NEXT:  mark_function_escape [[ARG2_PB]]
// CHECK-NEXT:  // function_ref
// CHECK-NEXT:  [[FUNC:%.*]] = function_ref @_TFF10properties32propertyWithDidSetTakingOldValueFT_T_WL_1pSi : $@convention(thin) (Int, @owned { var Int }) -> ()
// CHECK-NEXT:  %11 = apply [[FUNC]]([[ARG2_PB_VAL]], [[ARG2_COPY]]) : $@convention(thin) (Int, @owned { var Int }) -> ()
// CHECK-NEXT:  destroy_value [[ARG2]] : ${ var Int }
// CHECK-NEXT:  %13 = tuple ()
// CHECK-NEXT:  return %13 : $()
// CHECK-NEXT:} // end sil function '_TFF10properties32propertyWithDidSetTakingOldValue{{.*}}'


class BaseProperty {
  var x : Int { get {} set {} }
}

class DerivedProperty : BaseProperty {
  override var x : Int { get {} set {} }

  func super_property_reference() -> Int {
    return super.x
  }
}

// rdar://16381392 - Super property references in non-objc classes should be direct.

// CHECK-LABEL: sil hidden @_TFC10properties15DerivedProperty24super_property_referencefT_Si : $@convention(method) (@guaranteed DerivedProperty) -> Int {
// CHECK: bb0([[SELF:%.*]] : $DerivedProperty):
// CHECK:   [[SELF_COPY:%[0-9]+]] = copy_value [[SELF]]
// CHECK:   [[BASEPTR:%[0-9]+]] = upcast [[SELF_COPY]] : $DerivedProperty to $BaseProperty
// CHECK:   [[FN:%[0-9]+]] = function_ref @_TFC10properties12BasePropertyg1xSi : $@convention(method) (@guaranteed BaseProperty) -> Int 
// CHECK:   [[RESULT:%.*]] = apply [[FN]]([[BASEPTR]]) : $@convention(method) (@guaranteed BaseProperty) -> Int
// CHECK:   destroy_value [[BASEPTR]]
// CHECK:   return [[RESULT]] : $Int
// CHECK: } // end sil function '_TFC10properties15DerivedProperty24super_property_referencefT_Si'


// <rdar://problem/16411449> ownership qualifiers don't work with non-mutating struct property
struct ReferenceStorageTypeRValues {
  unowned var p1 : Ref

  func testRValueUnowned() -> Ref {
    return p1
  }
// CHECK: sil hidden @{{.*}}testRValueUnowned
// CHECK: bb0(%0 : $ReferenceStorageTypeRValues):
// CHECK-NEXT:   debug_value %0 : $ReferenceStorageTypeRValues
// CHECK-NEXT:   %2 = struct_extract %0 : $ReferenceStorageTypeRValues, #ReferenceStorageTypeRValues.p1
// CHECK-NEXT:   strong_retain_unowned %2 : $@sil_unowned Ref
// CHECK-NEXT:   %4 = unowned_to_ref %2 : $@sil_unowned Ref to $Ref
// CHECK-NEXT:   return %4 : $Ref

  init() {
  }
}


// <rdar://problem/16406886> Observing properties don't work with ownership types
struct ObservingPropertiesWithOwnershipTypes {
  unowned var alwaysPresent : Ref {
    didSet {
    }
  }

  init(res: Ref) {
    alwaysPresent = res
  }
}

struct ObservingPropertiesWithOwnershipTypesInferred {
  unowned var alwaysPresent = Ref(i: 0) {
    didSet {
    }
  }

  weak var maybePresent = nil as Ref? {
    willSet {
    }
  }
}

// <rdar://problem/16554876> property accessor synthesization of weak variables doesn't work
protocol WeakPropertyProtocol {
 weak var maybePresent : Ref? { get set }
}

struct WeakPropertyStruct : WeakPropertyProtocol {
 weak var maybePresent : Ref?

  init() {
    maybePresent = nil
  }
}

// <rdar://problem/16629598> direct property accesses to generic struct
// properties were being mischecked as computed property accesses.

struct SomeGenericStruct<T> {
  var x: Int
}

// CHECK-LABEL: sil hidden @_TF10properties4getX
// CHECK:         struct_extract {{%.*}} : $SomeGenericStruct<T>, #SomeGenericStruct.x
func getX<T>(_ g: SomeGenericStruct<T>) -> Int {
  return g.x
}


// <rdar://problem/16189360> [DF] Assert on subscript with variadic parameter
struct VariadicSubscript {
  subscript(subs: Int...) -> Int {
    get {
      return 42
    }
  }

  func test() {
    var s = VariadicSubscript()
    var x = s[0, 1, 2]
  }
}


//<rdar://problem/16620121> Initializing constructor tries to initialize computed property overridden with willSet/didSet
class ObservedBase {
     var printInfo: Ref!
}
class ObservedDerived : ObservedBase {
  override init() {}
  override var printInfo: Ref! {
    didSet { }
  }
}



/// <rdar://problem/16953517> Class properties should be allowed in protocols, even without stored class properties
protocol ProtoWithClassProp {
  static var x: Int { get }
}

class ClassWithClassProp : ProtoWithClassProp {
  class var x: Int {
  return 42
  }
}

struct StructWithClassProp : ProtoWithClassProp {
  static var x: Int {
  return 19
  }
}


func getX<T : ProtoWithClassProp>(_ a : T) -> Int {
  return T.x
}

func testClassPropertiesInProtocol() -> Int {
  return getX(ClassWithClassProp())+getX(StructWithClassProp())
}

class GenericClass<T> {
  var x: T
  var y: Int
  final let z: T

  init() { fatalError("scaffold") }
}

// CHECK-LABEL: sil hidden @_TF10properties12genericPropsFGCS_12GenericClassSS_T_ 
func genericProps(_ x: GenericClass<String>) {
  // CHECK: class_method %0 : $GenericClass<String>, #GenericClass.x!getter.1
  let _ = x.x
  // CHECK: class_method %0 : $GenericClass<String>, #GenericClass.y!getter.1
  let _ = x.y
  // CHECK: [[Z:%.*]] = ref_element_addr %0 : $GenericClass<String>, #GenericClass.z
  // CHECK: load [copy] [[Z]] : $*String
  let _ = x.z
}

// CHECK-LABEL: sil hidden @_TF10properties28genericPropsInGenericContext
func genericPropsInGenericContext<U>(_ x: GenericClass<U>) {
  // CHECK: [[Z:%.*]] = ref_element_addr %0 : $GenericClass<U>, #GenericClass.z
  // CHECK: copy_addr [[Z]] {{.*}} : $*U
  let _ = x.z
}


// <rdar://problem/18275556> 'let' properties in a class should be implicitly final
class ClassWithLetProperty {
  let p = 42
  dynamic let q = 97

  // We shouldn't have any dynamic dispatch within this method, just load p.
  func ReturnConstant() -> Int { return p }
// CHECK-LABEL: sil hidden @_TFC10properties20ClassWithLetProperty14ReturnConstant
// CHECK:       bb0([[ARG:%.*]] : $ClassWithLetProperty):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[PTR:%[0-9]+]] = ref_element_addr [[ARG]] : $ClassWithLetProperty, #ClassWithLetProperty.p
// CHECK-NEXT:    [[VAL:%[0-9]+]] = load [trivial] [[PTR]] : $*Int
// CHECK-NEXT:   return [[VAL]] : $Int


  // This property is marked dynamic, so go through the getter, always.
  func ReturnDynamicConstant() -> Int { return q }
// CHECK-LABEL: sil hidden @_TFC10properties20ClassWithLetProperty21ReturnDynamicConstant
// CHECK: class_method [volatile] %0 : $ClassWithLetProperty, #ClassWithLetProperty.q!getter.1.foreign
}


// <rdar://problem/19254812> DI bug when referencing let member of a class
class r19254812Base {}
class r19254812Derived: r19254812Base{
  let pi = 3.14159265359
  
  init(x : ()) {
    use(pi)
  }
  
// Accessing the "pi" property should not copy_value/release self.
// CHECK-LABEL: sil hidden @_TFC10properties16r19254812Derivedc
// CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [derivedself] 

// Initialization of the pi field: no copy_values/releases.
// CHECK:  [[SELF:%[0-9]+]] = load_borrow [[SELFMUI]] : $*r19254812Derived
// CHECK-NEXT:  [[PIPTR:%[0-9]+]] = ref_element_addr [[SELF]] : $r19254812Derived, #r19254812Derived.pi
// CHECK-NEXT:  assign {{.*}} to [[PIPTR]] : $*Double

// CHECK-NOT: destroy_value
// CHECK-NOT: copy_value

// Load of the pi field: no copy_values/releases.
// CHECK:  [[SELF:%[0-9]+]] = load_borrow [[SELFMUI]] : $*r19254812Derived
// CHECK-NEXT:  [[PIPTR:%[0-9]+]] = ref_element_addr [[SELF]] : $r19254812Derived, #r19254812Derived.pi
// CHECK-NEXT:  {{.*}} = load [trivial] [[PIPTR]] : $*Double
// CHECK: return
}


class RedundantSelfRetains {
  final var f : RedundantSelfRetains
  
  init() {
    f = RedundantSelfRetains()
  }
  
  // <rdar://problem/19275047> Extraneous copy_values/releases of self are bad
  func testMethod1() {
    f = RedundantSelfRetains()
  }
  // CHECK-LABEL: sil hidden @_TFC10properties20RedundantSelfRetains11testMethod1
  // CHECK: bb0(%0 : $RedundantSelfRetains):

  // CHECK-NOT: copy_value
  
  // CHECK: [[FPTR:%[0-9]+]] = ref_element_addr %0 : $RedundantSelfRetains, #RedundantSelfRetains.f
  // CHECK-NEXT: assign {{.*}} to [[FPTR]] : $*RedundantSelfRetains

  // CHECK: return
}

class RedundantRetains {
  final var field = 0
}

func testRedundantRetains() {
  let a = RedundantRetains()
  a.field = 4  // no copy_value/release of a necessary here.
}

// CHECK-LABEL: sil hidden @_TF10properties20testRedundantRetainsFT_T_ : $@convention(thin) () -> () {
// CHECK: [[A:%[0-9]+]] = apply
// CHECK-NOT: copy_value
// CHECK: destroy_value [[A]] : $RedundantRetains
// CHECK-NOT: copy_value
// CHECK-NOT: destroy_value
// CHECK: return

struct AddressOnlyNonmutatingSet<T> {
  var x: T
  init(x: T) { self.x = x }
  var prop: Int {
    get { return 0 }
    nonmutating set { }
  }
}

func addressOnlyNonmutatingProperty<T>(_ x: AddressOnlyNonmutatingSet<T>)
-> Int {
  x.prop = 0
  return x.prop
}
// CHECK-LABEL: sil hidden @_TF10properties30addressOnlyNonmutatingProperty
// CHECK:         [[SET:%.*]] = function_ref @_TFV10properties25AddressOnlyNonmutatingSets4propSi
// CHECK:         apply [[SET]]<T>({{%.*}}, [[TMP:%[0-9]*]])
// CHECK:         destroy_addr [[TMP]]
// CHECK:         dealloc_stack [[TMP]]
// CHECK:         [[GET:%.*]] = function_ref @_TFV10properties25AddressOnlyNonmutatingSetg4propSi
// CHECK:         apply [[GET]]<T>([[TMP:%[0-9]*]])
// CHECK:         destroy_addr [[TMP]]
// CHECK:         dealloc_stack [[TMP]]

protocol MakeAddressOnly {}
struct AddressOnlyReadOnlySubscript {
  var x: MakeAddressOnly?

  subscript(z: Int) -> Int { return z }
}

// CHECK-LABEL: sil hidden @_TF10properties43addressOnlyReadOnlySubscriptFromMutableBase
// CHECK:         [[BASE:%.*]] = alloc_box ${ var AddressOnlyReadOnlySubscript }
// CHECK:         copy_addr [[BASE:%.*]] to [initialization] [[COPY:%.*]] :
// CHECK:         [[GETTER:%.*]] = function_ref @_TFV10properties28AddressOnlyReadOnlySubscriptg9subscript
// CHECK:         apply [[GETTER]]({{%.*}}, [[COPY]])
func addressOnlyReadOnlySubscriptFromMutableBase(_ x: Int) {
  var base = AddressOnlyReadOnlySubscript()
  _ = base[x]
}



/// <rdar://problem/20912019> passing unmaterialized r-value as inout argument
struct MutatingGetterStruct {
  var write: Int {
    mutating get {  }
  }

  // CHECK-LABEL: sil hidden @_TZFV10properties20MutatingGetterStruct4test
  // CHECK: [[X:%.*]] = alloc_box ${ var MutatingGetterStruct }, var, name "x"
  // CHECK-NEXT: [[PB:%.*]] = project_box [[X]]
  // CHECK: store {{.*}} to [trivial] [[PB]] : $*MutatingGetterStruct
  // CHECK: apply {{%.*}}([[PB]]) : $@convention(method) (@inout MutatingGetterStruct) -> Int
  static func test() {
    var x = MutatingGetterStruct()
    _ = x.write
  }
}


protocol ProtocolWithReadWriteSubscript {
  subscript(i: Int) -> Int { get set }
}

struct CrashWithUnnamedSubscript : ProtocolWithReadWriteSubscript {
  subscript(_: Int) -> Int { get { } set { } }
}


/// <rdar://problem/26408353> crash when overriding internal property with
/// public property

public class BaseClassWithInternalProperty {
  var x: () = ()
}

public class DerivedClassWithPublicProperty : BaseClassWithInternalProperty {
  public override var x: () {
    didSet {}
  }
}

// CHECK-LABEL: sil hidden [transparent] @_TFC10properties29BaseClassWithInternalPropertyg1xT_

// CHECK-LABEL: sil [transparent] [fragile] @_TFC10properties30DerivedClassWithPublicPropertyg1xT_
// CHECK:       bb0([[SELF:%.*]] : $DerivedClassWithPublicProperty):
// CHECK:         [[SELF_COPY:%.*]] = copy_value [[SELF]] : $DerivedClassWithPublicProperty
// CHECK-NEXT:    [[SUPER:%.*]] = upcast [[SELF_COPY]] : $DerivedClassWithPublicProperty to $BaseClassWithInternalProperty
// CHECK-NEXT:    [[METHOD:%.*]] = super_method [[SELF_COPY]] : $DerivedClassWithPublicProperty, #BaseClassWithInternalProperty.x!getter.1 : (BaseClassWithInternalProperty) -> () -> () , $@convention(method) (@guaranteed BaseClassWithInternalProperty) -> ()
// CHECK-NEXT:    [[RESULT:%.*]] = apply [[METHOD]]([[SUPER]]) : $@convention(method) (@guaranteed BaseClassWithInternalProperty) -> ()
// CHECK-NEXT:    destroy_value [[SUPER]] : $BaseClassWithInternalProperty
// CHECK: } // end sil function '_TFC10properties30DerivedClassWithPublicPropertyg1xT_'
