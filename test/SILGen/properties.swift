// RUN: %target-swift-frontend  -parse-as-library -emit-silgen -disable-objc-attr-requires-foundation-module %s | FileCheck %s

var zero: Int = 0

func use(_: Int) {}
func use(_: Double) {}
func getInt() -> Int { return zero }

// CHECK-LABEL: sil hidden  @{{.*}}physical_tuple_lvalue
// CHECK-NEXT: bb0(%0 : $Int):
func physical_tuple_lvalue(c: Int) {
  var x : (Int, Int)
  // CHECK: [[XADDR1:%[0-9]+]] = alloc_box $(Int, Int)
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
func tuple_assignment(inout a: Int, inout b: Int) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int, [[B_ADDR:%[0-9]+]] : $*Int):
  // CHECK: [[A_LOCAL:%.*]] = alloc_box $Int
  // CHECK: [[B_LOCAL:%.*]] = alloc_box $Int
  // CHECK: [[B:%[0-9]+]] = load [[B_LOCAL]]#1
  // CHECK: [[A:%[0-9]+]] = load [[A_LOCAL]]#1
  // CHECK: assign [[B]] to [[A_LOCAL]]#1
  // CHECK: assign [[A]] to [[B_LOCAL]]#1
  (a, b) = (b, a)
}

// CHECK-LABEL: sil hidden  @_TF10properties18tuple_assignment_2
func tuple_assignment_2(inout a: Int, inout b: Int, xy: (Int, Int)) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int, [[B_ADDR:%[0-9]+]] : $*Int, [[X:%[0-9]+]] : $Int, [[Y:%[0-9]+]] : $Int):
  // CHECK: [[A_LOCAL:%.*]] = alloc_box $Int
  // CHECK: [[B_LOCAL:%.*]] = alloc_box $Int
  (a, b) = xy
  // CHECK: [[XY2:%[0-9]+]] = tuple ([[X]] : $Int, [[Y]] : $Int)
  // CHECK: [[X:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 0
  // CHECK: [[Y:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 1
  // CHECK: assign [[X]] to [[A_LOCAL]]#1
  // CHECK: assign [[Y]] to [[B_LOCAL]]#1
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
func physical_struct_lvalue(c: Int) {
  var v : Val
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  v.y = c
  // CHECK: assign %0 to [[X_1]]
}

// CHECK-LABEL: sil hidden  @_TF10properties21physical_class_lvalue
 func physical_class_lvalue(r: Ref, a: Int) {
    r.y = a

   // CHECK: [[FN:%[0-9]+]] = class_method %0 : $Ref, #Ref.y!setter.1
   // CHECK: apply [[FN]](%1, %0) : $@convention(method) (Int, @guaranteed Ref) -> ()
   // CHECK: strong_release %0 : $Ref
  }


// CHECK-LABEL: sil hidden  @_TF10properties24physical_subclass_lvalue
 func physical_subclass_lvalue(r: RefSubclass, a: Int) {
    r.y = a
   // strong_retain %0 : $RefSubclass
   // CHECK: [[R_SUP:%[0-9]+]] = upcast %0 : $RefSubclass to $Ref
   // CHECK: [[FN:%[0-9]+]] = class_method [[R_SUP]] : $Ref, #Ref.y!setter.1 : Ref -> (Int) -> () , $@convention(method) (Int, @guaranteed Ref) -> ()
   // CHECK: apply [[FN]](%1, [[R_SUP]]) :
   // CHECK: strong_release [[R_SUP]]
    r.w = a

   // CHECK: [[FN:%[0-9]+]] = class_method %0 : $RefSubclass, #RefSubclass.w!setter.1
   // CHECK: apply [[FN]](%1, %0) : $@convention(method) (Int, @guaranteed RefSubclass) -> ()
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
func logical_struct_set(inout value: Val, z: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z:%[0-9]+]] : $Int):
  value.z = z
  // CHECK: [[VAL_LOCAL:%[0-9]+]] = alloc_box $Val
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals1z
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL_LOCAL]]#1)
  // CHECK: return
}

// CHECK-LABEL: sil hidden  @_TF10properties27logical_struct_in_tuple_set
func logical_struct_in_tuple_set(inout value: (Int, Val), z: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*(Int, Val), [[Z:%[0-9]+]] : $Int):
  value.1.z = z
  // CHECK: [[VAL_LOCAL:%[0-9]+]] = alloc_box $(Int, Val)
  // CHECK: [[VAL_1:%[0-9]+]] = tuple_element_addr [[VAL_LOCAL]]#1 : {{.*}}, 1
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals1z
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL_1]])
  // CHECK: return
}

// CHECK-LABEL: sil hidden  @_TF10properties29logical_struct_in_reftype_set
func logical_struct_in_reftype_set(inout value: Val, z1: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z1:%[0-9]+]] : $Int):
  value.ref.val_prop.z_tuple.1 = z1
  // CHECK: [[VAL_LOCAL:%[0-9]+]] = alloc_box $Val
  // -- val.ref
  // CHECK: [[VAL_REF_ADDR:%[0-9]+]] = struct_element_addr [[VAL_LOCAL]]#1 : $*Val, #Val.ref
  // CHECK: [[VAL_REF:%[0-9]+]] = load [[VAL_REF_ADDR]]
  // -- getters and setters
  // -- val.ref.val_prop
  // CHECK: [[STORAGE:%.*]] = alloc_stack $Builtin.UnsafeValueBuffer
  // CHECK: [[VAL_REF_VAL_PROP_TEMP:%.*]] = alloc_stack $Val
  // CHECK: [[T0:%.*]] = address_to_pointer [[VAL_REF_VAL_PROP_TEMP]]#1 : $*Val to $Builtin.RawPointer
  // CHECK: [[MAT_VAL_PROP_METHOD:%[0-9]+]] = class_method {{.*}} : $Ref, #Ref.val_prop!materializeForSet.1 : Ref -> (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer) -> (Builtin.RawPointer, (@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Ref, @thick Ref.Type) -> ())?)
  // CHECK: [[MAT_RESULT:%[0-9]+]] = apply [[MAT_VAL_PROP_METHOD]]([[T0]], [[STORAGE]]#1, [[VAL_REF]])
  // CHECK: [[T0:%.*]] = tuple_extract [[MAT_RESULT]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Ref, @thick Ref.Type) -> ()>), 0
  // CHECK: [[T1:%[0-9]+]] = pointer_to_address [[T0]] : $Builtin.RawPointer to $*Val
  // CHECK: [[OPT_CALLBACK:%.*]] = tuple_extract [[MAT_RESULT]] : $(Builtin.RawPointer, Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Ref, @thick Ref.Type) -> ()>), 1  
  // CHECK: [[VAL_REF_VAL_PROP_MAT:%.*]] = mark_dependence [[T1]] : $*Val on [[VAL_REF]]
  // CHECK: [[V_R_VP_Z_TUPLE_MAT:%[0-9]+]] = alloc_stack $(Int, Int)
  // CHECK: [[LD:%[0-9]+]] = load [[VAL_REF_VAL_PROP_MAT]]
  // CHECK: retain_value [[LD]]
  // -- val.ref.val_prop.z_tuple
  // CHECK: [[GET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg7z_tupleT
  // CHECK: [[V_R_VP_Z_TUPLE:%[0-9]+]] = apply [[GET_Z_TUPLE_METHOD]]([[LD]])
  // CHECK: store [[V_R_VP_Z_TUPLE]] to [[V_R_VP_Z_TUPLE_MAT]]#1
  // -- write to val.ref.val_prop.z_tuple.1
  // CHECK: [[V_R_VP_Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[V_R_VP_Z_TUPLE_MAT]]#1 : {{.*}}, 1
  // CHECK: assign [[Z1]] to [[V_R_VP_Z_TUPLE_1]]
  // -- writeback to val.ref.val_prop.z_tuple
  // CHECK: [[WB_V_R_VP_Z_TUPLE:%[0-9]+]] = load [[V_R_VP_Z_TUPLE_MAT]]#1
  // CHECK: [[SET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals7z_tupleT
  // CHECK: apply [[SET_Z_TUPLE_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL_REF_VAL_PROP_MAT]])
  // -- writeback to val.ref.val_prop
  // CHECK: switch_enum [[OPT_CALLBACK]] : $Optional<@convention(thin) (Builtin.RawPointer, inout Builtin.UnsafeValueBuffer, inout Ref, @thick Ref.Type) -> ()>, case #Optional.Some!enumelt.1: [[WRITEBACK:bb[0-9]+]], case #Optional.None!enumelt: [[CONT:bb[0-9]+]]
  // CHECK: [[WRITEBACK]]([[CALLBACK:%.*]] : $@convention(thin) (Builtin.RawPointer, @inout Builtin.UnsafeValueBuffer, @inout Ref, @thick Ref.Type) -> ()):
  // CHECK: [[REF_MAT:%.*]] = alloc_stack $Ref
  // CHECK: store [[VAL_REF]] to [[REF_MAT]]#1
  // CHECK: [[T0:%.*]] = metatype $@thick Ref.Type
  // CHECK: [[T1:%.*]] = address_to_pointer [[VAL_REF_VAL_PROP_MAT]]
  // CHECK: apply [[CALLBACK]]([[T1]], [[STORAGE]]#1, [[REF_MAT]]#1, [[T0]])
  // CHECK: br [[CONT]]
  // CHECK: [[CONT]]:
  // -- cleanup
  // CHECK: dealloc_stack [[V_R_VP_Z_TUPLE_MAT]]#0
  // CHECK: dealloc_stack [[VAL_REF_VAL_PROP_TEMP]]#0
  // -- don't need to write back to val.ref because it's a ref type
}

func reftype_rvalue() -> Ref {}

// CHECK-LABEL: sil hidden  @_TF10properties18reftype_rvalue_set
func reftype_rvalue_set(value: Val) {
  reftype_rvalue().val_prop = value
}

// CHECK-LABEL: sil hidden  @_TF10properties27tuple_in_logical_struct_set
func tuple_in_logical_struct_set(inout value: Val, z1: Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z1:%[0-9]+]] : $Int):
  value.z_tuple.1 = z1
  // CHECK: [[VAL_LOCAL:%[0-9]+]] = alloc_box $Val
  // CHECK: [[Z_TUPLE_MATERIALIZED:%[0-9]+]] = alloc_stack $(Int, Int)
  // CHECK: [[VAL1:%[0-9]+]] = load [[VAL_LOCAL]]
  // CHECK: retain_value [[VAL1]]
  // CHECK: [[Z_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg7z_tupleT
  // CHECK: [[Z_TUPLE:%[0-9]+]] = apply [[Z_GET_METHOD]]([[VAL1]])
  // CHECK: store [[Z_TUPLE]] to [[Z_TUPLE_MATERIALIZED]]#1
  // CHECK: [[Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[Z_TUPLE_MATERIALIZED]]#1 : {{.*}}, 1
  // CHECK: assign [[Z1]] to [[Z_TUPLE_1]]
  // CHECK: [[Z_TUPLE_MODIFIED:%[0-9]+]] = load [[Z_TUPLE_MATERIALIZED]]#1
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals7z_tupleT
  // CHECK: apply [[Z_SET_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL_LOCAL]]#1)
  // CHECK: dealloc_stack [[Z_TUPLE_MATERIALIZED]]#0
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
func logical_global_set(x: Int) {
  global_prop = x
  // CHECK: [[SET:%[0-9]+]] = function_ref @_TF10propertiess11global_prop
  // CHECK: apply [[SET]](%0)
}

// CHECK-LABEL: sil hidden  @_TF10properties17logical_local_get
func logical_local_get(x: Int) -> Int {
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
func logical_local_captured_get(x: Int) -> Int {
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

func inout_arg(inout x: Int) {}

// CHECK-LABEL: sil hidden  @_TF10properties14physical_inout
func physical_inout(var x: Int) {
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
  inout_arg(&x)
  // CHECK: [[INOUT_ARG:%[0-9]+]] = function_ref @_TF10properties9inout_arg
  // CHECK: apply [[INOUT_ARG]]([[XADDR]]#1)
}


/* TODO check writeback to more complex logical prop, check that writeback
 * reuses temporaries */

// CHECK-LABEL: sil hidden  @_TF10properties17val_subscript_get
// CHECK-NEXT: bb0([[VVAL:%[0-9]+]] : $Val, [[I:%[0-9]+]] : $Int):
func val_subscript_get(v: Val, i: Int) -> Float {
  return v[i]
  // CHECK: [[SUBSCRIPT_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Valg9subscript
  // CHECK: [[RET:%[0-9]+]] = apply [[SUBSCRIPT_GET_METHOD]]([[I]], [[VVAL]]) : $@convention(method) (Int, @guaranteed Val)
  // CHECK: return [[RET]]
}

// CHECK-LABEL: sil hidden  @_TF10properties17val_subscript_set
// CHECK: bb0(%0 : $Val, [[I:%[0-9]+]] : $Int, [[X:%[0-9]+]] : $Float):
func val_subscript_set(var v: Val, i: Int, x: Float) {
  v[i] = x
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  // CHECK: [[SUBSCRIPT_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties3Vals9subscript
  // CHECK: apply [[SUBSCRIPT_SET_METHOD]]([[X]], [[I]], [[VADDR]]#1)
}

struct Generic<T> {
  var mono_phys:Int
  var mono_log: Int { get {} set {} }
  var typevar_member:T

  subscript(x: Int) -> Float { get {} set {} }

  subscript(x: T) -> T { get {} set {} }

  // CHECK-LABEL: sil hidden  @_TFV10properties7Generic19copy_typevar_member
  mutating
  func copy_typevar_member(x: Generic<T>) {
    typevar_member = x.typevar_member
  }
}

// CHECK-LABEL: sil hidden  @_TF10properties21generic_mono_phys_get
func generic_mono_phys_get<T>(g: Generic<T>) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #Generic.mono_phys
}

// CHECK-LABEL: sil hidden  @_TF10properties20generic_mono_log_get
func generic_mono_log_get<T>(g: Generic<T>) -> Int {
  return g.mono_log
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg8mono_log
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties20generic_mono_log_set
func generic_mono_log_set<T>(var g: Generic<T>, x: Int) {
  g.mono_log = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Generics8mono_log
  // CHECK: apply [[GENERIC_SET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties26generic_mono_subscript_get
func generic_mono_subscript_get<T>(g: Generic<T>, i: Int) -> Float {
  return g[i]
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg9subscript
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @{{.*}}generic_mono_subscript_set
func generic_mono_subscript_set<T>(inout g: Generic<T>, i: Int, x: Float) {
  g[i] = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Generics9subscript
  // CHECK: apply [[GENERIC_SET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @{{.*}}bound_generic_mono_phys_get
func bound_generic_mono_phys_get(inout g: Generic<UnicodeScalar>, x: Int) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #Generic.mono_phys
}

// CHECK-LABEL: sil hidden  @_TF10properties26bound_generic_mono_log_get
func bound_generic_mono_log_get(g: Generic<UnicodeScalar>, x: Int) -> Int {
  return g.mono_log
// CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TFV10properties7Genericg8mono_log
  // CHECK: apply [[GENERIC_GET_METHOD]]<
}

// CHECK-LABEL: sil hidden  @_TF10properties22generic_subscript_type
func generic_subscript_type<T>(var g: Generic<T>, i: T, x: T) -> T {
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
// CHECK:   function_ref @_TZFV10properties14StaticPropertyg3foo{{.*}} : $@convention(thin) (@thin StaticProperty.Type) -> Int
func static_get() -> Int {
  return StaticProperty.foo
}

// CHECK-LABEL: sil hidden @_TF10properties10static_set
// CHECK:   function_ref @_TZFV10properties14StaticPropertys3foo{{.*}} : $@convention(thin) (Int, @thin StaticProperty.Type) -> ()
func static_set(x: Int) {
  StaticProperty.foo = x
}

func takeInt(a : Int) {}

protocol ForceAccessors {
  var a: Int { get set }
}

struct DidSetWillSetTests: ForceAccessors {
  var a: Int {
    willSet(newA) {
      // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.willset
      // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestsw1a
      // CHECK-NEXT: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug_value %0
      // CHECK-NEXT: [[SELFBOX:%.*]] = alloc_box $DidSetWillSetTests
      // CHECK-NEXT: copy_addr %1 to [initialization] [[SELFBOX]]#1 : $*DidSetWillSetTests

      takeInt(a)

      // CHECK-NEXT: // function_ref properties.takeInt
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: [[FIELDPTR:%.*]] = struct_element_addr [[SELFBOX]]#1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [[FIELDPTR]] : $*Int
      // CHECK-NEXT: apply [[TAKEINTFN]]([[A]]) : $@convention(thin) (Int) -> ()

      takeInt(newA)

      // CHECK-NEXT: // function_ref properties.takeInt (Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: apply [[TAKEINTFN]](%0) : $@convention(thin) (Int) -> ()
      // CHECK-NEXT: copy_addr [[SELFBOX]]#1 to %1 : $*DidSetWillSetTests
    }

    didSet {
      // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.didset
      // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestsW1a
      // CHECK-NEXT: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
      // CHECK-NEXT: debug
      // CHECK-NEXT: [[SELFBOX:%.*]] = alloc_box $DidSetWillSetTests
      // CHECK-NEXT: copy_addr %1 to [initialization] [[SELFBOX:%.*]]#1 : $*DidSetWillSetTests

      takeInt(a)

      // CHECK-NEXT: // function_ref properties.takeInt (Swift.Int) -> ()
      // CHECK-NEXT: [[TAKEINTFN:%.*]] = function_ref @_TF10properties7takeInt
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[SELFBOX:%.*]]#1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: [[A:%.*]] = load [[AADDR]] : $*Int
      // CHECK-NEXT: apply %5([[A]]) : $@convention(thin) (Int) -> ()

      a = zero  // reassign, but don't infinite loop.

      // CHECK-NEXT: // function_ref properties.zero.unsafeMutableAddressor : Swift.Int
      // CHECK-NEXT: [[ZEROFN:%.*]] = function_ref @_TF10propertiesau4zero
      // CHECK-NEXT: [[ZERORAW:%.*]] = apply [[ZEROFN]]() : $@convention(thin) () -> Builtin.RawPointer
      // CHECK-NEXT: [[ZEROADDR:%.*]] = pointer_to_address [[ZERORAW]] : $Builtin.RawPointer to $*Int
      // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[SELFBOX:%.*]]#1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
      // CHECK-NEXT: copy_addr [[ZEROADDR]] to [[AADDR]] : $*Int
      // CHECK-NEXT: copy_addr [[SELFBOX]]#1 to %1 : $*DidSetWillSetTests
    }
  }

  init(x : Int) {
    // Accesses to didset/willset variables are direct in init methods and dtors.
    a = x
    a = x
  }
  
   
  // These are the synthesized getter and setter for the willset/didset variable.

  // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.getter
  // CHECK-NEXT: sil hidden [transparent] @_TFV10properties18DidSetWillSetTestsg1a
  // CHECK-NEXT: bb0(%0 : $DidSetWillSetTests):
  // CHECK-NEXT:   debug_value %0
  // CHECK-NEXT:   %2 = struct_extract %0 : $DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT:   return %2 : $Int                      // id: %3
  
  // CHECK-LABEL: // {{.*}}.DidSetWillSetTests.a.setter
  // CHECK-NEXT: sil hidden @_TFV10properties18DidSetWillSetTestss1a
  // CHECK-NEXT: bb0(%0 : $Int, %1 : $*DidSetWillSetTests):
  // CHECK-NEXT:   debug_value %0
  // CHECK-NEXT: [[SELFBOX:%.*]] = alloc_box $DidSetWillSetTests
  // CHECK-NEXT: copy_addr %1 to [initialization] [[SELFBOX]]#1 : $*DidSetWillSetTests

  // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[SELFBOX:%.*]]#1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT: [[OLDVAL:%.*]] = load [[AADDR]] : $*Int
  // CHECK-NEXT: debug_value [[OLDVAL]] : $Int  // let tmp

  // CHECK-NEXT: // function_ref {{.*}}.DidSetWillSetTests.a.willset : Swift.Int
  // CHECK-NEXT: [[WILLSETFN:%.*]] = function_ref @_TFV10properties18DidSetWillSetTestsw1a
  // CHECK-NEXT:  apply [[WILLSETFN]](%0, [[SELFBOX]]#1) : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
  // CHECK-NEXT: [[AADDR:%.*]] = struct_element_addr [[SELFBOX:%.*]]#1 : $*DidSetWillSetTests, #DidSetWillSetTests.a
  // CHECK-NEXT: assign %0 to [[AADDR]] : $*Int
  // CHECK-NEXT: // function_ref {{.*}}.DidSetWillSetTests.a.didset : Swift.Int
  // CHECK-NEXT: [[DIDSETFN:%.*]] = function_ref @_TFV10properties18DidSetWillSetTestsW1a{{.*}} : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()
  // CHECK-NEXT: apply [[DIDSETFN]]([[OLDVAL]], [[SELFBOX]]#1) : $@convention(method) (Int, @inout DidSetWillSetTests) -> ()

  // CHECK-NEXT: copy_addr [[SELFBOX]]#1 to %1 : $*DidSetWillSetTests


  // CHECK-LABEL: sil hidden @_TFV10properties18DidSetWillSetTestsCfMS0_FT1x
  // CHECK-NEXT: bb0(%0 : $Int, %1 : $@thin DidSetWillSetTests.Type):
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
// CHECK-NEXT: bb0:
// CHECK-NEXT: %0 = global_addr @_Tv10properties25global_observing_propertySi : $*Int
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

func local_observing_property(arg: Int) {
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
// CHECK-NEXT: bb0([[ARG:%[0-9]+]] : $Int)
// CHECK: [[BOX:%[0-9]+]] = alloc_box $Int
// CHECK: store [[ARG]] to [[BOX]]#1




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
    // CHECK-LABEL: sil hidden @_TFC10properties19rdar16151899DerivedcfMS0_FT_S0_
    override init() {
        super.init()
        // CHECK: upcast {{.*}} : $rdar16151899Derived to $rdar16151899Base
        // CHECK-NEXT: function_ref properties.rdar16151899Base.init

        // This should not be a direct access, it should call the setter in the
        // base.
        x = zero
        
        // CHECK:  [[BASEPTR:%[0-9]+]] = upcast {{.*}} : $rdar16151899Derived to $rdar16151899Base
        // CHECK: load{{.*}}Int
        // CHECK-NEXT: [[SETTER:%[0-9]+]] = class_method {{.*}} : $rdar16151899Base, #rdar16151899Base.x!setter.1 : rdar16151899Base
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
// CHECK-NEXT: bb0(%0 : $Int, %1 : $Builtin.NativeObject, %2 : $*Int):
// CHECK-NEXT:  debug_value %0 : $Int
// CHECK-NEXT:  %4 = load %2 : $*Int
// CHECK-NEXT:  debug_value %4 : $Int
// CHECK-NEXT:  assign %0 to %2 : $*Int
// CHECK-NEXT:  strong_retain %1 : $Builtin.NativeObject
// CHECK-NEXT:  // function_ref
// CHECK-NEXT:  %8 = function_ref @_TFF10properties32propertyWithDidSetTakingOldValueFT_T_WL_1pSi : $@convention(thin) (Int, @owned Builtin.NativeObject, @inout Int) -> ()
// CHECK-NEXT:  %9 = apply %8(%4, %1, %2) : $@convention(thin) (Int, @owned Builtin.NativeObject, @inout Int) -> ()
// CHECK-NEXT:  strong_release %1 : $Builtin.NativeObject
// CHECK-NEXT:  %11 = tuple ()
// CHECK-NEXT:  return %11 : $()
// CHECK-NEXT:}


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

// CHECK: sil hidden @_TFC10properties15DerivedProperty24super_property_referencefS0_FT_
// CHECK-NEXT: bb0(%0 : $DerivedProperty):
// CHECK:  [[BASEPTR:%[0-9]+]] = upcast %0 : $DerivedProperty to $BaseProperty
// CHECK:  // function_ref properties.BaseProperty.x.getter
// CHECK:  [[FN:%[0-9]+]] = function_ref @_TFC10properties12BasePropertyg1x
// CHECK:  apply [[FN]]([[BASEPTR]]) : $@convention(method) (@guaranteed BaseProperty) -> Int // user: %7


// <rdar://problem/16411449> ownership qualifiers don't work with non-mutating struct property
struct ReferenceStorageTypeRValues {
  unowned var p1 : Ref

  func testRValueUnowned() -> Ref {
    return p1
  }
// CHECK: sil hidden @{{.*}}testRValueUnowned
// CHECK-NEXT: bb0(%0 : $ReferenceStorageTypeRValues):
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
func getX<T>(g: SomeGenericStruct<T>) -> Int {
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


func getX<T : ProtoWithClassProp>(a : T) -> Int {
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
func genericProps(x: GenericClass<String>) {
  // CHECK: class_method %0 : $GenericClass<String>, #GenericClass.x!getter.1
  let _ = x.x
  // CHECK: class_method %0 : $GenericClass<String>, #GenericClass.y!getter.1
  let _ = x.y
  // CHECK: [[Z:%.*]] = ref_element_addr %0 : $GenericClass<String>, #GenericClass.z
  // CHECK: load [[Z]] : $*String
  let _ = x.z
}

// CHECK-LABEL: sil hidden @_TF10properties28genericPropsInGenericContexturFGCS_12GenericClassq__T_
func genericPropsInGenericContext<U>(x: GenericClass<U>) {
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
// CHECK-LABEL: sil hidden @_TFC10properties20ClassWithLetProperty14ReturnConstantfS0_FT_Si
// CHECK-NEXT:  bb0(%0 : $ClassWithLetProperty):
// CHECK-NEXT:    debug_value
// CHECK-NEXT:    [[PTR:%[0-9]+]] = ref_element_addr %0 : $ClassWithLetProperty, #ClassWithLetProperty.p
// CHECK-NEXT:    [[VAL:%[0-9]+]] = load [[PTR]] : $*Int
// CHECK-NEXT:   return [[VAL]] : $Int


  // This property is marked dynamic, so go through the getter, always.
  func ReturnDynamicConstant() -> Int { return q }
// CHECK-LABEL: sil hidden @_TFC10properties20ClassWithLetProperty21ReturnDynamicConstantfS0_FT_Si
// CHECK: class_method [volatile] %0 : $ClassWithLetProperty, #ClassWithLetProperty.q!getter.1.foreign
}


// <rdar://problem/19254812> DI bug when referencing let member of a class
class r19254812Base {}
class r19254812Derived: r19254812Base{
  let pi = 3.14159265359
  
  init(x : ()) {
    use(pi)
  }
  
// Accessing the "pi" property should not retain/release self.
// CHECK-LABEL: sil hidden @_TFC10properties16r19254812DerivedcfMS0_FT1xT__S0_
// CHECK: [[SELFMUI:%[0-9]+]] = mark_uninitialized [derivedself] 

// Initialization of the pi field: no retains/releases.
// CHECK:  [[SELF:%[0-9]+]] = load [[SELFMUI]] : $*r19254812Derived
// CHECK-NEXT:  [[PIPTR:%[0-9]+]] = ref_element_addr [[SELF]] : $r19254812Derived, #r19254812Derived.pi
// CHECK-NEXT:  assign {{.*}} to [[PIPTR]] : $*Double

// CHECK-NOT: strong_release
// CHECK-NOT: strong_retain

// Load of the pi field: no retains/releases.
// CHECK:  [[SELF:%[0-9]+]] = load [[SELFMUI]] : $*r19254812Derived
// CHECK-NEXT:  [[PIPTR:%[0-9]+]] = ref_element_addr [[SELF]] : $r19254812Derived, #r19254812Derived.pi
// CHECK-NEXT:  {{.*}} = load [[PIPTR]] : $*Double
// CHECK: return
}


class RedundantSelfRetains {
  final var f : RedundantSelfRetains
  
  init() {
    f = RedundantSelfRetains()
  }
  
  // <rdar://problem/19275047> Extraneous retains/releases of self are bad
  func testMethod1() {
    f = RedundantSelfRetains()
  }
  // CHECK-LABEL: sil hidden @_TFC10properties20RedundantSelfRetains11testMethod1fS0_FT_T_
  // CHECK-NEXT: bb0(%0 : $RedundantSelfRetains):

  // CHECK-NOT: strong_retain
  
  // CHECK: [[FPTR:%[0-9]+]] = ref_element_addr %0 : $RedundantSelfRetains, #RedundantSelfRetains.f
  // CHECK-NEXT: assign {{.*}} to [[FPTR]] : $*RedundantSelfRetains

  // CHECK: return
}

class RedundantRetains {
  final var field = 0
}

func testRedundantRetains() {
  let a = RedundantRetains()
  a.field = 4  // no retain/release of a necessary here.
}

// CHECK-LABEL: sil hidden @_TF10properties20testRedundantRetainsFT_T_ : $@convention(thin) () -> () {
// CHECK: [[A:%[0-9]+]] = apply
// CHECK-NOT: strong_retain
// CHECK: strong_release [[A]] : $RedundantRetains
// CHECK-NOT: strong_retain
// CHECK-NOT: strong_release
// CHECK: return

struct AddressOnlyNonmutatingSet<T> {
  var x: T
  init(x: T) { self.x = x }
  var prop: Int {
    get { return 0 }
    nonmutating set { }
  }
}

func addressOnlyNonmutatingProperty<T>(x: AddressOnlyNonmutatingSet<T>)
-> Int {
  x.prop = 0
  return x.prop
}
// CHECK-LABEL: sil hidden @_TF10properties30addressOnlyNonmutatingPropertyurFGVS_25AddressOnlyNonmutatingSetq__Si : $@convention(thin) <T> (@in AddressOnlyNonmutatingSet<T>) -> Int {
// CHECK:         [[SET:%.*]] = function_ref @_TFV10properties25AddressOnlyNonmutatingSets4propSi
// CHECK:         apply [[SET]]<T>({{%.*}}, [[TMP:%.*]]#1)
// CHECK:         destroy_addr [[TMP]]
// CHECK:         dealloc_stack [[TMP]]
// CHECK:         [[GET:%.*]] = function_ref @_TFV10properties25AddressOnlyNonmutatingSetg4propSi
// CHECK:         apply [[GET]]<T>([[TMP:%.*]]#1)
// CHECK:         destroy_addr [[TMP]]
// CHECK:         dealloc_stack [[TMP]]

protocol MakeAddressOnly {}
struct AddressOnlyReadOnlySubscript {
  var x: MakeAddressOnly?

  subscript(z: Int) -> Int { return z }
}

// CHECK-LABEL: sil hidden @_TF10properties43addressOnlyReadOnlySubscriptFromMutableBaseFSiT_
// CHECK:         [[BASE:%.*]] = alloc_box $AddressOnlyReadOnlySubscript
// CHECK:         copy_addr [[BASE:%.*]]#1 to [initialization] [[COPY:%.*]]#1
// CHECK:         [[GETTER:%.*]] = function_ref @_TFV10properties28AddressOnlyReadOnlySubscriptg9subscriptFSiSi
// CHECK:         apply [[GETTER]]({{%.*}}, [[COPY]]#1)
func addressOnlyReadOnlySubscriptFromMutableBase(x: Int) {
  var base = AddressOnlyReadOnlySubscript()
  _ = base[x]
}



/// <rdar://problem/20912019> passing unmaterialized r-value as inout argument
struct MutatingGetterStruct {
  var write: Int {
    mutating get {  }
  }

  // CHECK-LABEL: sil hidden @_TZFV10properties20MutatingGetterStruct4testfMS0_FT_T_
  // CHECK: [[X:%.*]] = alloc_box $MutatingGetterStruct  // var x
  // CHECK: store {{.*}} to [[X]]#1 : $*MutatingGetterStruct
  // CHECK: apply {{%.*}}([[X]]#1) : $@convention(method) (@inout MutatingGetterStruct) -> Int
  static func test() {
    var x = MutatingGetterStruct()
    _ = x.write
  }
}

