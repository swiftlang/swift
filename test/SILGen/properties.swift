// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK: sil @_T10properties21physical_tuple_lvalueFT1cSi_T_
func physical_tuple_lvalue(c:Int) {
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box $Int64
  var x : (Int, Int)
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $(Int64, Int64)
  x.1 = c
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[X_1:%[0-9]+]] = tuple_element_addr [[XADDR]]#1 : {{.*}}, 1
  // CHECK: store [[C]] to [[X_1]]
}

func tuple_rvalue() -> (Int, Int) {}

// CHECK: sil @_T10properties21physical_tuple_rvalueFT_Si
func physical_tuple_rvalue() -> Int {
  return tuple_rvalue().1
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T10properties12tuple_rvalueFT_TSiSi_
  // CHECK: [[TUPLE:%[0-9]+]] = apply [[FUNC]]()
  // CHECK: [[RET:%[0-9]+]] = tuple_extract [[TUPLE]] : {{.*}}, 1
  // CHECK: return [[RET]]
}

// CHECK: sil @_T10properties16tuple_assignmentFT1aRSi1bRSi_T_
func tuple_assignment(a:[byref] Int, b:[byref] Int) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int64, [[B_ADDR:%[0-9]+]] : $*Int64):
  // CHECK: [[B:%[0-9]+]] = load [[B_ADDR]]
  // CHECK: [[A:%[0-9]+]] = load [[A_ADDR]]
  // CHECK: store [[B]] to [[A_ADDR]]
  // CHECK: store [[A]] to [[B_ADDR]]
  (a, b) = (b, a)
}

// CHECK: sil @_T10properties18tuple_assignment_2FT1aRSi1bRSi2xyTSiSi__T_
func tuple_assignment_2(a:[byref] Int, b:[byref] Int, xy:(Int, Int)) {
  // CHECK: bb0([[A_ADDR:%[0-9]+]] : $*Int64, [[B_ADDR:%[0-9]+]] : $*Int64, [[X:%[0-9]+]] : $Int64, [[Y:%[0-9]+]] : $Int64):
  // CHECK: [[XY_ADDR:%[0-9]+]] = alloc_box $(Int64, Int64)
  (a, b) = xy
  // CHECK: [[XY2:%[0-9]+]] = load [[XY_ADDR]]
  // CHECK: [[X:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 0
  // CHECK: [[Y:%[0-9]+]] = tuple_extract [[XY2]] : {{.*}}, 1
  // CHECK: store [[X]] to [[A_ADDR]]
  // CHECK: store [[Y]] to [[B_ADDR]]
}

class Ref {
  var x, y : Int
  var ref : Ref

  var z : Int { get: set: }

  var val_prop : Val { get: set: }

  subscript(i:Int) -> Float { get: set: }
}

class RefSubclass : Ref {
  var w : Int
}

struct Val {
  var x, y : Int
  var ref : Ref

  var z : Int { get: set: }

  var z_tuple : (Int, Int) { get: set: }

  subscript(i:Int) -> Float { get: set: }
}

// CHECK: sil @_T10properties22physical_struct_lvalueFT1cSi_T_
func physical_struct_lvalue(c:Int) {
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box $Int64
  var v : Val
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  v.y = c
  // CHECK: [[C:%[0-9]+]] = load [[CADDR]]
  // CHECK: [[X_1:%[0-9]+]] = struct_element_addr [[VADDR]]#1 : $*Val, #y
  // CHECK: store [[C]] to [[X_1]]
}

// CHECK: sil @_T10properties21physical_class_lvalueFT1rCS_3Ref1aSi_T_
func physical_class_lvalue(r:Ref, a:Int) {
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box $Ref
  // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Int64

  r.y = a
  // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
  // CHECK: [[R:%[0-9]+]] = load [[RADDR]]
  // CHECK: [[R_Y:%[0-9]+]] = ref_element_addr [[R]] : {{.*}}, #y
  // CHECK: store [[A]] to [[R_Y]]
}

// CHECK: sil @_T10properties24physical_subclass_lvalueFT1rCS_11RefSubclass1aSi_T_
func physical_subclass_lvalue(r:RefSubclass, a:Int) {
  // CHECK: [[RADDR:%[0-9]+]] = alloc_box $RefSubclass
  // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Int64

  r.y = a
  // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
  // CHECK: [[R:%[0-9]+]] = load [[RADDR]]
  // CHECK: [[R_SUP:%[0-9]+]] = upcast [[R]] : $RefSubclass to $Ref
  // CHECK: [[R_SUP_Y:%[0-9]+]] = ref_element_addr [[R_SUP]] : {{.*}}, #y
  // CHECK: store [[A]] to [[R_SUP_Y]]

  r.w = a
  // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
  // CHECK: [[R:%[0-9]+]] = load [[RADDR]]
  // CHECK: [[R_W:%[0-9]+]] = ref_element_addr [[R]] : {{.*}}, #w
  // CHECK: store [[A]] to [[R_W]]
}


func struct_rvalue() -> Val {}

// CHECK: sil @_T10properties22physical_struct_rvalueFT_Si
func physical_struct_rvalue() -> Int {
  return struct_rvalue().y
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T10properties13struct_rvalueFT_VS_3Val
  // CHECK: [[STRUCT:%[0-9]+]] = apply [[FUNC]]()
  // CHECK: [[STRUCT_TEMP:%[0-9]+]] = alloc_stack $Val
  // CHECK: store [[STRUCT]] to [[STRUCT_TEMP]]#1
  // CHECK: [[RET_ADDR:%[0-9]+]] = struct_element_addr [[STRUCT_TEMP]]#1 : $*Val, #y
  // CHECK: [[RET:%[0-9]+]] = load [[RET_ADDR]]
  // CHECK: dealloc_stack [[STRUCT_TEMP]]#0
  // CHECK: return [[RET]]
}

func class_rvalue() -> Ref {}

// CHECK: sil @_T10properties21physical_class_rvalueFT_Si
func physical_class_rvalue() -> Int {
  return class_rvalue().y
  // CHECK: [[FUNC:%[0-9]+]] = function_ref @_T10properties12class_rvalueFT_CS_3Ref
  // CHECK: [[CLASS:%[0-9]+]] = apply [[FUNC]]()
  // CHECK: [[RET_ADDR:%[0-9]+]] = ref_element_addr [[CLASS]] : {{.*}}, #y
  // CHECK: [[RET:%[0-9]+]] = load [[RET_ADDR]]
  // CHECK: return [[RET]]
}

// CHECK: sil @_T10properties18logical_struct_getFT_Si
func logical_struct_get() -> Int {
  return struct_rvalue().z
  // CHECK: [[STRUCT_TEMP:%[0-9]+]] = alloc_stack $Val
  // CHECK: [[GET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val1zSig
  // CHECK: [[VALUE:%[0-9]+]] = apply [[GET_METHOD]]([[STRUCT_TEMP]]#1)
  // CHECK: dealloc_stack [[STRUCT_TEMP]]#0
  // CHECK: return [[VALUE]]
}

// CHECK: sil @_T10properties18logical_struct_setFT3valRVS_3Val1zSi_T_
func logical_struct_set(val:[byref] Val, z:Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[ZARG:%[0-9]+]] : $Int64):
  val.z = z
  // CHECK: [[ZADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Z:%[0-9]+]] = load [[ZADDR]]
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val1zSis
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL]])
  // CHECK: return
}

// CHECK: sil @_T10properties27logical_struct_in_tuple_setFT3valRTSiVS_3Val_1zSi_T_
func logical_struct_in_tuple_set(val:[byref] (Int, Val), z:Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*(Int64, Val), [[ZARG:%[0-9]+]] : $Int64):
  val.1.z = z
  // CHECK: [[ZADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Z:%[0-9]+]] = load [[ZADDR]]
  // CHECK: [[VAL_1:%[0-9]+]] = tuple_element_addr [[VAL]] : {{.*}}, 1
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val1zSis
  // CHECK: apply [[Z_SET_METHOD]]([[Z]], [[VAL_1]])
  // CHECK: return
}

// CHECK: sil @_T10properties29logical_struct_in_reftype_setFT3valRVS_3Val2z1Si_T_
func logical_struct_in_reftype_set(val:[byref] Val, z1:Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, %{{[0-9]+}} : $Int64):
  val.ref.val_prop.z_tuple.1 = z1
  // CHECK: [[Z1ADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Z1:%[0-9]+]] = load [[Z1ADDR]]
  // -- val.ref
  // CHECK: [[VAL_REF_ADDR:%[0-9]+]] = struct_element_addr [[VAL]] : $*Val, #ref
  // CHECK: [[VAL_REF:%[0-9]+]] = load [[VAL_REF_ADDR]]
  // -- getters and setters
  // -- val.ref.val_prop
  // CHECK: [[GET_VAL_PROP_METHOD:%[0-9]+]] = function_ref @_TC10properties3Ref8val_propVS_3Valg
  // CHECK: [[VAL_REF_VAL_PROP:%[0-9]+]] = apply [[GET_VAL_PROP_METHOD]]([[VAL_REF]])
  // CHECK: [[VAL_REF_VAL_PROP_MAT:%[0-9]+]] = alloc_stack $Val
  // CHECK: store [[VAL_REF_VAL_PROP]] to [[VAL_REF_VAL_PROP_MAT]]#1
  // -- val.ref.val_prop.z_tuple
  // CHECK: [[GET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val7z_tupleTSiSi_g
  // CHECK: [[V_R_VP_Z_TUPLE:%[0-9]+]] = apply [[GET_Z_TUPLE_METHOD]]([[VAL_REF_VAL_PROP_MAT]]#1)
  // CHECK: [[V_R_VP_Z_TUPLE_MAT:%[0-9]+]] = alloc_stack $(Int64, Int64)
  // CHECK: store [[V_R_VP_Z_TUPLE]] to [[V_R_VP_Z_TUPLE_MAT]]#1
  // -- write to val.ref.val_prop.z_tuple.1
  // CHECK: [[V_R_VP_Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[V_R_VP_Z_TUPLE_MAT]]#1 : {{.*}}, 1
  // CHECK: store [[Z1]] to [[V_R_VP_Z_TUPLE_1]]
  // -- writeback to val.ref.val_prop.z_tuple
  // CHECK: [[WB_V_R_VP_Z_TUPLE:%[0-9]+]] = load [[V_R_VP_Z_TUPLE_MAT]]#1
  // CHECK: [[SET_Z_TUPLE_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val7z_tupleTSiSi_s
  // CHECK: apply [[SET_Z_TUPLE_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL_REF_VAL_PROP_MAT]]#1)
  // -- writeback to val.ref.val_prop
  // CHECK: [[WB_VAL_REF_VAL_PROP:%[0-9]+]] = load [[VAL_REF_VAL_PROP_MAT]]#1
  // CHECK: [[SET_VAL_PROP_METHOD:%[0-9]+]] = function_ref @_TC10properties3Ref8val_propVS_3Vals
  // CHECK: apply [[SET_VAL_PROP_METHOD]]([[WB_VAL_REF_VAL_PROP]], [[VAL_REF]])
  // -- cleanup
  // CHECK: dealloc_stack [[V_R_VP_Z_TUPLE_MAT]]#0
  // CHECK: dealloc_stack [[VAL_REF_VAL_PROP_MAT]]#0
  // -- don't need to write back to val.ref because it's a ref type
}

func reftype_rvalue() -> Ref {}

// CHECK: sil @_T10properties18reftype_rvalue_setFT3valVS_3Val_T_
func reftype_rvalue_set(val:Val) {
  // CHECK: [[VALADDR:%[0-9]+]] = alloc_box $Val
  reftype_rvalue().val_prop = val
}

// CHECK: sil @_T10properties27tuple_in_logical_struct_setFT3valRVS_3Val2z1Si_T_
func tuple_in_logical_struct_set(val:[byref] Val, z1:Int) {
  // CHECK: bb0([[VAL:%[0-9]+]] : $*Val, [[Z1ARG:%[0-9]+]] : $Int64):
  val.z_tuple.1 = z1
  // CHECK: [[Z1ADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[Z1:%[0-9]+]] = load [[ZADDR]]
  // CHECK: [[Z_GET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val7z_tupleTSiSi_g
  // CHECK: [[Z_TUPLE:%[0-9]+]] = apply [[Z_GET_METHOD]]([[VAL]])
  // CHECK: [[Z_TUPLE_MATERIALIZED:%[0-9]+]] = alloc_stack $(Int64, Int64)
  // CHECK: store [[Z_TUPLE]] to [[Z_TUPLE_MATERIALIZED]]#1
  // CHECK: [[Z_TUPLE_1:%[0-9]+]] = tuple_element_addr [[Z_TUPLE_MATERIALIZED]]#1 : {{.*}}, 1
  // CHECK: store [[Z1]] to [[Z_TUPLE_1]]
  // CHECK: [[Z_TUPLE_MODIFIED:%[0-9]+]] = load [[Z_TUPLE_MATERIALIZED]]#1
  // CHECK: [[Z_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val7z_tupleTSiSi_s
  // CHECK: apply [[Z_SET_METHOD]]({{%[0-9]+, %[0-9]+}}, [[VAL]])
  // CHECK: dealloc_stack [[Z_TUPLE_MATERIALIZED]]#0
  // CHECK: return
}

var global_prop : Int {
  // CHECK: sil @_T10properties11global_propSig
  get: return 219
  // CHECK: sil @_T10properties11global_propSis
  set: println(value)
}

// CHECK: sil @_T10properties18logical_global_getFT_Si
func logical_global_get() -> Int {
  return global_prop
  // CHECK: [[GET:%[0-9]+]] = function_ref @_T10properties11global_propSig
  // CHECK: [[VALUE:%[0-9]+]] = apply [[GET]]()
  // CHECK: return [[VALUE]]
}

// CHECK: sil @_T10properties18logical_global_setFT1xSi_T_
func logical_global_set(x:Int) {
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
  global_prop = x
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[SET:%[0-9]+]] = function_ref @_T10properties11global_propSis
  // CHECK: apply [[SET]]([[X]])
}

// CHECK: sil @_T10properties17logical_local_getFT1xSi_Si
func logical_local_get(x:Int) -> Int {
  var prop : Int {
    get: return x + 219
  }
  // CHECK: [[GET_REF:%[0-9]+]] = function_ref @_TL10properties17logical_local_getFT1xSi_Si4propSig
  // CHECK: [[GET_CLOSURE:%[0-9]+]] = partial_apply [[GET_REF]]({{.*}})
  // CHECK: apply [[GET_CLOSURE]]()
  return prop
}
// CHECK: sil internal @_TL10properties17logical_local_getFT1xSi_Si4propSig
// CHECK: bb0(%{{[0-9]+}} : $Builtin.ObjectPointer, %{{[0-9]+}} : $*Int64):

// CHECK: sil @_T10properties26logical_local_captured_getFT1xSi_Si
func logical_local_captured_get(x:Int) -> Int {
  var prop : Int {
    get: return x + 219
  }
  // CHECK: [[GET_REF:%[0-9]+]] = function_ref @_TL10properties26logical_local_captured_getFT1xSi_Si4propSig
  // CHECK: [[GET_CLOSURE:%[0-9]+]] = partial_apply [[GET_REF]]({{.*}})

  func get_prop() -> Int {
    return prop
  }
  // CHECK: [[FUNC_REF:%[0-9]+]] = function_ref @{{closure[0-9]*}}
  // CHECK: [[FUNC_CLOSURE:%[0-9]+]] = partial_apply [[FUNC_REF]]([[GET_CLOSURE]])

  return get_prop()
  // CHECK: apply [[FUNC_CLOSURE]]()
}
// CHECK: sil internal @_TL10properties26logical_local_captured_getFT1xSi_Si4propSig
// CHECK: bb0(%{{[0-9]+}} : $Builtin.ObjectPointer, %{{[0-9]+}} : $*Int64):

func byref_arg(x:[byref] Int) {}

// CHECK: sil @_T10properties14physical_byrefFT1xSi_T_
func physical_byref(x:Int) {
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
  byref_arg(&x)
  // CHECK: [[BYREF_ARG:%[0-9]+]] = function_ref @_T10properties9byref_argFT1xRSi_T_
  // CHECK: apply [[BYREF_ARG]]([[XADDR]]#1)
}


/* TODO check writeback to more complex logical prop, check that writeback
 * reuses temporaries */

// CHECK: sil @_T10properties17val_subscript_getFT1vVS_3Val1iSi_Sf
func val_subscript_get(v:Val, i:Int) -> Float {
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int64

  return v[i]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[SUBSCRIPT_GET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val11__subscriptFT1iSi_Sfg
  // CHECK: [[RET:%[0-9]+]] = apply [[SUBSCRIPT_GET_METHOD]]([[I]], [[VADDR]]#1)
  // CHECK: return [[RET]]
}

// CHECK: sil @_T10properties17val_subscript_setFT1vVS_3Val1iSi1xSf_T_
func val_subscript_set(v:Val, i:Int, x:Float) {
  v[i] = x
  // CHECK: [[VADDR:%[0-9]+]] = alloc_box $Val
  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Float
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[I:%[0-9]+]] = load [[IADDR]]
  // CHECK: [[SUBSCRIPT_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties3Val11__subscriptFT1iSi_Sfs
  // CHECK: apply [[SUBSCRIPT_SET_METHOD]]([[X]], [[I]], [[VADDR]]#1)
}

struct Generic<T> {
  var mono_phys:Int
  var mono_log:Int { get: set: }
  var typevar_member:T

  subscript(x:Int) -> Float { get: set: }

  subscript(x:T) -> T { get: set: }

  // CHECK: sil @_TV10properties7Generic19copy_typevar_memberU__fRGS0_Q__FT1xGS0_Q___T_
  func copy_typevar_member(x:Generic<T>) {
    typevar_member = x.typevar_member
  }
}

// CHECK: sil @_T10properties21generic_mono_phys_getU__FT1gGVS_7GenericQ___Si
func generic_mono_phys_get<T>(g:Generic<T>) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #mono_phys
}

// CHECK: sil @_T10properties20generic_mono_log_getU__FT1gGVS_7GenericQ___Si
func generic_mono_log_get<T>(g:Generic<T>) -> Int {
  return g.mono_log
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TV10properties7Generic8mono_logSig : $[cc(method), thin] <T> ((), [byref] Generic<T>) -> Int64
  // CHECK: [[SPECIAL_GET_METHOD:%[0-9]+]] = specialize [[GENERIC_GET_METHOD]] : {{.*}}, $[cc(method), thin] {{[^<].*}}
  // CHECK: apply [[SPECIAL_GET_METHOD]](
}

// CHECK: sil @_T10properties20generic_mono_log_setU__FT1gGVS_7GenericQ__1xSi_T_
func generic_mono_log_set<T>(g:Generic<T>, x:Int) {
  g.mono_log = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties7Generic8mono_logSis : $[cc(method), thin] <{{.*}}
  // CHECK: [[SPECIAL_SET_METHOD:%[0-9]+]] = specialize [[GENERIC_SET_METHOD]] : {{.*}}, $[cc(method), thin] {{[^<].*}}
  // CHECK: apply [[SPECIAL_SET_METHOD]](
}

// CHECK: sil @_T10properties26generic_mono_subscript_getU__FT1gGVS_7GenericQ__1iSi_Sf
func generic_mono_subscript_get<T>(g:Generic<T>, i:Int) -> Float {
  return g[i]
  // CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TV10properties7Generic11__subscriptFT1xSi_Sfg : $[cc(method), thin] {{.*}}
  // CHECK: [[SPECIAL_GET_METHOD:%[0-9]+]] = specialize [[GENERIC_GET_METHOD]] : {{.*}}, $[cc(method), thin] {{[^<].*}}
  // CHECK: apply [[SPECIAL_GET_METHOD]](
}

// CHECK: sil @_T10properties26generic_mono_subscript_setU__FT1gGVS_7GenericQ__1iSi1xSf_T_
func generic_mono_subscript_set<T>(g:Generic<T>, i:Int, x:Float) {
  g[i] = x
  // CHECK: [[GENERIC_SET_METHOD:%[0-9]+]] = function_ref @_TV10properties7Generic11__subscriptFT1xSi_Sfs : $[cc(method), thin] <{{.*}}
  // CHECK: [[SPECIAL_SET_METHOD:%[0-9]+]] = specialize [[GENERIC_SET_METHOD]] : {{.*}}, $[cc(method), thin] {{[^<].*}}
  // CHECK: apply [[SPECIAL_SET_METHOD]](
}

// CHECK: sil @_T10properties27bound_generic_mono_phys_getFT1gGVS_7GenericSc_1xSi_Si
func bound_generic_mono_phys_get(g:Generic<Char>, x:Int) -> Int {
  return g.mono_phys
  // CHECK: struct_element_addr %{{.*}}, #mono_phys
}

// CHECK: sil @_T10properties26bound_generic_mono_log_getFT1gGVS_7GenericSc_1xSi_Si
func bound_generic_mono_log_get(g:Generic<Char>, x:Int) -> Int {
  return g.mono_log
// CHECK: [[GENERIC_GET_METHOD:%[0-9]+]] = function_ref @_TV10properties7Generic8mono_logSig : $[cc(method), thin] <{{.*}}
  // CHECK: [[SPECIAL_GET_METHOD:%[0-9]+]] = specialize [[GENERIC_GET_METHOD]] : {{.*}}, $[cc(method), thin] {{[^<].*}}
  // CHECK: apply [[SPECIAL_GET_METHOD]](
}

// CHECK: sil @_T10properties22generic_subscript_typeU__FT1gGVS_7GenericQ__1iQ_1xQ__Q_
func generic_subscript_type<T>(g:Generic<T>, i:T, x:T) -> T {
  g[i] = x
  return g[i]
}

/*TODO: archetype and existential properties and subscripts */
