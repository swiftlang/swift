// RUN: %swift -emit-silgen %s | FileCheck %s

struct B {
  var i : Int, j : Float
  var c : C
}

struct C {
  var x : Int
  init() { x = 17 }
}

struct D {
  var (i, j) : (Int, Double) = (2, 3.5)
}
// CHECK-LABEL: sil  @_TFV19default_constructor1DCfMS0_FT_S0_ : $@thin (@thin D.Type) -> D
// CHECK: [[THISBOX:%[0-9]+]] = alloc_box $D
// CHECK: [[THIS:%[0-9]+]] = mark_uninit
// CHECK: [[INTCONV:%[0-9]+]] = function_ref @_TFSi33_convertFromBuiltinIntegerLiteralfMSiFBi2048_Si : $@thin (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK: [[INTMETA:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 2
// CHECK: [[INTVAL:%[0-9]+]] = apply [transparent] [[INTCONV]]([[INTLIT]], [[INTMETA]])
// CHECK: [[FLOATCONV:%[0-9]+]] = function_ref @_TFSd31_convertFromBuiltinFloatLiteralfMSdFBf64_Sd : $@thin (Builtin.FPIEEE64, @thin Double.Type) -> Double
// CHECK: [[FLOATMETA:%[0-9]+]] = metatype $@thin Double.Type
// CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FPIEEE64, 0x400C000000000000
// CHECK: [[FLOATVAL:%[0-9]+]] = apply [transparent] [[FLOATCONV]]([[FLOATLIT]], [[FLOATMETA]])
// CHECK: [[IADDR:%[0-9]+]] = struct_element_addr [[THIS]] : $*D, #D.i
// CHECK: assign [[INTVAL]] to [[IADDR]]
// CHECK: [[JADDR:%[0-9]+]] = struct_element_addr [[THIS]] : $*D, #D.j
// CHECK: assign [[FLOATVAL]] to [[JADDR]]

class E {
  var i = Int64()
}

// CHECK-LABEL: sil @_TFC19default_constructor1EcfMS0_FT_S0_ : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: bb0([[SELFIN:%[0-9]+]] : $E)
// CHECK: [[SELF:%[0-9]+]] = mark_uninitialized
// CHECK: [[INT64_CTOR:%[0-9]+]] = function_ref @_TFVSs5Int64CfMS_FT_S_ : $@thin (@thin Int64.Type) -> Int64
// CHECK-NEXT: [[INT64:%[0-9]+]] = metatype $@thin Int64.Type
// CHECK-NEXT: [[ZERO:%[0-9]+]] = apply [transparent] [[INT64_CTOR]]([[INT64]]) : $@thin (@thin Int64.Type) -> Int64
// CHECK-NEXT: [[IREF:%[0-9]+]] = ref_element_addr [[SELF]] : $E, #E.i
// CHECK-NEXT: assign [[ZERO]] to [[IREF]] : $*Int64
// CHECK-NEXT: return [[SELF]] : $E

class F : E { }

// CHECK-LABEL: sil @_TFC19default_constructor1FcfMS0_FT_S0_ : $@cc(method) @thin (@owned F) -> @owned F
// CHECK-NEXT: bb0([[SELF:%[0-9]+]] : $F)
// CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $F
// CHECK-NEXT: [[SELF:%[0-9]+]] = mark_uninitialized [derivedself]
// CHECK-NEXT: store [[SELF]] to [[SELF_BOX]]#1 : $*F
// CHECK-NEXT: [[SELF:%[0-9]+]] = load [[SELF_BOX]]#1 : $*F
// CHECK-NEXT: strong_retain [[SELF]] : $F
// CHECK-NEXT: [[E:%[0-9]]] = upcast [[SELF]] : $F to $E
// CHECK: [[E_CTOR:%[0-9]+]] = function_ref @_TFC19default_constructor1EcfMS0_FT_S0_ : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: [[ESELF:%[0-9]]] = apply [[E_CTOR]]([[E]]) : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: [[ESELFW:%[0-9]+]] = unchecked_ref_cast [[ESELF]] : $E to $F
// CHECK-NEXT: assign [[ESELFW]] to [[SELF_BOX]]#1 : $*F
// CHECK-NEXT: [[SELF:%[0-9]+]] = load [[SELF_BOX]]#1 : $*F
// CHECK-NEXT: strong_retain [[SELF]] : $F
// CHECK-NEXT: strong_release [[SELF_BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return [[SELF]] : $F
