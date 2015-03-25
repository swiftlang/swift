// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

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
// CHECK-LABEL: sil hidden @_TFV19default_constructor1DCfMS0_FT_S0_ : $@thin (@thin D.Type) -> D
// CHECK: [[THISBOX:%[0-9]+]] = alloc_box $D
// CHECK: [[THIS:%[0-9]+]] = mark_uninit
// CHECK: [[INTCONV:%[0-9]+]] = function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
// CHECK: [[INTMETA:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 2
// CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCONV]]([[INTLIT]], [[INTMETA]])
// CHECK: [[FLOATCONV:%[0-9]+]] = function_ref @_TFSdCfMSdFT20_builtinFloatLiteralBf{{64|80}}__Sd
// CHECK: [[FLOATMETA:%[0-9]+]] = metatype $@thin Double.Type
// CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x400C000000000000|0x4000E000000000000000}}
// CHECK: [[FLOATVAL:%[0-9]+]] = apply [[FLOATCONV]]([[FLOATLIT]], [[FLOATMETA]])
// CHECK: [[IADDR:%[0-9]+]] = struct_element_addr [[THIS]] : $*D, #D.i
// CHECK: assign [[INTVAL]] to [[IADDR]]
// CHECK: [[JADDR:%[0-9]+]] = struct_element_addr [[THIS]] : $*D, #D.j
// CHECK: assign [[FLOATVAL]] to [[JADDR]]

class E {
  var i = Int64()
}

// CHECK-LABEL: sil hidden @_TFC19default_constructor1EcfMS0_FT_S0_ : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: bb0([[SELFIN:%[0-9]+]] : $E)
// CHECK: [[SELF:%[0-9]+]] = mark_uninitialized
// CHECK: [[INT64_CTOR:%[0-9]+]] = function_ref @_TFVSs5Int64CfMS_FT_S_ : $@thin (@thin Int64.Type) -> Int64
// CHECK-NEXT: [[INT64:%[0-9]+]] = metatype $@thin Int64.Type
// CHECK-NEXT: [[ZERO:%[0-9]+]] = apply [[INT64_CTOR]]([[INT64]]) : $@thin (@thin Int64.Type) -> Int64
// CHECK-NEXT: [[IREF:%[0-9]+]] = ref_element_addr [[SELF]] : $E, #E.i
// CHECK-NEXT: assign [[ZERO]] to [[IREF]] : $*Int64
// CHECK-NEXT: return [[SELF]] : $E

class F : E { }

// CHECK-LABEL: sil hidden @_TFC19default_constructor1FcfMS0_FT_S0_ : $@cc(method) @thin (@owned F) -> @owned F
// CHECK-NEXT: bb0([[ORIGSELF:%[0-9]+]] : $F)
// CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box $F
// CHECK-NEXT: [[SELF:%[0-9]+]] = mark_uninitialized [derivedself]
// CHECK-NEXT: store [[ORIGSELF]] to [[SELF]] : $*F
// CHECK-NEXT: [[SELFP:%[0-9]+]] = load [[SELF]] : $*F
// CHECK-NEXT: [[E:%[0-9]]] = upcast [[SELFP]] : $F to $E
// CHECK: [[E_CTOR:%[0-9]+]] = function_ref @_TFC19default_constructor1EcfMS0_FT_S0_ : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: [[ESELF:%[0-9]]] = apply [[E_CTOR]]([[E]]) : $@cc(method) @thin (@owned E) -> @owned E
// CHECK-NEXT: [[ESELFW:%[0-9]+]] = unchecked_ref_cast [[ESELF]] : $E to $F
// CHECK-NEXT: store [[ESELFW]] to [[SELF]] : $*F
// CHECK-NEXT: [[SELFP:%[0-9]+]] = load [[SELF]] : $*F
// CHECK-NEXT: strong_retain [[SELFP]] : $F
// CHECK-NEXT: strong_release [[SELF_BOX]]#0 : $Builtin.NativeObject
// CHECK-NEXT: return [[SELFP]] : $F


// <rdar://problem/19780343> Default constructor for a struct with optional doesn't compile

// This shouldn't get a default init, since it would be pointless (bar can never
// be reassigned).  It should get a memberwise init though.
struct G {
  let bar: Int32?
}

// CHECK-NOT: default_constructor.G.init (default_constructor.G.Type)()
// CHECK-LABEL: default_constructor.G.init (default_constructor.G.Type)(bar : Swift.Optional<Swift.Int32>)
// CHECK-NEXT: sil hidden @_TFV19default_constructor1GCfMS0_FT3barGSqVSs5Int32__S0_
// CHECK-NOT: default_constructor.G.init (default_constructor.G.Type)()

