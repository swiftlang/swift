// RUN: %swift -emit-sil %s | FileCheck %s

struct B {
  var i : Int, j : Float
  var c : C
}

struct C {
  var x : Int
  constructor() { x = 17 }
}

// CHECK: sil @_TV19default_constructor1BCfMS0_FT_S0_ : $[thin] ((), B.metatype) -> B {
// CHECK: bb0([[metaThis:%[0-9]+]] : $B.metatype):
// CHECK: [[this:%[0-9]+]] = alloc_var stack $B
// Initialize i
// CHECK: [[Int64Ctor:%[0-9]+]] = function_ref @_TSiCfMSiFT_Si : $[thin] ((), Int64.metatype) -> Int64
// CHECK: [[Int64Meta:%[0-9]+]] = metatype $Int64.metatype
// CHECK: [[Int64Construct:%[0-9]+]] = apply [[Int64Ctor]]([[Int64Meta]])
// CHECK: [[i:%[0-9]+]] = struct_element_addr [[this]], @i
// CHECK: store [[Int64Construct]] to [[i]]
// Initialize j
// CHECK: [[Float32Ctor:%[0-9]+]] = function_ref @_TSfCfMSfFT_Sf : $[thin] ((), Float32.metatype) -> Float32
// CHECK: [[Float32Meta:%[0-9]+]] = metatype $Float32.metatype
// CHECK: [[Float32Construct:%[0-9]+]] = apply [[Float32Ctor]]([[Float32Meta]])
// CHECK: [[i:%[0-9]+]] = struct_element_addr [[this]], @j
// CHECK: store [[Float32Construct]] to [[i]]
// Initialize c
// CHECK: [[CCtor:%[0-9]+]] = function_ref @_TV19default_constructor1CCfMS0_FT_S0_ : $[thin] ((), C.metatype) -> C
// CHECK: [[CMeta:%[0-9]+]] = metatype $C.metatype
// CHECK: [[CConstruct:%[0-9]+]] = apply [[CCtor]]([[CMeta]])
// CHECK: [[i:%[0-9]+]] = struct_element_addr [[this]], @c
// CHECK: store [[CConstruct]] to [[i]]

struct D {
  var (i, j) : (Int, Double) = (2, 3.5)
}
// CHECK: sil @_TV19default_constructor1DCfMS0_FT_S0_ : $[thin] ((), D.metatype) -> D
// CHECK: [[THIS:%[0-9]+]] = alloc_var stack $D
// CHECK: [[INTCONV:%[0-9]+]] = function_ref @_TSi33_convertFromBuiltinIntegerLiteralfMSiFT3valBi128__Si : $[thin] ((val : Builtin.Int128), Int64.metatype) -> Int64
// CHECK: [[INTMETA:%[0-9]+]] = metatype $Int64.metatype
// CHECK: [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int128, 2
// CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCONV]]([[INTLIT]], [[INTMETA]])
// CHECK: [[FLOATCONV:%[0-9]+]] = function_ref @_TSd31_convertFromBuiltinFloatLiteralfMSdFT5valueBf64__Sd : $[thin] ((value : Builtin.FP_IEEE64), Float64.metatype) -> Float64
// CHECK: [[FLOATMETA:%[0-9]+]] = metatype $Float64.metatype
// CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FP_IEEE64, 3.5
// CHECK: [[FLOATVAL:%[0-9]+]] = apply [[FLOATCONV]]([[FLOATLIT]], [[FLOATMETA]])
// CHECK: [[IADDR:%[0-9]+]] = struct_element_addr [[THIS]], @i
// CHECK: store [[INTVAL]] to [[IADDR]]
// CHECK: [[JADDR:%[0-9]+]] = struct_element_addr [[THIS]], @j
// CHECK: store [[FLOATVAL]] to [[JADDR]]
