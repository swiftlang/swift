// RUN: %swift -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK: sil @_T5decls11void_returnFT_T_
// CHECK: = tuple
// CHECK: return
func void_return() {
}

// CHECK: sil @_T5decls14typealias_declFT_T_
func typealias_decl() {
  typealias a = Int
}

// CHECK: sil @_T5decls15simple_patternsFT_T_
func simple_patterns() {
  var _ = 4
  var _ : Int
}

// CHECK: sil @_T5decls13named_patternFT_Si
func named_pattern() -> Int {
  var local_var : Int = 4

  var defaulted_var : Int  // Defaults to zero initialization

  return local_var + defaulted_var
}

func MRV() -> (Int, Float, (), Double) {}

// CHECK: sil @_T5decls14tuple_patternsFT_T_
func tuple_patterns() {
  var (a, b) : (Int, Float)
  // CHECK: [[AADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[BADDR:%[0-9]+]] = alloc_box $Float32
  // CHECK: [[INTCTOR:%[0-9]+]] = function_ref @_TSiCfMSiFT_Si : $[thin] ((), Int64.metatype) -> Int64
  // CHECK: [[INTMETA:%[0-9]+]] = metatype $Int64.metatype
  // CHECK: [[INTVAL:%[0-9]+]] = apply [[INTCTOR]]([[INTMETA]])
  // CHECK: store [[INTVAL]] to [[AADDR]]
  // CHECK: [[FLOATCTOR:%[0-9]+]] = function_ref @_TSfCfMSfFT_Sf : $[thin] ((), Float32.metatype) -> Float32
  // CHECK: [[FLOATMETA:%[0-9]+]] = metatype $Float32.metatype
  // CHECK: [[FLOATVAL:%[0-9]+]] = apply [[FLOATCTOR]]([[FLOATMETA]])
  // CHECK: store [[FLOATVAL]] to [[BADDR]]

  var (c, d) = (a, b)
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[DADDR:%[0-9]+]] = alloc_box $Float32
  // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
  // CHECK: store [[A]] to [[CADDR]]
  // CHECK: [[B:%[0-9]+]] = load [[BADDR]]
  // CHECK: store [[B]] to [[DADDR]]

  // CHECK: [[EADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[FADDR:%[0-9]+]] = alloc_box $Float32
  // CHECK: [[GADDR:%[0-9]+]] = alloc_box $()
  // CHECK: [[HADDR:%[0-9]+]] = alloc_box $Float64
  // CHECK: [[EFGH:%[0-9]+]] = apply
  // CHECK: [[E:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[F:%[0-9]+]] = tuple_extract {{.*}}, 1
  // CHECK: [[G:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: [[H:%[0-9]+]] = tuple_extract {{.*}}, 3
  // CHECK: store [[E]] to [[EADDR]]
  // CHECK: store [[F]] to [[FADDR]]
  // CHECK: store [[H]] to [[HADDR]]
  var (e,f,g,h) = MRV()

  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK-NOT: alloc_box $Float32
  // CHECK: [[A:%[0-9]+]] = load [[AADDR]]
  // CHECK: store [[A]] to [[IADDR]]
  // CHECK: [[B:%[0-9]+]] = load [[BADDR]]
  // CHECK-NOT: store [[B]]
  var (i,_) = (a, b)

  // CHECK: [[JADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK-NOT: alloc_box $Float32
  // CHECK: [[KADDR:%[0-9]+]] = alloc_box $()
  // CHECK-NOT: alloc_box $Float64
  // CHECK: [[J_K_:%[0-9]+]] = apply
  // CHECK: [[J:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[K:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: store [[J]] to [[JADDR]]
  var (j,_,k,_) = MRV()
}

// CHECK: sil @_T5decls16simple_argumentsFT1xSi1ySi_Si
// CHECK: bb0(%0 : $Int64, %1 : $Int64):
// CHECK: [[X:%[0-9]+]] = alloc_box $Int64
// CHECK-NEXT: [[Y:%[0-9]+]] = alloc_box $Int64
// CHECK-NEXT: store %0 to [[X]]
// CHECK-NEXT: store %1 to [[Y]]
func simple_arguments(x : Int64, y : Int64) -> Int {
  return x+y
}

// CHECK: sil @_T5decls17curried_argumentsfT1xSi_FT1ySi_Si
// CHECK: bb0(%0 : $Int64, %1 : $Int64):
// CHECK: [[X:%[0-9]+]] = alloc_box $Int64
// CHECK-NEXT: store %0 to [[X]]
// CHECK: [[Y:%[0-9]+]] = alloc_box $Int64
// CHECK-NEXT: store %1 to [[Y]]
func curried_arguments(x : Int64) (y : Int64) -> Int {
  return x+y
}

// CHECK: sil @_T5decls14tuple_argumentFT1xTSiSfT___T_
// CHECK: bb0(%0 : $Int64, %1 : $Float32):
// CHECK: [[XADDR:%[0-9]+]] = alloc_box $(Int64, Float32, ())
// CHECK: [[EMPTY:%[0-9]+]] = tuple ()
// CHECK: [[X:%[0-9]+]] = tuple (%0 : {{.*}}, %1 : {{.*}}, [[EMPTY]] : {{.*}})
// CHECK: store [[X]] to [[XADDR]]
func tuple_argument(x : (Int, Float32, ())) {
}

// CHECK: sil @_T5decls14byref_argumentFT1xRSi1ySi_T_
// CHECK: bb0(%0 : $*Int64, %1 : $Int64):
// CHECK-NOT: alloc_box
// CHECK-NOT: store
// CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int64
// CHECK: [[Y:%[0-9]+]] = load [[YADDR]]
// CHECK: store [[Y]] to %0
func byref_argument(x : [byref] Int64, y : Int64) {
  x = y
}

var global : Int

// CHECK: sil @_T5decls16load_from_globalFT_Si
func load_from_global() -> Int {
  return global
  // CHECK: [[ADDR:%[0-9]+]] = global_addr #global : $*Int64
  // CHECK: [[VALUE:%[0-9]+]] = load [[ADDR]]
  // CHECK: return [[VALUE]]
}

// CHECK: sil @_T5decls15store_to_globalFT1xSi_T_
func store_to_global(x:Int) {
  global = x
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int64
  // CHECK: [[X:%[0-9]+]] = load [[XADDR]]
  // CHECK: [[ADDR:%[0-9]+]] = global_addr #global : $*Int64
  // CHECK: store [[X]] to [[ADDR]]
  // CHECK: return
}

struct S {
  var x:Int

  // CHECK: sil @_TV5decls1SCfMS0_FT_S0_
  constructor() {
    x = 219
  }

  constructor(a:Int, b:Int) {
    x = a + b
  }
}
