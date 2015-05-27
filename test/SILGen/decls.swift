// RUN: %target-swift-frontend -parse-as-library -emit-silgen %s | FileCheck %s

// CHECK-LABEL: sil hidden @_TF5decls11void_returnFT_T_
// CHECK: = tuple
// CHECK: return
func void_return() {
}

// CHECK-LABEL: sil hidden @_TF5decls14typealias_declFT_T_
func typealias_decl() {
  typealias a = Int
}

// CHECK-LABEL: sil hidden @_TF5decls15simple_patternsFT_T_
func simple_patterns() {
  _ = 4
  var _ : Int
}

// CHECK-LABEL: sil hidden @_TF5decls13named_patternFT_Si
func named_pattern() -> Int {
  var local_var : Int = 4

  var defaulted_var : Int  // Defaults to zero initialization

  return local_var + defaulted_var
}

func MRV() -> (Int, Float, (), Double) {}

// CHECK-LABEL: sil hidden @_TF5decls14tuple_patternsFT_T_
func tuple_patterns() {
  var (a, b) : (Int, Float)
  // CHECK: [[AADDR1:%[0-9]+]] = alloc_box $Int
  // CHECK: [[AADDR:%[0-9]+]] = mark_uninitialized [var] [[AADDR1]]#1
  // CHECK: [[BADDR1:%[0-9]+]] = alloc_box $Float
  // CHECK: [[BADDR:%[0-9]+]] = mark_uninitialized [var] [[BADDR1]]#1

  var (c, d) = (a, b)
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[DADDR:%[0-9]+]] = alloc_box $Float
  // CHECK: copy_addr [[AADDR]] to [initialization] [[CADDR]]#1
  // CHECK: copy_addr [[BADDR]] to [initialization] [[DADDR]]#1

  // CHECK: [[EADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[FADDR:%[0-9]+]] = alloc_box $Float
  // CHECK: [[GADDR:%[0-9]+]] = alloc_box $()
  // CHECK: [[HADDR:%[0-9]+]] = alloc_box $Double
  // CHECK: [[EFGH:%[0-9]+]] = apply
  // CHECK: [[E:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[F:%[0-9]+]] = tuple_extract {{.*}}, 1
  // CHECK: [[G:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: [[H:%[0-9]+]] = tuple_extract {{.*}}, 3
  // CHECK: store [[E]] to [[EADDR]]
  // CHECK: store [[F]] to [[FADDR]]
  // CHECK: store [[H]] to [[HADDR]]
  var (e,f,g,h) : (Int, Float, (), Double) = MRV()

  // CHECK: [[IADDR:%[0-9]+]] = alloc_box $Int
  // CHECK-NOT: alloc_box $Float
  // CHECK: copy_addr [[AADDR]] to [initialization] [[IADDR]]#1
  // CHECK: [[B:%[0-9]+]] = load [[BADDR]]
  // CHECK-NOT: store [[B]]
  var (i,_) = (a, b)

  // CHECK: [[JADDR:%[0-9]+]] = alloc_box $Int
  // CHECK-NOT: alloc_box $Float
  // CHECK: [[KADDR:%[0-9]+]] = alloc_box $()
  // CHECK-NOT: alloc_box $Double
  // CHECK: [[J_K_:%[0-9]+]] = apply
  // CHECK: [[J:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[K:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: store [[J]] to [[JADDR]]
  var (j,_,k,_) : (Int, Float, (), Double) = MRV()
}

// CHECK-LABEL: sil hidden @_TF5decls16simple_arguments
// CHECK: bb0(%0 : $Int, %1 : $Int):
// CHECK: [[X:%[0-9]+]] = alloc_box $Int
// CHECK-NEXT: store %0 to [[X]]
// CHECK-NEXT: [[Y:%[0-9]+]] = alloc_box $Int
// CHECK-NEXT: store %1 to [[Y]]
func simple_arguments(var x: Int, var y: Int) -> Int {
  return x+y
}

// CHECK-LABEL: sil hidden @_TF5decls17curried_arguments
// CHECK: bb0(%0 : $Int, %1 : $Int):
// CHECK: [[X:%[0-9]+]] = alloc_box $Int
// CHECK-NEXT: store %0 to [[X]]
// CHECK: [[Y:%[0-9]+]] = alloc_box $Int
// CHECK-NEXT: store %1 to [[Y]]
func curried_arguments(var x: Int)(var y: Int) -> Int {
  return x+y
}

// CHECK-LABEL: sil hidden @_TF5decls14tuple_argument
// CHECK: bb0(%0 : $Int, %1 : $Float):
// CHECK: [[UNIT:%[0-9]+]] = tuple ()
// CHECK: [[TUPLE:%[0-9]+]] = tuple (%0 : $Int, %1 : $Float, [[UNIT]] : $())
// CHECK: [[XADDR:%[0-9]+]] = alloc_box $(Int, Float, ())
// CHECK: store [[TUPLE]] to [[XADDR]]
func tuple_argument(var x: (Int, Float, ())) {
}

// CHECK-LABEL: sil hidden @_TF5decls14inout_argument
// CHECK: bb0(%0 : $*Int, %1 : $Int):
// CHECK: [[X_LOCAL:%[0-9]+]] = alloc_box $Int
// CHECK: [[YADDR:%[0-9]+]] = alloc_box $Int
// CHECK: copy_addr [[YADDR]]#1 to [[X_LOCAL]]#1
func inout_argument(inout x: Int, var y: Int) {
  x = y
}

var global = 42

// CHECK-LABEL: sil hidden @_TF5decls16load_from_global
func load_from_global() -> Int {
  return global
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @_TF5declsau6globalSi
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: [[VALUE:%[0-9]+]] = load [[ADDR]]
  // CHECK: return [[VALUE]]
}

// CHECK-LABEL: sil hidden @_TF5decls15store_to_global
func store_to_global(var x: Int) {
  global = x
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box $Int
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @_TF5declsau6globalSi
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: copy_addr [[XADDR]]#1 to [[ADDR]]
  // CHECK: return
}

struct S {
  var x:Int

  // CHECK-LABEL: sil hidden @_TFV5decls1SCfMS0_FT_S0_
  init() {
    x = 219
  }

  init(a:Int, b:Int) {
    x = a + b
  }
}

// CHECK-LABEL: StructWithStaticVar.init
// rdar://15821990 - Don't emit default value for static var in instance init()
struct StructWithStaticVar {
  static var a : String = ""
  var b : String = ""

  init() {
  }
}

// <rdar://problem/17405715> lazy property crashes silgen of implicit memberwise initializer
// CHECK-LABEL: // decls.StructWithLazyField.init
// CHECK-NEXT: sil hidden @_TFV5decls19StructWithLazyFieldCfMS0_FT4onceGSqSi__S0_ : $@convention(thin) (Optional<Int>, @thin StructWithLazyField.Type) -> @owned StructWithLazyField {
struct StructWithLazyField {
  lazy var once : Int = 42
  let someProp = "Some value"
}

// <rdar://problem/21057425> Crash while compiling attached test-app.
// CHECK-LABEL: // decls.test21057425
func test21057425() {
  var x = 0, y: Int = 0
}
