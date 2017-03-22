// RUN: %target-swift-frontend -Xllvm -sil-full-demangle -parse-as-library -emit-silgen %s | %FileCheck %s

// CHECK-LABEL: sil hidden @_T05decls11void_returnyyF
// CHECK: = tuple
// CHECK: return
func void_return() {
}

// CHECK-LABEL: sil hidden @_T05decls14typealias_declyyF
func typealias_decl() {
  typealias a = Int
}

// CHECK-LABEL: sil hidden @_T05decls15simple_patternsyyF
func simple_patterns() {
  _ = 4
  var _ : Int
}

// CHECK-LABEL: sil hidden @_T05decls13named_patternSiyF
func named_pattern() -> Int {
  var local_var : Int = 4

  var defaulted_var : Int  // Defaults to zero initialization

  return local_var + defaulted_var
}

func MRV() -> (Int, Float, (), Double) {}

// CHECK-LABEL: sil hidden @_T05decls14tuple_patternsyyF
func tuple_patterns() {
  var (a, b) : (Int, Float)
  // CHECK: [[AADDR1:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBA:%.*]] = project_box [[AADDR1]]
  // CHECK: [[AADDR:%[0-9]+]] = mark_uninitialized [var] [[PBA]]
  // CHECK: [[BADDR1:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[PBB:%.*]] = project_box [[BADDR1]]
  // CHECK: [[BADDR:%[0-9]+]] = mark_uninitialized [var] [[PBB]]

  var (c, d) = (a, b)
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBC:%.*]] = project_box [[CADDR]]
  // CHECK: [[DADDR:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[PBD:%.*]] = project_box [[DADDR]]
  // CHECK: copy_addr [[AADDR]] to [initialization] [[PBC]]
  // CHECK: copy_addr [[BADDR]] to [initialization] [[PBD]]

  // CHECK: [[EADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBE:%.*]] = project_box [[EADDR]]
  // CHECK: [[FADDR:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[PBF:%.*]] = project_box [[FADDR]]
  // CHECK: [[GADDR:%[0-9]+]] = alloc_box ${ var () }
  // CHECK: [[HADDR:%[0-9]+]] = alloc_box ${ var Double }
  // CHECK: [[PBH:%.*]] = project_box [[HADDR]]
  // CHECK: [[EFGH:%[0-9]+]] = apply
  // CHECK: [[E:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[F:%[0-9]+]] = tuple_extract {{.*}}, 1
  // CHECK: [[H:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: store [[E]] to [trivial] [[PBE]]
  // CHECK: store [[F]] to [trivial] [[PBF]]
  // CHECK: store [[H]] to [trivial] [[PBH]]
  var (e,f,g,h) : (Int, Float, (), Double) = MRV()

  // CHECK: [[IADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBI:%.*]] = project_box [[IADDR]]
  // CHECK-NOT: alloc_box ${ var Float }
  // CHECK: copy_addr [[AADDR]] to [initialization] [[PBI]]
  // CHECK: [[B:%[0-9]+]] = load [trivial] [[BADDR]]
  // CHECK-NOT: store [[B]]
  var (i,_) = (a, b)

  // CHECK: [[JADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBJ:%.*]] = project_box [[JADDR]]
  // CHECK-NOT: alloc_box ${ var Float }
  // CHECK: [[KADDR:%[0-9]+]] = alloc_box ${ var () }
  // CHECK-NOT: alloc_box ${ var Double }
  // CHECK: [[J_K_:%[0-9]+]] = apply
  // CHECK: [[J:%[0-9]+]] = tuple_extract {{.*}}, 0
  // CHECK: [[K:%[0-9]+]] = tuple_extract {{.*}}, 2
  // CHECK: store [[J]] to [trivial] [[PBJ]]
  var (j,_,k,_) : (Int, Float, (), Double) = MRV()
}

// CHECK-LABEL: sil hidden @_T05decls16simple_arguments{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $Int, %1 : $Int):
// CHECK: [[X:%[0-9]+]] = alloc_box ${ var Int }
// CHECK-NEXT: [[PBX:%.*]] = project_box [[X]]
// CHECK-NEXT: store %0 to [trivial] [[PBX]]
// CHECK-NEXT: [[Y:%[0-9]+]] = alloc_box ${ var Int }
// CHECK-NEXT: [[PBY:%[0-9]+]] = project_box [[Y]]
// CHECK-NEXT: store %1 to [trivial] [[PBY]]
func simple_arguments(x: Int, y: Int) -> Int {
  var x = x
  var y = y
  return x+y
}

// CHECK-LABEL: sil hidden @_T05decls14tuple_argument{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $Int, %1 : $Float):
// CHECK: [[UNIT:%[0-9]+]] = tuple ()
// CHECK: [[TUPLE:%[0-9]+]] = tuple (%0 : $Int, %1 : $Float, [[UNIT]] : $())
func tuple_argument(x: (Int, Float, ())) {
}

// CHECK-LABEL: sil hidden @_T05decls14inout_argument{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Int, %1 : $Int):
// CHECK: [[X_LOCAL:%[0-9]+]] = alloc_box ${ var Int }
// CHECK: [[PBX:%.*]] = project_box [[X_LOCAL]]
func inout_argument(x: inout Int, y: Int) {
  var y = y
  x = y
}

var global = 42

// CHECK-LABEL: sil hidden @_T05decls16load_from_global{{[_0-9a-zA-Z]*}}F
func load_from_global() -> Int {
  return global
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @_T05decls6globalSifau
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: [[VALUE:%[0-9]+]] = load [trivial] [[ADDR]]
  // CHECK: return [[VALUE]]
}

// CHECK-LABEL: sil hidden @_T05decls15store_to_global{{[_0-9a-zA-Z]*}}F
func store_to_global(x: Int) {
  var x = x
  global = x
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[PBX:%.*]] = project_box [[XADDR]]
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @_T05decls6globalSifau
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: copy_addr [[PBX]] to [[ADDR]]
  // CHECK: return
}

struct S {
  var x:Int

  // CHECK-LABEL: sil hidden @_T05decls1SVACycfC
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
// CHECK-NEXT: sil hidden @_T05decls19StructWithLazyFieldVACSiSg4once_tcfC : $@convention(method) (Optional<Int>, @thin StructWithLazyField.Type) -> @owned StructWithLazyField {
struct StructWithLazyField {
  lazy var once : Int = 42
  let someProp = "Some value"
}

// <rdar://problem/21057425> Crash while compiling attached test-app.
// CHECK-LABEL: // decls.test21057425
func test21057425() {
  var x = 0, y: Int = 0
}

func useImplicitDecls() {
  _ = StructWithLazyField(once: 55)
}

