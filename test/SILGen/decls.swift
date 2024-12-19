// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -Xllvm -sil-full-demangle -parse-as-library %s | %FileCheck %s

// CHECK-LABEL: sil hidden [ossa] @$s5decls11void_returnyyF
// CHECK: = tuple
// CHECK: return
func void_return() {
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls14typealias_declyyF
func typealias_decl() {
  typealias a = Int
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls15simple_patternsyyF
func simple_patterns() {
  _ = 4
  var _ : Int
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls13named_patternSiyF
func named_pattern() -> Int {
  var local_var : Int = 4

  var defaulted_var : Int  // Defaults to zero initialization

  return local_var + defaulted_var
}

func MRV() -> (Int, Float, (), Double) {}

// CHECK-LABEL: sil hidden [ossa] @$s5decls14tuple_patternsyyF
func tuple_patterns() {
  var (a, b) : (Int, Float)
  // CHECK: [[ABOX:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[AADDR:%[0-9]+]] = mark_uninitialized [var] [[ABOX]]
  // CHECK: [[ALIFE:%.*]] = begin_borrow [var_decl] [[AADDR]]
  // CHECK: [[PBA:%.*]] = project_box [[ALIFE]]
  // CHECK: [[BBOX:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[BADDR:%[0-9]+]] = mark_uninitialized [var] [[BBOX]]
  // CHECK: [[BLIFE:%.*]] = begin_borrow [var_decl] [[BADDR]]
  // CHECK: [[PBB:%.*]] = project_box [[BLIFE]]

  var (c, d) = (a, b)
  // CHECK: [[CADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[CLIFE:%.*]] = begin_borrow [var_decl] [[CADDR]]
  // CHECK: [[PBC:%.*]] = project_box [[CLIFE]]
  // CHECK: [[DADDR:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[DLIFE:%.*]] = begin_borrow [var_decl] [[DADDR]]
  // CHECK: [[PBD:%.*]] = project_box [[DLIFE]]
  // CHECK: [[READA:%.*]] = begin_access [read] [unknown] [[PBA]] : $*Int
  // CHECK: copy_addr [[READA]] to [init] [[PBC]]
  // CHECK: [[READB:%.*]] = begin_access [read] [unknown] [[PBB]] : $*Float
  // CHECK: copy_addr [[READB]] to [init] [[PBD]]
  // CHECK: [[EADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[ELIFE:%.*]] = begin_borrow [var_decl] [[EADDR]]
  // CHECK: [[PBE:%.*]] = project_box [[ELIFE]]
  // CHECK: [[FADDR:%[0-9]+]] = alloc_box ${ var Float }
  // CHECK: [[FLIFE:%.*]] = begin_borrow [var_decl] [[FADDR]]
  // CHECK: [[PBF:%.*]] = project_box [[FLIFE]]
  // CHECK: [[GADDR:%[0-9]+]] = alloc_box ${ var () }
  // CHECK: [[HADDR:%[0-9]+]] = alloc_box ${ var Double }
  // CHECK: [[HLIFE:%.*]] = begin_borrow [var_decl] [[HADDR]]
  // CHECK: [[PBH:%.*]] = project_box [[HLIFE]]
  // CHECK: [[EFGH:%[0-9]+]] = apply
  // CHECK: ([[E:%[0-9]+]], [[F:%[0-9]+]], [[H:%[0-9]+]]) = destructure_tuple
  // CHECK: store [[E]] to [trivial] [[PBE]]
  // CHECK: store [[F]] to [trivial] [[PBF]]
  // CHECK: store [[H]] to [trivial] [[PBH]]
  var (e,f,g,h) : (Int, Float, (), Double) = MRV()

  // CHECK: [[IADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[ILIFE:%.*]] = begin_borrow [var_decl] [[IADDR]]
  // CHECK: [[PBI:%.*]] = project_box [[ILIFE]]
  // CHECK-NOT: alloc_box ${ var Float }
  // CHECK: [[READA:%.*]] = begin_access [read] [unknown] [[PBA]] : $*Int
  // CHECK: copy_addr [[READA]] to [init] [[PBI]]
  // CHECK: [[READB:%.*]] = begin_access [read] [unknown] [[PBB]] : $*Float
  // CHECK: [[B:%[0-9]+]] = load [trivial] [[READB]]
  // CHECK-NOT: store [[B]]
  var (i,_) = (a, b)

  // CHECK: [[JADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[JLIFE:%.*]] = begin_borrow [var_decl] [[JADDR]]
  // CHECK: [[PBJ:%.*]] = project_box [[JLIFE]]
  // CHECK-NOT: alloc_box ${ var Float }
  // CHECK: [[KADDR:%[0-9]+]] = alloc_box ${ var () }
  // CHECK-NOT: alloc_box ${ var Double }
  // CHECK: [[J_K_:%[0-9]+]] = apply
  // CHECK: ([[J:%[0-9]+]], [[K:%[0-9]+]], {{%[0-9]+}}) = destructure_tuple
  // CHECK: store [[J]] to [trivial] [[PBJ]]
  var (j,_,k,_) : (Int, Float, (), Double) = MRV()
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls16simple_arguments{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $Int, %1 : $Int):
// CHECK: [[X:%[0-9]+]] = alloc_box ${ var Int }
// CHECK-NEXT: [[XLIFE:%.*]] = begin_borrow [var_decl] [[X]]
// CHECK-NEXT: [[PBX:%.*]] = project_box [[XLIFE]]
// CHECK-NEXT: store %0 to [trivial] [[PBX]]
// CHECK-NEXT: [[Y:%[0-9]+]] = alloc_box ${ var Int }
// CHECK-NEXT: [[YLIFE:%[0-9]+]] = begin_borrow [var_decl] [[Y]]
// CHECK-NEXT: [[PBY:%[0-9]+]] = project_box [[YLIFE]]
// CHECK-NEXT: store %1 to [trivial] [[PBY]]
func simple_arguments(x: Int, y: Int) -> Int {
  var x = x
  var y = y
  return x+y
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls14tuple_argument{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $Int, %1 : $Float):
// CHECK: [[UNIT:%[0-9]+]] = tuple ()
// CHECK: [[TUPLE:%[0-9]+]] = tuple (%0 : $Int, %1 : $Float, [[UNIT]] : $())
func tuple_argument(x: (Int, Float, ())) {
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls14inout_argument{{[_0-9a-zA-Z]*}}F
// CHECK: bb0(%0 : $*Int, %1 : $Int):
// CHECK: [[X_LOCAL:%[0-9]+]] = alloc_box ${ var Int }
// CHECK: [[XLIFE:%.*]] = begin_borrow [var_decl] [[X_LOCAL]]
// CHECK: [[PBX:%.*]] = project_box [[XLIFE]]
func inout_argument(x: inout Int, y: Int) {
  var y = y
  x = y
}

var global = 42

// CHECK-LABEL: sil hidden [ossa] @$s5decls16load_from_global{{[_0-9a-zA-Z]*}}F
func load_from_global() -> Int {
  return global
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @$s5decls6globalSivau
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: [[READ:%.*]] = begin_access [read] [dynamic] [[ADDR]] : $*Int
  // CHECK: [[VALUE:%[0-9]+]] = load [trivial] [[READ]]
  // CHECK: return [[VALUE]]
}

// CHECK-LABEL: sil hidden [ossa] @$s5decls15store_to_global{{[_0-9a-zA-Z]*}}F
func store_to_global(x: Int) {
  var x = x
  global = x
  // CHECK: [[XADDR:%[0-9]+]] = alloc_box ${ var Int }
  // CHECK: [[XLIFE:%.*]] = begin_borrow [var_decl] [[XADDR]]
  // CHECK: [[PBX:%.*]] = project_box [[XLIFE]]
  // CHECK: [[ACCESSOR:%[0-9]+]] = function_ref @$s5decls6globalSivau
  // CHECK: [[PTR:%[0-9]+]] = apply [[ACCESSOR]]()
  // CHECK: [[ADDR:%[0-9]+]] = pointer_to_address [[PTR]]
  // CHECK: [[READ:%.*]] = begin_access [read] [unknown] [[PBX]] : $*Int
  // CHECK: [[COPY:%.*]] = load [trivial] [[READ]] : $*Int
  // CHECK: [[WRITE:%.*]] = begin_access [modify] [dynamic] [[ADDR]] : $*Int
  // CHECK: assign [[COPY]] to [[WRITE]] : $*Int
  // CHECK: end_access [[WRITE]] : $*Int
  // CHECK: return
}

struct S {
  var x:Int

  // CHECK-LABEL: sil hidden [ossa] @$s5decls1SVACycfC
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

// Make sure unbound method references on class hierarchies are
// properly represented in the AST

class Base {
  func method1() -> Self { return self }
  func method2() -> Self { return self }
}

class Derived : Base {
  override func method2() -> Self { return self }
}

func generic<T>(arg: T) { }

func unboundMethodReferences() {
  generic(arg: Derived.method1)
  generic(arg: Derived.method2)

  _ = type(of: Derived.method1)
  _ = type(of: Derived.method2)
}

// CHECK-LABEL: sil_vtable EscapeKeywordsInDottedPaths
class EscapeKeywordsInDottedPaths {
  // CHECK: #EscapeKeywordsInDottedPaths.`switch`!getter
  var `switch`: String = ""
}
