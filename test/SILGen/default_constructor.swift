// RUN: %target-swift-emit-silgen -Xllvm -sil-print-types -Xllvm -sil-full-demangle -primary-file %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s19default_constructor1DV1iSivpfi : $@convention(thin) () -> (Int, Double)
// CHECK:      [[VALUE:%.*]] = integer_literal $Builtin.IntLiteral, 2
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Int.Type
// CHECK:      [[FN:%.*]] = function_ref @$sSi22_builtinIntegerLiteralSiBI_tcfC : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT: [[LEFT:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.IntLiteral, @thin Int.Type) -> Int
// CHECK-NEXT: [[VALUE:%.*]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x400C000000000000|0x4000E000000000000000}}
// CHECK-NEXT: [[METATYPE:%.*]] = metatype $@thin Double.Type
// CHECK:      [[FN:%.*]] = function_ref @$sSd20_builtinFloatLiteralSdBf{{64|80}}__tcfC : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Double.Type) -> Double
// CHECK-NEXT: [[RIGHT:%.*]] = apply [[FN]]([[VALUE]], [[METATYPE]]) : $@convention(method) (Builtin.FPIEEE{{64|80}}, @thin Double.Type) -> Double
// CHECK-NEXT: [[RESULT:%.*]] = tuple ([[LEFT]] : $Int, [[RIGHT]] : $Double)
// CHECK-NEXT: return [[RESULT]] : $(Int, Double)


// CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1DV{{[_0-9a-zA-Z]*}}fC : $@convention(method) (@thin D.Type) -> D
// CHECK: [[THISBOX:%[0-9]+]] = alloc_box ${ var D }
// CHECK: [[THIS:%[0-9]+]] = mark_uninit
// CHECK: [[THIS_LIFE:%.*]] = begin_borrow [var_decl] [[THIS]]
// CHECK: [[PB_THIS:%.*]] = project_box [[THIS_LIFE]]
// CHECK: [[IADDR:%[0-9]+]] = struct_element_addr [[PB_THIS]] : $*D, #D.i
// CHECK: [[JADDR:%[0-9]+]] = struct_element_addr [[PB_THIS]] : $*D, #D.j
// CHECK: [[INIT:%[0-9]+]] = function_ref @$s19default_constructor1DV1iSivpfi
// CHECK: [[RESULT:%[0-9]+]] = apply [[INIT]]()
// CHECK: ([[INTVAL:%[0-9]+]], [[FLOATVAL:%[0-9]+]]) = destructure_tuple [[RESULT]]
// CHECK: store [[INTVAL]] to [trivial] [[IADDR]]
// CHECK: store [[FLOATVAL]] to [trivial] [[JADDR]]

class E {
  var i = Int64()
}

// CHECK-LABEL: sil hidden [transparent] [ossa] @$s19default_constructor1EC1is5Int64Vvpfi : $@convention(thin) () -> Int64
// CHECK:      [[IADDR:%[0-9]+]] = alloc_stack $Int64
// CHECK:      [[INTTYPE:%[0-9]+]] = metatype $@thick Int64.Type
// CHECK:      [[INIT:%[0-9]+]] = function_ref @$sSzsExycfC : $@convention(method)
// CHECK-NEXT: apply [[INIT]]<Int64>([[IADDR]], [[INTTYPE]])
// CHECK-NEXT: [[VALUE:%[0-9]+]] = load [trivial] [[IADDR]]
// CHECK-NEXT: dealloc_stack [[IADDR]]
// CHECK-NEXT: return [[VALUE]] : $Int64

// CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1EC{{[_0-9a-zA-Z]*}}fc : $@convention(method) (@owned E) -> @owned E
// CHECK: bb0([[SELFIN:%[0-9]+]] : @owned $E)
// CHECK: [[SELF:%[0-9]+]] = mark_uninitialized
// CHECK-NEXT: [[BORROWED_SELF:%.*]] = begin_borrow [[SELF]]
// CHECK-NEXT: [[IREF:%[0-9]+]] = ref_element_addr [[BORROWED_SELF]] : $E, #E.i
// CHECK: [[INIT:%[0-9]+]] = function_ref @$s19default_constructor1EC1is5Int64Vvpfi : $@convention(thin) () -> Int64
// CHECK-NEXT: [[VALUE:%[0-9]+]] = apply [[INIT]]() : $@convention(thin) () -> Int64
// CHECK-NEXT: store [[VALUE]] to [trivial] [[IREF]] : $*Int64
// CHECK-NEXT: end_borrow [[BORROWED_SELF]]
// CHECK-NEXT: [[SELF_COPY:%.*]] = copy_value [[SELF]]
// CHECK-NEXT: destroy_value [[SELF]]
// CHECK-NEXT: return [[SELF_COPY]] : $E

class F : E { }

// CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1FCACycfc : $@convention(method) (@owned F) -> @owned F
// CHECK: bb0([[ORIGSELF:%[0-9]+]] : @owned $F)
// CHECK-NEXT: [[SELF_BOX:%[0-9]+]] = alloc_box ${ var F }
// CHECK-NEXT: [[SELF:%[0-9]+]] = mark_uninitialized [derivedself] [[SELF_BOX]]
// CHECK-NEXT: [[SELFLIFE:%[^,]+]] = begin_borrow [lexical] [var_decl] [[SELF]]
// CHECK-NEXT: [[PB:%.*]] = project_box [[SELFLIFE]]
// CHECK-NEXT: store [[ORIGSELF]] to [init] [[PB]] : $*F
// CHECK-NEXT: [[SELFP:%[0-9]+]] = load [take] [[PB]] : $*F
// CHECK-NEXT: [[E:%[0-9]]] = upcast [[SELFP]] : $F to $E
// CHECK: [[E_CTOR:%[0-9]+]] = function_ref @$s19default_constructor1ECACycfc : $@convention(method) (@owned E) -> @owned E
// CHECK-NEXT: [[ESELF:%[0-9]]] = apply [[E_CTOR]]([[E]]) : $@convention(method) (@owned E) -> @owned E

// CHECK-NEXT: [[ESELFW:%[0-9]+]] = unchecked_ref_cast [[ESELF]] : $E to $F
// CHECK-NEXT: store [[ESELFW]] to [init] [[PB]] : $*F
// CHECK-NEXT: [[SELFP:%[0-9]+]] = load [copy] [[PB]] : $*F
// CHECK-NEXT: end_borrow [[SELFLIFE]]
// CHECK-NEXT: destroy_value [[SELF]] : ${ var F }
// CHECK-NEXT: return [[SELFP]] : $F

// <rdar://problem/19780343> Default constructor for a struct with optional doesn't compile

// This shouldn't get a default init, since it would be pointless (bar can never
// be reassigned).  It should get a memberwise init though.
struct G {
  let bar: Int32?
}

// CHECK-NOT: default_constructor.G.init()
// CHECK-LABEL: default_constructor.G.init(bar: Swift.Optional<Swift.Int32>)
// CHECK-NEXT: // Isolation:
// CHECK-NEXT: sil hidden [ossa] @$s19default_constructor1GV{{[_0-9a-zA-Z]*}}fC
// CHECK-NOT: default_constructor.G.init()

struct H<T> {
  var opt: T?

  // CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1HVyACyxGqd__clufC : $@convention(method) <T><U> (@in U, @thin H<T>.Type) -> @out H<T> {
  // CHECK: [[OPT_T:%[0-9]+]] = struct_element_addr {{%.*}} : $*H<T>, #H.opt
  // CHECK: [[INIT_FN:%[0-9]+]] = function_ref @$s19default_constructor1HV3optxSgvpfi : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
  // CHECK-NEXT: apply [[INIT_FN]]<T>([[OPT_T]]) : $@convention(thin) <τ_0_0> () -> @out Optional<τ_0_0>
  init<U>(_: U) { }
}

// <rdar://problem/29605388> Member initializer for non-generic type with generic constructor doesn't compile

struct I {
  var x: Int = 0

  // CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1IVyACxclufC : $@convention(method) <T> (@in T, @thin I.Type) -> I {
  // CHECK: [[X_ADDR:%[0-9]+]] = struct_element_addr {{.*}} : $*I, #I.x
  // CHECK: [[INIT_FN:%[0-9]+]] = function_ref @$s19default_constructor1IV1xSivpfi : $@convention(thin) () -> Int
  // CHECK: [[RESULT:%[0-9]+]] = apply [[INIT_FN]]() : $@convention(thin) () -> Int
  // CHECK: store [[RESULT]] to [trivial] [[X_ADDR]] : $*Int
  init<T>(_: T) {}
}

// https://github.com/apple/swift/issues/52477

func defaultValue<T>() -> T {
    fatalError()
}

struct S<T> {
  let value1: T = defaultValue()
  let value2: Int

  // CHECK-LABEL: sil hidden [ossa] @$s19default_constructor1SV6value2ACyxGSi_tcfC : $@convention(method) <T> (Int, @thin S<T>.Type) -> @out S<T>
}
