// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// __FUNCTION__ used as top-level parameter produces the module name.
// CHECK-LABEL: sil @main
// CHECK:         string_literal utf16 "default_arguments"

// Default argument for first parameter.
// CHECK-LABEL: sil hidden @_TIF17default_arguments7defarg1{{.*}} : $@convention(thin) () -> Int
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
// CHECK: [[INT:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[LIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 17
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LIT]], [[INT]]) : $@convention(thin) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK: return [[RESULT]] : $Int

// Default argument for third parameter.
// CHECK-LABEL: sil hidden @_TIF17default_arguments7defarg1{{.*}} : $@convention(thin) () -> @owned String
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TFSSCfMSSFT21_builtinStringLiteralBp8byteSizeBw7isASCIIBi1__SS
// CHECK: [[STRING:%[0-9]+]] = metatype $@thin String.Type
// CHECK: [[LIT:%[0-9]+]] = string_literal utf8 "Hello"
// CHECK: [[LEN:%[0-9]+]] = integer_literal $Builtin.Word, 5
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LIT]], [[LEN]], {{[^,]+}}, [[STRING]]) : $@convention(thin)
// CHECK: return [[RESULT]] : $String
func defarg1(i i: Int = 17, d: Double, s: String = "Hello") { }

// CHECK-LABEL: sil hidden @_TF17default_arguments15testDefaultArg1FT_T_
func testDefaultArg1() {
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @_TF17default_arguments7defarg1{{.*}}
  // CHECK: [[LITFN:%[0-9]+]] = function_ref @_TFSdCfMSdFT20_builtinFloatLiteralBf{{64|80}}__Sd
  // CHECK: [[FLOAT64:%[0-9]+]] = metatype $@thin Double.Type
  // CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x4009000000000000|0x4000C800000000000000}}
  // CHECK: [[FLOATVAL:%[0-9]+]] = apply [[LITFN]]([[FLOATLIT]], [[FLOAT64]])
  // CHECK: [[DEF0FN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg1{{.*}}_A_
  // CHECK: [[DEF0:%[0-9]+]] = apply [[DEF0FN]]()
  // CHECK: [[DEF2FN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg1{{.*}}_A1_
  // CHECK: [[DEF2:%[0-9]+]] = apply [[DEF2FN]]()
  // CHECK: apply [[FNREF]]([[DEF0]], [[FLOATVAL]], [[DEF2]])
  defarg1(d:3.125)
}

func defarg2(i: Int, d: Double = 3.125, s: String = "Hello") { }

// CHECK-LABEL: sil hidden @_TF17default_arguments15testDefaultArg2
func testDefaultArg2() {
// CHECK:  [[FNREF:%[0-9]+]] = function_ref @_TF17default_arguments7defarg2{{.*}} : $@convention(thin) (Int, Double, @owned String) -> ()
// CHECK:  [[LITFN:%[0-9]+]] = function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
// CHECK:  [[INT64:%[0-9]+]] = metatype $@thin Int.Type
// CHECK:  [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 5
// CHECK:  [[I:%[0-9]+]] = apply [[LITFN]]([[INTLIT]], [[INT64]]) : $@convention(thin) (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:  [[DFN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg2{{.*}}_A0_ : $@convention(thin) () -> Double
// CHECK:  [[D:%[0-9]+]] = apply [[DFN]]() : $@convention(thin) () -> Double
// CHECK:  [[SFN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg2{{.*}}_A1_ : $@convention(thin) () -> @owned String
// CHECK:  [[S:%[0-9]+]] = apply [[SFN]]() : $@convention(thin) () -> @owned String
// CHECK:  apply [[FNREF]]([[I]], [[D]], [[S]]) : $@convention(thin) (Int, Double, @owned String) -> ()
  defarg2(5)
}

func autocloseFile(@autoclosure x: () -> String = __FILE__,
                   @autoclosure y: () -> Int = __LINE__) { }
// CHECK-LABEL: sil hidden @_TF17default_arguments17testAutocloseFileFT_T_
func testAutocloseFile() {
  // CHECK-LABEL: sil shared [transparent] @_TFF17default_arguments17testAutocloseFileFT_T_u_KT_SS : $@convention(thin) () -> @owned String
  // CHECK: string_literal utf16{{.*}}default_arguments.swift

  // CHECK-LABEL: sil shared [transparent] @_TFF17default_arguments17testAutocloseFileFT_T_u0_KT_Si : $@convention(thin) () -> Int
  // CHECK: integer_literal $Builtin.Int2048, [[@LINE+1]]
  autocloseFile()
}

func testMagicLiterals(file file: String = __FILE__,
                       function: String = __FUNCTION__,
                       line: Int = __LINE__,
                       column: Int = __COLUMN__) {}

// Check that default argument generator functions don't leak information about
// user's source.
//
// CHECK-LABEL: sil hidden @_TIF17default_arguments17testMagicLiteralsFT4fileSS8functionSS4lineSi6columnSi_T_A_
// CHECK: string_literal utf16 ""
//
// CHECK-LABEL: sil hidden @_TIF17default_arguments17testMagicLiteralsFT4fileSS8functionSS4lineSi6columnSi_T_A0_
// CHECK: string_literal utf16 ""
//
// CHECK-LABEL: sil hidden @_TIF17default_arguments17testMagicLiteralsFT4fileSS8functionSS4lineSi6columnSi_T_A1_
// CHECK: integer_literal $Builtin.Int2048, 0
//
// CHECK-LABEL: sil hidden @_TIF17default_arguments17testMagicLiteralsFT4fileSS8functionSS4lineSi6columnSi_T_A2_
// CHECK: integer_literal $Builtin.Int2048, 0

func closure(_: () -> ()) {}
func autoclosure(@autoclosure _: () -> ()) {}

// CHECK-LABEL: sil hidden @_TF17default_arguments25testCallWithMagicLiteralsFT_T_
// CHECK:         string_literal utf16 "testCallWithMagicLiterals()"
// CHECK:         string_literal utf16 "testCallWithMagicLiterals()"
// CHECK-LABEL: sil shared @_TFF17default_arguments25testCallWithMagicLiteralsFT_T_U_FT_T_
// CHECK:         string_literal utf16 "testCallWithMagicLiterals()"
// CHECK-LABEL: sil shared [transparent] @_TFF17default_arguments25testCallWithMagicLiteralsFT_T_u_KT_T_
// CHECK:         string_literal utf16 "testCallWithMagicLiterals()"
func testCallWithMagicLiterals() {
  testMagicLiterals()
  testMagicLiterals()
  closure { testMagicLiterals() }
  autoclosure(testMagicLiterals())
}

// CHECK-LABEL: sil hidden @_TF17default_argumentsg25testPropWithMagicLiteralsSi
// CHECK:         string_literal utf16 "testPropWithMagicLiterals"
var testPropWithMagicLiterals: Int {
  testMagicLiterals()
  closure { testMagicLiterals() }
  autoclosure(testMagicLiterals())
  return 0
}

class Foo {

  // CHECK-LABEL: sil hidden @_TFC17default_arguments3FoocfMS0_FT3intSi6stringSS_S0_ : $@convention(method) (Int, @owned String, @owned Foo) -> @owned Foo
  // CHECK:         string_literal utf16 "init(int:string:)"
  init(int: Int, string: String) {
    testMagicLiterals()
    closure { testMagicLiterals() }
    autoclosure(testMagicLiterals())
  }

  // CHECK-LABEL: sil hidden @_TFC17default_arguments3Food
  // CHECK:         string_literal utf16 "deinit"
  deinit {
    testMagicLiterals()
    closure { testMagicLiterals() }
    autoclosure(testMagicLiterals())
  }

  // CHECK-LABEL: sil hidden @_TFC17default_arguments3Foog9subscriptFSiSi
  // CHECK:         string_literal utf16 "subscript"
  subscript(x: Int) -> Int {
    testMagicLiterals()
    closure { testMagicLiterals() }
    autoclosure(testMagicLiterals())
    return x
  }
}

// Test at top level.
testMagicLiterals()
closure { testMagicLiterals() }
autoclosure(testMagicLiterals())

// CHECK-LABEL: sil hidden @_TF17default_arguments16testSelectorCallFTSi17withMagicLiteralsSi_T_
// CHECK:         string_literal utf16 "testSelectorCall(_:withMagicLiterals:)"
func testSelectorCall(x: Int, withMagicLiterals y: Int) {
  testMagicLiterals()
}

// CHECK-LABEL: sil hidden @_TF17default_arguments32testSelectorCallWithUnnamedPieceFTSiSi_T_
// CHECK:         string_literal utf16 "testSelectorCallWithUnnamedPiece"
func testSelectorCallWithUnnamedPiece(x: Int, _ y: Int) {
  testMagicLiterals()
}

// Test default arguments in an inherited subobject initializer
class SuperDefArg {
  init(int i: Int = 10) { }
}

// CHECK: sil hidden @_TIFC17default_arguments11SuperDefArgcFMS0_FT3intSi_S0_A_ : $@convention(thin) () -> Int

// CHECK-NOT: sil hidden @_TIFC17default_arguments9SubDefArgcFMS0_FT3intSi_S0_A_ : $@convention(thin) () -> Int

class SubDefArg : SuperDefArg { }

// CHECK: sil hidden @_TF17default_arguments13testSubDefArgFT_CS_9SubDefArg : $@convention(thin) () -> @owned SubDefArg
func testSubDefArg() -> SubDefArg {
  // CHECK: function_ref @_TFC17default_arguments9SubDefArgCfMS0_FT3intSi_S0_
  // CHECK: function_ref @_TIFC17default_arguments11SuperDefArgcFMS0_FT3intSi_S0_A_
  // CHECK: return
  return SubDefArg()
}

// CHECK-NOT: sil hidden @_TIFC17default_arguments9SubDefArgcFMS0_FT3intSi_S0_A_ : $@convention(thin) () -> Int

// <rdar://problem/17379550>
func takeDefaultArgUnnamed(x: Int = 5) { }

// CHECK-LABEL: sil hidden @_TF17default_arguments25testTakeDefaultArgUnnamed
func testTakeDefaultArgUnnamed(i: Int) {
  // CHECK: bb0([[I:%[0-9]+]] : $Int):
  // CHECK:   [[FN:%[0-9]+]] = function_ref @_TF17default_arguments21takeDefaultArgUnnamedFTSi_T_ : $@convention(thin) (Int) -> ()
  // CHECK:   apply [[FN]]([[I]]) : $@convention(thin) (Int) -> ()
  takeDefaultArgUnnamed(i)
}

func takeDSOHandle(handle: UnsafeMutablePointer<Void> = __DSO_HANDLE__) { }

// CHECK-LABEL: sil hidden @_TF17default_arguments13testDSOHandleFT_T_
func testDSOHandle() {
  // CHECK: [[DSO_HANDLE:%[0-9]+]] = global_addr @_Tv17default_arguments12__dso_handleGVSs20UnsafeMutablePointerT__ : $*UnsafeMutablePointer<()>
  takeDSOHandle()
}

// Test __FUNCTION__ in an extension initializer. rdar://problem/19792181
extension SuperDefArg {
  static let extensionInitializerWithClosure: Int = { return 22 }()
}


// <rdar://problem/19086357> SILGen crashes reabstracting default argument closure in members
class ReabstractDefaultArgument<T> {
  init(a: (T, T) -> Bool = { _, _ in true }) {
  }
}

// CHECK-LABEL: sil hidden @_TF17default_arguments32testDefaultArgumentReabstractionFT_T_
// function_ref default_arguments.ReabstractDefaultArgument.__allocating_init <A>(default_arguments.ReabstractDefaultArgument<A>.Type)(a : (A, A) -> Swift.Bool) -> default_arguments.ReabstractDefaultArgument<A>
// CHECK: [[INITFN:%[0-9]+]] = function_ref @_TFC17default_arguments25ReabstractDefaultArgumentCU__fMGS0_Q__FT1aFTQ_Q__Sb_GS0_Q__
// %1 = metatype $@thick ReabstractDefaultArgument<Int>.Type
// function_ref default_arguments.ReabstractDefaultArgument.(init <A>(default_arguments.ReabstractDefaultArgument<A>.Type) -> (a : (A, A) -> Swift.Bool) -> default_arguments.ReabstractDefaultArgument<A>).(default argument 0)
// CHECK: %2 = function_ref @_TIFC17default_arguments25ReabstractDefaultArgumentcU__FMGS0_Q__FT1aFTQ_Q__Sb_GS0_Q__A_ : $@convention(thin) <τ_0_0> () -> @owned @callee_owned (@in τ_0_0, @in τ_0_0) -> Bool
// CHECK-NEXT: %3 = apply %2<Int>() : $@convention(thin) <τ_0_0> () -> @owned @callee_owned (@in τ_0_0, @in τ_0_0) -> Bool
// CHECK-NEXT: function_ref reabstraction thunk helper from @callee_owned (@in Swift.Int, @in Swift.Int) -> (@unowned Swift.Bool) to @callee_owned (@unowned Swift.Int, @unowned Swift.Int) -> (@unowned Swift.Bool)
// CHECK-NEXT: %4 = function_ref @_TTRXFo_iSiiSi_dSb_XFo_dSidSi_dSb_ : $@convention(thin) (Int, Int, @owned @callee_owned (@in Int, @in Int) -> Bool) -> Bool
// CHECK-NEXT: %5 = partial_apply %4(%3) : $@convention(thin) (Int, Int, @owned @callee_owned (@in Int, @in Int) -> Bool) -> Bool
// function_ref reabstraction thunk helper from @callee_owned (@unowned Swift.Int, @unowned Swift.Int) -> (@unowned Swift.Bool) to @callee_owned (@in Swift.Int, @in Swift.Int) -> (@unowned Swift.Bool)
// CHECK: %6 = function_ref @_TTRXFo_dSidSi_dSb_XFo_iSiiSi_dSb_ : $@convention(thin) (@in Int, @in Int, @owned @callee_owned (Int, Int) -> Bool) -> Bool
// CHECK-NEXT: %7 = partial_apply %6(%5) : $@convention(thin) (@in Int, @in Int, @owned @callee_owned (Int, Int) -> Bool) -> Bool
// CHECK-NEXT: apply [[INITFN]]<Int>(%7, 

func testDefaultArgumentReabstraction() {
  ReabstractDefaultArgument<Int>()
}


