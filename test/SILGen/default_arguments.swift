// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

// __FUNCTION__ used as top-level parameter produces the module name.
// CHECK-LABEL: sil @main
// CHECK:         string_literal utf16 "default_arguments"

// Default argument for first parameter.
// CHECK-LABEL: sil hidden @_TIF17default_arguments7defarg1{{.*}} : $@thin () -> Int
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
// CHECK: [[INT:%[0-9]+]] = metatype $@thin Int.Type
// CHECK: [[LIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 17
// CHECK: [[RESULT:%[0-9]+]] = apply [transparent] [[CVT]]([[LIT]], [[INT]]) : $@thin (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK: return [[RESULT]] : $Int

// Default argument for third parameter.
// CHECK-LABEL: sil hidden @_TIF17default_arguments7defarg1{{.*}} : $@thin () -> @owned String
// CHECK: [[CVT:%[0-9]+]] = function_ref @_TFSSCfMSSFT21_builtinStringLiteralBp8byteSizeBw7isASCIIBi1__SS
// CHECK: [[STRING:%[0-9]+]] = metatype $@thin String.Type
// CHECK: [[LIT:%[0-9]+]] = string_literal utf8 "Hello"
// CHECK: [[LEN:%[0-9]+]] = integer_literal $Builtin.Word, 5
// CHECK: [[RESULT:%[0-9]+]] = apply [[CVT]]([[LIT]], [[LEN]], {{[^,]+}}, [[STRING]]) : $@thin
// CHECK: return [[RESULT]] : $String
func defarg1(i: Int = 17, #d: Double, s: String = "Hello") { }

// CHECK-LABEL: sil hidden @_TF17default_arguments15testDefaultArg1FT_T_
func testDefaultArg1() {
  // CHECK: [[FNREF:%[0-9]+]] = function_ref @_TF17default_arguments7defarg1{{.*}}
  // CHECK: [[LITFN:%[0-9]+]] = function_ref @_TFSdCfMSdFT20_builtinFloatLiteralBf{{64|80}}__Sd
  // CHECK: [[FLOAT64:%[0-9]+]] = metatype $@thin Double.Type
  // CHECK: [[FLOATLIT:%[0-9]+]] = float_literal $Builtin.FPIEEE{{64|80}}, {{0x4009000000000000|0x4000C800000000000000}}
  // CHECK: [[FLOATVAL:%[0-9]+]] = apply [transparent] [[LITFN]]([[FLOATLIT]], [[FLOAT64]])
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
// CHECK:  [[FNREF:%[0-9]+]] = function_ref @_TF17default_arguments7defarg2{{.*}} : $@thin (Int, Double, @owned String) -> ()
// CHECK:  [[LITFN:%[0-9]+]] = function_ref @_TFSiCfMSiFT22_builtinIntegerLiteralBi2048__Si
// CHECK:  [[INT64:%[0-9]+]] = metatype $@thin Int.Type
// CHECK:  [[INTLIT:%[0-9]+]] = integer_literal $Builtin.Int2048, 5
// CHECK:  [[I:%[0-9]+]] = apply [transparent] [[LITFN]]([[INTLIT]], [[INT64]]) : $@thin (Builtin.Int2048, @thin Int.Type) -> Int
// CHECK:  [[DFN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg2{{.*}}_A0_ : $@thin () -> Double
// CHECK:  [[D:%[0-9]+]] = apply [[DFN]]() : $@thin () -> Double
// CHECK:  [[SFN:%[0-9]+]] = function_ref @_TIF17default_arguments7defarg2{{.*}}_A1_ : $@thin () -> @owned String
// CHECK:  [[S:%[0-9]+]] = apply [[SFN]]() : $@thin () -> @owned String
// CHECK:  apply [[FNREF]]([[I]], [[D]], [[S]]) : $@thin (Int, Double, @owned String) -> ()
  defarg2(5)
}

func autocloseFile(@autoclosure x: () -> String = __FILE__,
                   @autoclosure y: () -> Int = __LINE__) { }
// CHECK-LABEL: sil hidden @_TF17default_arguments17testAutocloseFileFT_T_
func testAutocloseFile() {
  // CHECK-LABEL: sil shared @_TFF17default_arguments17testAutocloseFileFT_T_u_KT_SS : $@thin () -> @owned String
  // CHECK: string_literal utf16{{.*}}default_arguments.swift

  // CHECK-LABEL: sil shared @_TFF17default_arguments17testAutocloseFileFT_T_u0_KT_Si : $@thin () -> Int
  // CHECK: integer_literal $Builtin.Int2048, [[@LINE+1]]
  autocloseFile()
}

func testMagicLiterals(file: String = __FILE__,
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
// CHECK-LABEL: sil shared @_TFF17default_arguments25testCallWithMagicLiteralsFT_T_u_KT_T_
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

  // CHECK-LABEL: sil hidden @_TFC17default_arguments3FoocfMS0_FT3intSi6stringSS_S0_ : $@cc(method) @thin (Int, @owned String, @owned Foo) -> @owned Foo
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
func testSelectorCallWithUnnamedPiece(x: Int, y: Int) {
  testMagicLiterals()
}

// Test default arguments in an inherited subobject initializer
class SuperDefArg {
  init(int i: Int = 10) { }
}

// CHECK: sil hidden @_TIFC17default_arguments11SuperDefArgcFMS0_FT3intSi_S0_A_ : $@thin () -> Int

// CHECK-NOT: sil hidden @_TIFC17default_arguments9SubDefArgcFMS0_FT3intSi_S0_A_ : $@thin () -> Int

class SubDefArg : SuperDefArg { }

// CHECK: sil hidden @_TF17default_arguments13testSubDefArgFT_CS_9SubDefArg : $@thin () -> @owned SubDefArg
func testSubDefArg() -> SubDefArg {
  // CHECK: function_ref @_TFC17default_arguments9SubDefArgCfMS0_FT3intSi_S0_
  // CHECK: function_ref @_TIFC17default_arguments11SuperDefArgcFMS0_FT3intSi_S0_A_
  // CHECK: return
  return SubDefArg()
}

// CHECK-NOT: sil hidden @_TIFC17default_arguments9SubDefArgcFMS0_FT3intSi_S0_A_ : $@thin () -> Int

// <rdar://problem/17379550>
func takeDefaultArgUnnamed(_ x: Int = 5) { }

// CHECK-LABEL: sil hidden @_TF17default_arguments25testTakeDefaultArgUnnamed
func testTakeDefaultArgUnnamed(i: Int) {
  // CHECK: bb0([[I:%[0-9]+]] : $Int):
  // CHECK:   [[FN:%[0-9]+]] = function_ref @_TF17default_arguments21takeDefaultArgUnnamedFTSi_T_ : $@thin (Int) -> ()
  // CHECK:   apply [[FN]]([[I]]) : $@thin (Int) -> ()
  takeDefaultArgUnnamed(i)
}

func takeDSOHandle(handle: UnsafeMutablePointer<Void> = __DSO_HANDLE__) { }

// CHECK-LABEL: sil hidden @_TF17default_arguments13testDSOHandleFT_T_
func testDSOHandle() {
  // CHECK: [[DSO_HANDLE:%[0-9]+]] = global_addr @_Tv17default_arguments12__dso_handleGVSs20UnsafeMutablePointerT__ : $*UnsafeMutablePointer<()>
  takeDSOHandle()
}

