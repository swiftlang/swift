// RUN: %target-swift-frontend -emit-silgen %s | FileCheck %s

func foo<T: IntegerLiteralConvertible>(x: T = 0) { }

struct Zim<T: IntegerLiteralConvertible> {
  init(x: T = 0) { }
  init<U: FloatLiteralConvertible>(x: T = 0, y: U = 0.5) { }

  static func zim(x: T = 0) { }
  static func zang<U: FloatLiteralConvertible>(_ x: T = 0,
                                               y: U = 0.5) { }
}

// CHECK-LABEL: sil hidden @_TF25default_arguments_generic3barFT_T_ : $@thin () -> () {
func bar() {
  // CHECK: [[FOO_DFLT:%.*]] = function_ref @_TIF25default_arguments_generic3fooUSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__FT1xQ__T_A_
  // CHECK: apply [[FOO_DFLT]]<Int, Int>
  foo()
  // CHECK: [[ZIM_DFLT:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim3zimUSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__FMGS0_Q__FT1xQ__T_A_ 
  // CHECK: apply [[ZIM_DFLT]]<Int, Int>
  Zim.zim()
  // CHECK: [[ZANG_DFLT_0:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zangUSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__FMGS0_Q__USs23FloatLiteralConvertible_USs31_BuiltinFloatLiteralConvertible__FTQd__1yQ__T_A_
  // CHECK: apply [[ZANG_DFLT_0]]<Int, Int, Double, Double>
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zangUSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__FMGS0_Q__USs23FloatLiteralConvertible_USs31_BuiltinFloatLiteralConvertible__FTQd__1yQ__T_A0_
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Int, Double, Double>
  Zim.zang()
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zangUSs25IntegerLiteralConvertible_USs33_BuiltinIntegerLiteralConvertible__FMGS0_Q__USs23FloatLiteralConvertible_USs31_BuiltinFloatLiteralConvertible__FTQd__1yQ__T_A0_
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Int, Double, Double>
  Zim.zang(22)
}
