// RUN: %target-swift-frontend -emit-silgen -swift-version 3 %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -swift-version 4 %s | %FileCheck %s

func foo<T: ExpressibleByIntegerLiteral>(_: T.Type, x: T = 0) { }

struct Zim<T: ExpressibleByIntegerLiteral> {
  init(x: T = 0) { }
  init<U: ExpressibleByFloatLiteral>(_ x: T = 0, y: U = 0.5) { }

  static func zim(x: T = 0) { }
  static func zang<U: ExpressibleByFloatLiteral>(_: U.Type, _ x: T = 0, y: U = 0.5) { }
}

// CHECK-LABEL: sil hidden @_TF25default_arguments_generic3barFT_T_ : $@convention(thin) () -> () {
func bar() {
  // CHECK: [[FOO_DFLT:%.*]] = function_ref @_TIF25default_arguments_generic3foo
  // CHECK: apply [[FOO_DFLT]]<Int>
  foo(Int.self)
  // CHECK: [[ZIM_DFLT:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim3zim
  // CHECK: apply [[ZIM_DFLT]]<Int>
  Zim<Int>.zim()
  // CHECK: [[ZANG_DFLT_0:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zang
  // CHECK: apply [[ZANG_DFLT_0]]<Int, Double>
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zang
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Double>
  Zim<Int>.zang(Double.self)
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_TIZFV25default_arguments_generic3Zim4zang
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Double>
  Zim<Int>.zang(Double.self, 22)
}
