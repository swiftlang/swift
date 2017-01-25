// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -swift-version 3 %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen -swift-version 4 %s | %FileCheck %s

func foo<T: ExpressibleByIntegerLiteral>(_: T.Type, x: T = 0) { }

struct Zim<T: ExpressibleByIntegerLiteral> {
  init(x: T = 0) { }
  init<U: ExpressibleByFloatLiteral>(_ x: T = 0, y: U = 0.5) { }

  static func zim(x: T = 0) { }
  static func zang<U: ExpressibleByFloatLiteral>(_: U.Type, _ x: T = 0, y: U = 0.5) { }
}

// CHECK-LABEL: sil hidden @_T025default_arguments_generic3baryyF : $@convention(thin) () -> () {
func bar() {
  // CHECK: [[FOO_DFLT:%.*]] = function_ref @_T025default_arguments_generic3foo
  // CHECK: apply [[FOO_DFLT]]<Int>
  foo(Int.self)
  // CHECK: [[ZIM_DFLT:%.*]] = function_ref @_T025default_arguments_generic3ZimV3zim
  // CHECK: apply [[ZIM_DFLT]]<Int>
  Zim<Int>.zim()
  // CHECK: [[ZANG_DFLT_0:%.*]] = function_ref @_T025default_arguments_generic3ZimV4zang{{.*}}A0_
  // CHECK: apply [[ZANG_DFLT_0]]<Int, Double>
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_T025default_arguments_generic3ZimV4zang{{.*}}A1_
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Double>
  Zim<Int>.zang(Double.self)
  // CHECK: [[ZANG_DFLT_1:%.*]] = function_ref @_T025default_arguments_generic3ZimV4zang{{.*}}A1_
  // CHECK: apply [[ZANG_DFLT_1]]<Int, Double>
  Zim<Int>.zang(Double.self, 22)
}
