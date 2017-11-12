// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -swift-version 3 %s | %FileCheck %s
// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership -swift-version 4 %s | %FileCheck %s

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

protocol Initializable {
  init()
}
struct Generic<T: Initializable> {
  init(_ value: T = T()) {}
}
struct InitializableImpl: Initializable {
  init() {}
}
// CHECK-LABEL: sil hidden @_T025default_arguments_generic17testInitializableyyF
func testInitializable() {
  // The ".init" is required to trigger the crash that used to happen.
  _ = Generic<InitializableImpl>.init()
  // CHECK: function_ref @_T025default_arguments_generic7GenericVACyxGxcfcfA_ : $@convention(thin) <τ_0_0 where τ_0_0 : Initializable> () -> @out τ_0_0
  // CHECK: [[INIT:%.+]] = function_ref @_T025default_arguments_generic7GenericVACyxGxcfC
  // CHECK: apply [[INIT]]<InitializableImpl>({{%.+}}, {{%.+}}) : $@convention(method) <τ_0_0 where τ_0_0 : Initializable> (@in τ_0_0, @thin Generic<τ_0_0>.Type) -> Generic<τ_0_0>
} // CHECK: end sil function '_T025default_arguments_generic17testInitializableyyF'

// Local generic functions with default arguments

// CHECK-LABEL: sil hidden @_T025default_arguments_generic5outeryx1t_tlF : $@convention(thin) <T> (@in T) -> ()
func outer<T>(t: T) {
  func inner1(x: Int = 0) {}

  // CHECK: [[ARG_GENERATOR:%.*]] = function_ref @_T025default_arguments_generic5outeryx1t_tlF6inner1L_ySi1x_tlFfA_ : $@convention(thin) () -> Int
  // CHECK: [[ARG:%.*]] = apply [[ARG_GENERATOR]]() : $@convention(thin) () -> Int
  _ = inner1()

  func inner2(x: Int = 0) { _ = T.self }

  // CHECK: [[ARG_GENERATOR:%.*]] = function_ref @_T025default_arguments_generic5outeryx1t_tlF6inner2L_ySi1x_tlFfA_ : $@convention(thin) <τ_0_0> () -> Int
  // CHECK: [[ARG:%.*]] = apply [[ARG_GENERATOR]]<T>() : $@convention(thin) <τ_0_0> () -> Int
  _ = inner2()
}
