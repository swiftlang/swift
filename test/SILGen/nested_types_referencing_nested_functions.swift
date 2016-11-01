// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

do {
  func foo() { bar(2) }
  func bar<T>(_: T) { foo() }

  class Foo {
    // CHECK-LABEL: sil shared @_TFC41nested_types_referencing_nested_functionsL_3FoocfT_S0_ : $@convention(method) (@owned Foo) -> @owned Foo {
    init() {
      foo()
    }
    // CHECK-LABEL: sil shared @_TFC41nested_types_referencing_nested_functionsL_3Foo3zimfT_T_ : $@convention(method) (@guaranteed Foo) -> ()
    func zim() {
      foo()
    }
    // CHECK-LABEL: sil shared @_TFC41nested_types_referencing_nested_functionsL_3Foo4zangurfxT_ : $@convention(method) <T> (@in T, @guaranteed Foo) -> ()
    func zang<T>(_ x: T) {
      bar(x)
    }
    // CHECK-LABEL: sil shared @_TFC41nested_types_referencing_nested_functionsL_3FooD : $@convention(method) (@owned Foo) -> ()
    deinit {
      foo()
    }
  }

  let x = Foo()
  x.zim()
  x.zang(1)
  _ = Foo.zim
  _ = Foo.zang as (Foo) -> (Int) -> ()
  _ = x.zim
  _ = x.zang as (Int) -> ()
}
