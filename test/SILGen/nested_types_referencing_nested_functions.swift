// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

do {
  func foo() { bar(2) }
  func bar<T>(_: T) { foo() }

  class Foo {
    // CHECK-LABEL: sil shared @_T0025nested_types_referencing_A10_functions3FooL_CACycfc : $@convention(method) (@owned Foo) -> @owned Foo {
    init() {
      foo()
    }
    // CHECK-LABEL: sil shared @_T0025nested_types_referencing_A10_functions3FooL_C3zimyyF : $@convention(method) (@guaranteed Foo) -> ()
    func zim() {
      foo()
    }
    // CHECK-LABEL: sil shared @_T0025nested_types_referencing_A10_functions3FooL_C4zangyxlF : $@convention(method) <T> (@in T, @guaranteed Foo) -> ()
    func zang<T>(_ x: T) {
      bar(x)
    }
    // CHECK-LABEL: sil shared @_T0025nested_types_referencing_A10_functions3FooL_CfD : $@convention(method) (@owned Foo) -> ()
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
