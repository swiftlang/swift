// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

func use<T>(_: T) {}

// CHECK-LABEL: sil hidden @_T022generic_local_property3fooyx1x_Si1ytlF
func foo<T>(x: T, y: Int) {
  var mutable: Int {
    get {
      use(x)
      return y
    }
    set {
      use(x)
    }
  }

  var readonly: Int {
    get {
      use(x)
      return y
    }
  }

  // CHECK-LABEL: function_ref (foo<A> (x : A, y : Int) -> ()).(readonly #1).getter
  _ = readonly
  // CHECK-LABEL: function_ref (foo<A> (x : A, y : Int) -> ()).(mutable #1).getter
  _ = mutable
  // CHECK-LABEL: function_ref (foo<A> (x : A, y : Int) -> ()).(mutable #1).setter
  mutable = y
}
