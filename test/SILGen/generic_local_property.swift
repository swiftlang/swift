// RUN: %target-swift-frontend -emit-silgen -enable-sil-ownership %s | %FileCheck %s

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

  // CHECK-LABEL: function_ref getter of readonly #1 in foo<A>(x:y:)
  _ = readonly
  // CHECK-LABEL: function_ref getter of mutable #1 in foo<A>(x:y:)
  _ = mutable
  // CHECK-LABEL: function_ref setter of mutable #1 in foo<A>(x:y:)
  mutable = y
}
