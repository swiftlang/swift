// RUN: %target-swift-emit-silgen %s | %FileCheck %s

func use<T>(_: T) {}

// CHECK-LABEL: sil hidden [ossa] @$s22generic_local_property3foo1x1yyx_SitlF
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
