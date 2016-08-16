// RUN: %target-swift-frontend -emit-sil -parse-as-library -O -enable-experimental-property-behaviors %s | %FileCheck %s

@inline(never) func whatever<T>() -> T { fatalError("") }

protocol lazzy {
  associatedtype Value
}
extension lazzy {
  var value: Value {
    return whatever()
  }
}

struct Foo {
  var x: Int __behavior lazzy

  init() {}
}

public func exercise() {
  _ = Foo().x
}

// CHECK-LABEL: sil @_TF28specialize_property_behavior8exerciseFT_T_
// CHECK:         function_ref @_TTSg5Si___TF28specialize_property_behavior8whateverurFT_x 
