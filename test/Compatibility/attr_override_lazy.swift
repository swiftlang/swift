// RUN: %target-swift-frontend -swift-version 4 -Xllvm -sil-print-types -emit-silgen %s -verify | %FileCheck %s

class Base {
  var foo: Int { return 0 } // expected-note {{attempt to override property here}}
  var bar: Int = 0 // expected-note {{attempt to override property here}}
}

class Sub : Base {
  lazy override var foo: Int = 1 // expected-warning {{cannot override with a stored property 'foo'}}
  lazy override var bar: Int = 1 // expected-warning {{cannot override with a stored property 'bar'}}
  func test() -> Int {
    // CHECK-LABEL: sil {{.*}}@$s18attr_override_lazy3SubC4testSiyF
    // CHECK: class_method %0 : $Sub, #Sub.foo!getter
    // CHECK: class_method %0 : $Sub, #Sub.bar!getter
    // CHECK: // end sil function '$s18attr_override_lazy3SubC4testSiyF'
    return foo + bar // no ambiguity error here
  }
}

// CHECK-LABEL: sil_vtable Sub {
// CHECK: #Base.foo!getter: (Base) -> () -> Int : {{.*}} // Sub.foo.getter
// CHECK: #Base.bar!getter: (Base) -> () -> Int : {{.*}} // Sub.bar.getter
// CHECK: #Base.bar!setter: (Base) -> (Int) -> () : {{.*}} // Sub.bar.setter
// CHECK: #Base.bar!modify: (Base) -> {{.*}} : {{.*}} // Sub.bar.modify
// CHECK: }
