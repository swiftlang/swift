// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

protocol P {}

class Foo {
  var x: Foo!
  var p: P!

  // CHECK-LABEL: {{.*3Foo.*3foo.*}}
  // CHECK-NOT: unchecked_{{.*}}cast {{.*}} Optional{{.*}} to Optional
  func foo() -> Foo? {
    return x
  }
  // CHECK-LABEL: {{.*3Foo.*3poo.*}}
  // CHECK-NOT: unchecked_{{.*}}cast {{.*}} Optional{{.*}} to Optional
  func poo() -> P? {
    return p
  }

  // CHECK-LABEL: {{.*3Foo.*3bar.*}}
  // CHECK-NOT: unchecked_{{.*}}cast {{.*}} Optional{{.*}} to Optional
  func bar() -> Foo? {
    var x2 = x
  }
  // CHECK-LABEL: {{.*3Foo.*3par.*}}
  // CHECK-NOT: unchecked_{{.*}}cast {{.*}} Optional{{.*}} to Optional
  func par(p3: P) -> P? {
    var p2 = p
    p2! = p3
    p2? = p3
  }
}
