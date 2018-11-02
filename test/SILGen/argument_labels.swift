
// RUN: %target-swift-emit-silgen -module-name argument_labels -enable-sil-ownership %s | %FileCheck %s

public struct X { }
public struct Y { }

public class Foo {
  func doSomething(x: X, y: Y) { }
  func doSomethingElse(x: X) { }
}

// CHECK-LABEL: sil hidden @$S15argument_labels7testFoo{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG0:%.*]] : @guaranteed $Foo,
func testFoo(foo: Foo, x: X, y: Y) {
  // CHECK: class_method [[ARG0]] : $Foo, #Foo.doSomething!1 : (Foo) -> (X, Y) -> ()
  foo.doSomething(x: x, y: y)

  // CHECK: class_method [[ARG0]] : $Foo, #Foo.doSomethingElse!1 : (Foo) -> (X) -> ()
  foo.doSomethingElse(x: x)
}

