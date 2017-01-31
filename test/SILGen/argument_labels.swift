// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -emit-silgen %s | %FileCheck %s

public struct X { }
public struct Y { }

public class Foo {
  func doSomething(x: X, y: Y) { }
  func doSomethingElse(x: X) { }
}

// CHECK-LABEL: sil hidden @_T015argument_labels7testFoo{{[_0-9a-zA-Z]*}}F
func testFoo(foo: Foo, x: X, y: Y) {
  // CHECK: class_method %0 : $Foo, #Foo.doSomething!1 : (Foo) -> (X, Y) -> ()
  foo.doSomething(x: x, y: y)

  // CHECK: class_method %0 : $Foo, #Foo.doSomethingElse!1 : (Foo) -> (X) -> ()
  foo.doSomethingElse(x: x)
}

