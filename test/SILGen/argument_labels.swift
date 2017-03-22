// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

public struct X { }
public struct Y { }

public class Foo {
  func doSomething(x: X, y: Y) { }
  func doSomethingElse(x: X) { }
}

// CHECK-LABEL: sil hidden @_T015argument_labels7testFoo{{[_0-9a-zA-Z]*}}F
// CHECK: bb0([[ARG0:%.*]] : $Foo,
func testFoo(foo: Foo, x: X, y: Y) {
  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: class_method [[BORROWED_ARG0]] : $Foo, #Foo.doSomething!1 : (Foo) -> (X, Y) -> ()
  foo.doSomething(x: x, y: y)

  // CHECK: [[BORROWED_ARG0:%.*]] = begin_borrow [[ARG0]]
  // CHECK: class_method [[BORROWED_ARG0]] : $Foo, #Foo.doSomethingElse!1 : (Foo) -> (X) -> ()
  foo.doSomethingElse(x: x)
}

