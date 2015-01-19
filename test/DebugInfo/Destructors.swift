// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class Foo {
  // CHECK: _TFC11Destructors3FooD{{.*}} ; [ DW_TAG_subprogram ] [line [[@LINE-1]]] [def] [deinit]
  var x : Int
  init(x: Int) { self.x = x }
  func bar() -> (() -> ()) { return { println(self.x) } }
}

var f = Foo(x: 1)
f.bar()()
