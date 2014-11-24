// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir -g -o - | FileCheck %s
class Foo {
  var x : Int
  // CHECK: _TFC11Destructors3FooD{{.*}} ; [ DW_TAG_subprogram ] [line 2] [def] [deinit]
  init(x: Int) { self.x = x }
  func bar() -> (() -> ()) { return { println(self.x) } }
}

var f = Foo(x: 1)
f.bar()()
