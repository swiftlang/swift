// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

class Foo {
  // CHECK: !MDSubprogram(name: "deinit", linkageName: "_TFC11Destructors3FooD"
  // CHECK-SAME:          line: [[@LINE-2]]
  // CHECK-SAME:          isDefinition: true
  var x : Int
  init(x: Int) { self.x = x }
  func bar() -> (() -> ()) { return { println(self.x) } }
}

var f = Foo(x: 1)
f.bar()()
