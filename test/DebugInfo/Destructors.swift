// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func markUsed<T>(t: T) {}

class Foo {
  // CHECK: !DISubprogram(name: "deinit", linkageName: "_TFC11Destructors3FooD"
  // CHECK-SAME:          line: [[@LINE-2]]
  // CHECK-SAME:          isDefinition: true
  var x : Int
  init(x: Int) { self.x = x }
  func bar() -> (() -> ()) { return { markUsed(self.x) } }
}

var f = Foo(x: 1)
f.bar()()
