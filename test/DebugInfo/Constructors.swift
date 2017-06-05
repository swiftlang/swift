// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s
struct Foo {
  // Allocating constructor - should have no line table info.
  // CHECK: !DISubprogram(name: "init", linkageName: "_T012Constructors3FooVACs5Int64V1x_tcfC",
  // CHECK-SAME:          line: [[@LINE+3]]
  // CHECK-NOT:           scopeLine: 0
  // CHECK-SAME:          isDefinition: true
  init(x: Int64) {}
  func bar(_ x: Int64) {}
}

var f = Foo(x: 1)
f.bar(2)
