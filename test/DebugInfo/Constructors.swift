// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s
struct Foo {
  // Allocating constructor - should have no line table info.
  // CHECK: !MDSubprogram(name: "init", linkageName: "_TFV12Constructors3FooCfMS0_FT1xSi_S0_",
  // CHECK-SAME:          line: [[@LINE+3]]
  // CHECK-NOT:           scopeLine: 0
  // CHECK-SAME:          isDefinition: true
  init(x: Int) {}
  func bar(x: Int) {}
}

var f = Foo(x: 1)
f.bar(2)
