// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public class Foo {
  // CHECK: !DISubprogram(name: "deinit", linkageName: "_TFC11Destructors3FooD"
  // CHECK-SAME:          line: [[@LINE-2]]
  // CHECK-SAME:          isDefinition: true
  var x : Int64
  init(x: Int64) { self.x = x }
}
