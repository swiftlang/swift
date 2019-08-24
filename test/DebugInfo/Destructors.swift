// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

public class Foo {
  // CHECK: !DISubprogram(name: "deinit", linkageName: "$s11Destructors3FooCfD"
  // CHECK-SAME:          line: [[@LINE-2]]
  // CHECK-SAME:          DISPFlagDefinition
  var x : Int64
  init(x: Int64) { self.x = x }
}
