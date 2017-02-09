// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -emit-ir -g -o - | %FileCheck %s

public class Foo {
  // CHECK: !DISubprogram(name: "deinit", linkageName: "_T011Destructors3FooCfD"
  // CHECK-SAME:          line: [[@LINE-2]]
  // CHECK-SAME:          isDefinition: true
  var x : Int64
  init(x: Int64) { self.x = x }
}
