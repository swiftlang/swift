// RUN: %target-build-swift %s -g -emit-ir -o - | FileCheck %s
class X {
  init (i : Int) { x = i }
  var x : Int
}

// CHECK: define {{.*}}ifelseexpr
func ifelseexpr() -> Int {
  var x = X(i:0); 
  if true {
    // CHECK:  @swift_release
    x.x++; 
  } else {
    x.x--;
  }
  // CHECK:  @swift_release {{.*}}1X{{.*}}, !dbg ![[RET:.*]]
  // CHECK:  ret{{.*}}, !dbg ![[RET]]
  return x.x; // CHECK: ![[RET]] = !MDLocation(line: [[@LINE]],
}
