// RUN: %target-swift-frontend %s -g -emit-ir -o - | FileCheck %s

class X {
  init (i : Int64) { x = i }
  var x : Int64
}

// CHECK: define {{.*}}ifelseexpr
public func ifelseexpr() -> Int64 {
  var x = X(i:0); 
  // CHECK: [[META:%.*]] = call %swift.type* @_TMaC6return1X()
  // CHECK: [[X:%.*]] = call %C6return1X* @_TFC6return1XCfT1iVs5Int64_S0_(i64 0, %swift.type* [[META]])
  // CHECK:  @swift_release to void (%C6return1X*)*)(%C6return1X* [[X]])
  if true {
    x.x++; 
  } else {
    x.x--;
  }
  // CHECK:  @swift_release to void (%C6return1X*)*)(%C6return1X* [[X]])
  // CHECK:  @swift_release to void (%C6return1X*)*)(%C6return1X* [[X]]) {{.*}}, !dbg ![[RET:.*]]
  // CHECK:  ret{{.*}}, !dbg ![[RET]]
  return x.x; // CHECK: ![[RET]] = !DILocation(line: [[@LINE]],
}

