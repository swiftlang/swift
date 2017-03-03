// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests %s -g -emit-ir -o - | %FileCheck %s

class X {
  init (i : Int64) { x = i }
  var x : Int64
}

// CHECK: define {{.*}}ifelseexpr
public func ifelseexpr() -> Int64 {
  var x = X(i:0) 
  // CHECK: [[META:%.*]] = call %swift.type* @_T06return1XCMa()
  // CHECK: [[X:%.*]] = call {{.*}}%T6return1XC* @_T06return1XCACs5Int64V1i_tcfC(
  // CHECK-SAME:                                  i64 0, %swift.type* swiftself [[META]])
  // CHECK:  @swift_rt_swift_release to void (%T6return1XC*)*)(%T6return1XC* [[X]])
  if true {
    x.x += 1
  } else {
    x.x -= 1
  }
  // CHECK:  @swift_rt_swift_release to void (%T6return1XC*)*)(%T6return1XC* [[X]])
  // CHECK-SAME:                    , !dbg ![[RELEASE:.*]]

  // The ret instruction should be in the same scope as the return expression.
  // CHECK:  ret{{.*}}, !dbg ![[RELEASE]]
  return x.x // CHECK: ![[RELEASE]] = !DILocation(line: [[@LINE]], column: 3
}

