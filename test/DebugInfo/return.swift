// RUN: %target-swift-frontend %s -g -emit-ir -o - | %FileCheck %s

class X {
  init (i : Int64) { x = i }
  var x : Int64
}

// CHECK: define {{.*}}ifelseexpr
public func ifelseexpr() -> Int64 {
  var x = X(i:0)
  // CHECK: [[ALLOCA:%.*]] = alloca %T6return1XC*
  // CHECK: [[TMP:%.*]] = call swiftcc %swift.metadata_response @"$S6return1XCMa"(
  // CHECK: [[META:%.*]] = extractvalue %swift.metadata_response [[TMP]], 0
  // CHECK: [[X:%.*]] = call {{.*}}%T6return1XC* @"$S6return1XC1iACs5Int64V_tcfC"(
  // CHECK-SAME:                                  i64 0, %swift.type* swiftself [[META]])
  // CHECK:  store %T6return1XC* [[X]], %T6return1XC** [[ALLOCA]]
  // CHECK:  @swift_release to void (%T6return1XC*)*)(%T6return1XC* [[X]])
  if true {
    x.x += 1
  } else {
    x.x -= 1
  }
  // CHECK:  [[X:%.*]] = load %T6return1XC*, %T6return1XC** [[ALLOCA]]
  // CHECK:  @swift_release to void (%T6return1XC*)*)(%T6return1XC* [[X]])
  // CHECK-SAME:                    , !dbg ![[RELEASE:.*]]

  // The ret instruction should be in the same scope as the return expression.
  // CHECK:  ret{{.*}}, !dbg ![[RELEASE]]
  return x.x // CHECK: ![[RELEASE]] = !DILocation(line: [[@LINE]], column: 3
}

