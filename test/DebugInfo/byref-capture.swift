// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func makeIncrementor(_ inc : Int64) -> () -> Int64
{
  var sum : Int64 = 0
  // CHECK: define {{.*}}5inner
  func inner() -> Int64 {
    // CHECK: call void @llvm.dbg.declare(metadata %Ts5Int64V**
    // CHECK-SAME:                        metadata ![[SUM_CAPTURE:[0-9]+]],
    // CHECK-SAME:                        metadata ![[EMPTY:.*]])
    // CHECK: ![[EMPTY]] = !DIExpression()
    // CHECK: ![[INOUTTY:[0-9]+]] = !DICompositeType({{.*}}identifier: "_T0s5Int64VzD"
    //                                                              ^ inout type.
    // CHECK: ![[SUM_CAPTURE]] = !DILocalVariable(name: "sum", arg: 1,
    // CHECK-SAME:     line: [[@LINE-10]], type: ![[INOUTTY]]
    sum += inc
    return sum
  }
  return inner
}

var incrementor = makeIncrementor(5)
incrementor()
