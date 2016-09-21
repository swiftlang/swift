// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s

func makeIncrementor(_ inc : Int64) -> () -> Int64
{
  var sum : Int64 = 0
  // CHECK: define {{.*}}5inner
  func inner() -> Int64 {
    // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64**
    // CHECK-SAME:                        metadata ![[SUM_CAPTURE:[0-9]+]],
    // CHECK-SAME:                        metadata ![[EMPTY:.*]])
    // CHECK: ![[EMPTY]] = !DIExpression()
    // CHECK: ![[SUM_CAPTURE]] = !DILocalVariable(name: "sum", arg: 1,
    // CHECK-SAME:     line: [[@LINE-8]], type: ![[INOUTTY:[0-9]+]]
    // CHECK: ![[INOUTTY]] = !DICompositeType({{.*}}identifier: "_TtRVs5Int64"
    //                                                              ^ inout type.
    sum += inc
    return sum
  }
  return inner
}

var incrementor = makeIncrementor(5)
incrementor()
