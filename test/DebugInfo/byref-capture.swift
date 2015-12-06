// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func makeIncrementor(inc : Int64) -> () -> Int64
{
  var sum : Int64 = 0
  // CHECK: define {{.*}}5inner
  func inner() -> Int64 {
    // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64**
    // CHECK-SAME:                        metadata ![[SUM_CAPTURE:[0-9]+]],
    // CHECK-SAME:                        metadata ![[DEREF:[0-9]+]])
    // CHECK: ![[DEREF]] = !DIExpression(DW_OP_deref)
    // CHECK: ![[SUM_CAPTURE]] = !DILocalVariable(name: "sum",
    // CHECK-SAME:                                line: [[@LINE-8]]
    sum += inc
    return sum
  }
  return inner
}

var incrementor = makeIncrementor(5)
incrementor()
