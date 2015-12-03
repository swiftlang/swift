// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func makeIncrementor(inc : Int64) -> () -> Int64
{
  var sum : Int64 = 0
  // CHECK: define {{.*}}5inner
  func inner() -> Int64 {
    // CHECK: call void @llvm.dbg.declare(metadata %Vs5Int64** %{{.*}}, metadata ![[SUM_CAPTURE:[0-9]+]], metadata ![[DEREF:[0-9]+]])
    // CHECK-DAG: ![[SUM_CAPTURE]] = !DILocalVariable(name: "sum",{{.*}} line: [[@LINE-4]]
    // CHECK-DAG: ![[DEREF]] = !DIExpression(DW_OP_deref
    sum += inc
    return sum
  }
  return inner
}

var incrementor = makeIncrementor(5)
