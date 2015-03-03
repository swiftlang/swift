// RUN: %target-swift-frontend %s -emit-ir -g -o - | FileCheck %s

func makeIncrementor(inc : Int) -> () -> Int
{
  var sum = 0
  // CHECK: define {{.*}}5inner
  func inner() -> Int {
    // CHECK: call void @llvm.dbg.declare(metadata %Si** %{{.*}}, metadata ![[SUM_CAPTURE:[0-9]+]], metadata ![[DEREF:[0-9]+]])
    // CHECK-DAG: ![[SUM_CAPTURE]] = !MDLocalVariable(tag: DW_TAG_arg_variable, name: "sum",{{.*}} line: [[@LINE-4]]
    // CHECK-DAG: ![[DEREF]] = !MDExpression(DW_OP_deref
    sum += inc
    return sum
  }
  return inner
}

var incrementor = makeIncrementor (5)
var a = 5
var more_than_a = incrementor ()
println ("a was \(a) and more_than_a was \(more_than_a)")
