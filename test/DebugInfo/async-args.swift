// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M -enable-experimental-concurrency | %FileCheck %s
// REQUIRES: concurrency

func use<T>(_ t: T) {}
func forceSplit() async {
}
func withGenericArg<T>(_ msg: T) async {
  // This odd debug info is part of a contract with CoroSplit/CoroFrame to fix
  // this up after coroutine splitting.
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYlF"(%swift.task* %0, %swift.executor* %1, %swift.context* %2)
  // CHECK: call void @llvm.dbg.declare(metadata %swift.context** %[[ALLOCA:[^,]*]],
  // CHECK-SAME:   metadata ![[MSG:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))
  // CHECK: call void @llvm.dbg.declare(metadata %swift.context** %[[ALLOCA]],
  // CHECK-SAME:   metadata ![[TAU:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK: store %swift.context* %2, %swift.context** %[[ALLOCA]], align

  await forceSplit()
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYlF.resume.0"(i8* %0, i8* %1, i8* %2)
  // CHECK: store i8* %2, i8** %[[ALLOCA:.*]], align
  // CHECK: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-SAME:   metadata ![[TAU_R:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET:[0-9]+]],
  // CHECK-SAME:     DW_OP_plus_uconst, {{[0-9]+}}))
  // CHECK: call void @llvm.dbg.declare(metadata i8** %[[ALLOCA]],
  // CHECK-SAME:   metadata ![[MSG_R:[0-9]+]], metadata !DIExpression(
  // CHECK-SAME:     DW_OP_deref, DW_OP_plus_uconst, [[OFFSET]],
  // CHECK-SAME:     DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref))

  use(msg)
}
// CHECK-LABEL: {{^define }}
runAsyncAndBlock {
  await withGenericArg("hello (asynchronously)")
}
// CHECK: ![[MSG]] = !DILocalVariable(name: "msg", arg: 1,
// CHECK: ![[TAU]] = !DILocalVariable(name: "$\CF\84_0_0",
// CHECK: ![[TAU_R]] = !DILocalVariable(name: "$\CF\84_0_0",
// CHECK: ![[MSG_R]] = !DILocalVariable(name: "msg", arg: 1,

