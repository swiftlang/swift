// RUN: %target-swift-frontend %s -emit-ir -g -o - \
// RUN:    -module-name M  -target %target-swift-5.1-abi-triple \
// RUN:    -parse-as-library | %FileCheck %s

// REQUIRES: concurrency

func use<T>(_ t: T) {}
func forceSplit() async {
}
func withGenericArg<T>(_ msg: T) async {
  // This odd debug info is part of a contract with CoroSplit/CoroFrame to fix
  // this up after coroutine splitting.
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYalF"(ptr swiftasync %0
  // CHECK-DAG: #dbg_declare(ptr %0, ![[MSG:[0-9]+]], !DIExpression({{.*}}DW_OP_plus_uconst, {{.*}}DW_OP_deref),
  // CHECK-DAG: #dbg_declare(ptr %0, ![[TAU:[0-9]+]], !DIExpression({{.*}}DW_OP_plus_uconst,

  await forceSplit()
  // CHECK-LABEL: {{^define .*}} @"$s1M14withGenericArgyyxYalFTQ0_"(ptr swiftasync %0)
  // CHECK-DAG: #dbg_declare(ptr %0, ![[MSG_R:[0-9]+]], !DIExpression({{.*}}DW_OP_plus_uconst, {{[0-9]+}}, DW_OP_deref),
  // CHECK-DAG: #dbg_declare(ptr %0, ![[TAU_R:[0-9]+]], !DIExpression({{.*}}DW_OP_deref, DW_OP_plus_uconst, {{[0-9]+}}),
  use(msg)
}
// CHECK-LABEL: {{^define }}
@main struct Main {
  static func main() async {
    await withGenericArg("hello (asynchronously)")
  }
}
// CHECK-DAG: ![[TAU]] = !DILocalVariable(name: "$\CF\84_0_0",
// CHECK-DAG: ![[MSG]] = !DILocalVariable(name: "msg", arg: 1,
// CHECK-DAG: ![[TAU_R]] = !DILocalVariable(name: "$\CF\84_0_0",
// CHECK-DAG: ![[MSG_R]] = !DILocalVariable(name: "msg", arg: 1,

