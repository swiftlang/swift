// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -primary-file %s -Xllvm -sil-print-types -Xllvm -sil-disable-pass=temp-lvalue-elimination -emit-sil -O -g | %FileCheck %s --check-prefix CHECK-SIL
// RUN: %target-swift-frontend -target %target-swift-5.1-abi-triple -primary-file %s -Xllvm -sil-disable-pass=temp-lvalue-elimination -emit-irgen -O -g | %FileCheck %s

// REQUIRES: CPU=arm64 || CPU=x86_64 || CPU=arm64e

protocol External {
  func use(str: String);
  func decode<T>(_: T.Type) -> T
}

struct Data {
  var a: String
  var b: String
}

func test(cond: Int, external: External) async {
  // CHECK-DAG: ![[VAR:[0-9]+]] = !DILocalVariable(name: "data", scope: !{{[0-9]+}}, file: !{{[0-9]+}}, line: [[# @LINE + 1]], type: !{{[0-9]+}})
  let data: Data
  switch cond {
  // CHECK-DAG: ![[LOC1:[0-9]+]] = !DILocation(line: [[# @LINE + 1]], column: 12, scope: !{{.*}})
  case 42: data = external.decode(Data.self)
  // CHECK-DAG: ![[LOC2:[0-9]+]] = !DILocation(line: [[# @LINE + 1]], column: 12, scope: !{{.*}})
  default: data = external.decode(Data.self)
  }
  external.use(str: data.a)
  external.use(str: data.b)
}

// CHECK-SIL: debug_value %{{.*}} : $String, let, (name "data", {{.*}}), type $Data, expr op_fragment:#Data.a
// CHECK-SIL: debug_value %{{.*}} : $String, let, (name "data", {{.*}}), type $Data, expr op_fragment:#Data.b
// CHECK-SIL: debug_value %{{.*}} : $String, let, (name "data", {{.*}}), type $Data, expr op_fragment:#Data.a
// CHECK-SIL: debug_value %{{.*}} : $String, let, (name "data", {{.*}}), type $Data, expr op_fragment:#Data.b

// CHECK-DAG: #dbg_value{{.*}} ![[VAR:[0-9]+]], !DIExpression(DW_OP_LLVM_fragment, 192, 64){{.*}} ![[LOC1]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 128, 64){{.*}} ![[LOC1]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 64, 64){{.*}} ![[LOC1]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 0, 64){{.*}} ![[LOC1]]
//
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 192, 64){{.*}} ![[LOC2]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 128, 64){{.*}} ![[LOC2]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 64, 64){{.*}} ![[LOC2]]
// CHECK-DAG: #dbg_value{{.*}} ![[VAR]], !DIExpression(DW_OP_LLVM_fragment, 0, 64){{.*}} ![[LOC2]]
