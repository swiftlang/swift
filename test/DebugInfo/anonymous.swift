// RUN: %target-swift-frontend %s -Xllvm -sil-print-types -emit-sil -g -o - | %FileCheck --check-prefix=SIL %s
// RUN: %target-swift-frontend -primary-file %s -emit-ir -g -o - | %FileCheck %s

// SIL: debug_value %1 : $*T, let, name "_0", argno 1, expr op_deref
// SIL: debug_value %2 : $*T, let, name "_1", argno 2, expr op_deref
// SIL: debug_value %3 : $*T, let, name "_2", argno 3, expr op_deref
// SIL: debug_value %4 : $*T, let, name "x4", argno 4, expr op_deref
// CHECK: !DILocalVariable(name: "_0", arg: 1
// CHECK: !DILocalVariable(name: "_1", arg: 2
// CHECK: !DILocalVariable(name: "_2", arg: 3
// CHECK: !DILocalVariable(name: "x4", arg: 4

public func fourth<T>(_: T, _: T, _: T, x4 : T) -> T {
  return x4
}
