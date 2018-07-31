// RUN: %target-swift-frontend %s -emit-ir -g -o - | %FileCheck %s --check-prefixes CHECK,DWARF-CHECK
// RUN: %target-swift-frontend %s -emit-ir -g -debug-info-format=codeview -o - \
// RUN:   | %FileCheck %s --check-prefixes CHECK,CV-CHECK

public func foo(_ a: Int64, _ b: Int64) -> Int64 {      // line 5
  // CHECK: sdiv i64 {{.*}}, !dbg ![[DIV:[0-9]+]]
  // CHECK: call {{.*}} @llvm.sadd.with.overflow{{.*}}, !dbg ![[ADD:[0-9]+]]
  let c = a + b / a;                                    // line 8
  // CHECK: icmp slt i64 {{.*}}, !dbg ![[SLT:[0-9]+]]
  if (c < b) {                                          // line 10
    // CHECK: call {{.*}} @llvm.ssub.with.overflow{{.*}}, !dbg ![[SUB:[0-9]+]]
    return a - b                                        // line 12
  }
  return c
}

// CHECK-DAG: !DISubprogram(name: "foo",{{.*}} line: 5,
// DWARF-CHECK-DAG: !DILexicalBlock({{.*}}, line: 5, column: 50)
// DWARF-CHECK-DAG: ![[DIV]] = !DILocation(line: 8, column: 17,
// DWARF-CHECK-DAG: ![[ADD]] = !DILocation(line: 8, column: 13,
// DWARF-CHECK-DAG: ![[SLT]] = !DILocation(line: 10, column: 9,
// DWARF-CHECK-DAG: !DILexicalBlock({{.*}}, line: 10, column: 14)
// DWARF-CHECK-DAG: ![[SUB]] = !DILocation(line: 12, column: 14,

// CV-CHECK-DAG: !DILexicalBlock({{.*}}, line: 5)
// CV-CHECK-DAG: ![[DIV]] = !DILocation(line: 8, scope:
// CV-CHECK-DAG: ![[ADD]] = !DILocation(line: 8, scope:
// CV-CHECK-DAG: ![[SLT]] = !DILocation(line: 10, scope:
// CV-CHECK-DAG: !DILexicalBlock({{.*}}, line: 10)
// CV-CHECK-DAG: ![[SUB]] = !DILocation(line: 12, scope:
