// RUN: %target-swift-frontend -primary-file %s -g -emit-ir | %FileCheck %s --check-prefix CHECK-NOOPT
// RUN: %target-swift-frontend -primary-file %s -g -O -emit-ir | %FileCheck %s --check-prefix CHECK-OPT
// REQUIRES: optimized_stdlib

// CHECK-LABEL: define hidden swiftcc i8 @"$s16condfail_message6testitys4Int8VADF"(i8 %0)
// CHECK-OPT: call void @"$s16condfail_message6testitys4Int8VADF.cold.1"()
// CHECK-OPT: define internal void @"$s16condfail_message6testitys4Int8VADF.cold.1"()
// CHECK-NOOPT: call void @llvm.trap(), !dbg [[LOC:![0-9]+]]
// CHECK-OPT: call void @llvm.trap(), !dbg [[LOC:![0-9]+]]

func testit(_ a: Int8) -> Int8 {
  return a + 1
}

// CHECK-NOOPT: [[CALLER_LOC:![0-9]+]] = !DILocation(line: 12, column: 12, scope: !{{.*}})
// CHECK-NOOPT: [[LOC:![0-9]+]] = !DILocation(line: 0, scope: [[FAILURE_FUNC:![0-9]+]], inlinedAt: [[CALLER_LOC]])
// CHECK-NOOPT: [[FAILURE_FUNC]] = distinct !DISubprogram(name: "Swift runtime failure: arithmetic overflow", scope: {{.*}}, file: {{.*}}, type: [[FUNC_TYPE:![0-9]+]], flags: DIFlagArtificial, spFlags: DISPFlagDefinition, {{.*}})
// CHECK-NOOPT: [[FUNC_TYPE]] = !DISubroutineType(types: null)

// CHECK-OPT: [[CALLER_LOC:![0-9]+]] = !DILocation(line: 12, column: 12, scope: !{{.*}})
// CHECK-OPT: [[FAILURE_FUNC:![0-9]+]] = distinct !DISubprogram(name: "$s16condfail_message6testitys4Int8VADF.cold.1", linkageName: "$s16condfail_message6testitys4Int8VADF.cold.1", scope: null, file: {{.*}}, type: [[FUNC_TYPE:![0-9]+]], spFlags: DISPFlagLocalToUnit | DISPFlagDefinition | DISPFlagOptimized, unit: {{.*}}, retainedNodes: {{.*}})
// CHECK-OPT: [[FUNC_TYPE]] = !DISubroutineType(types: {{.*}})
// CHECK-OPT: [[LOC]] = !DILocation(line: 0, scope: [[FAILURE_FUNC:![0-9]+]])
 
