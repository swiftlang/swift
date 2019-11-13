// RUN: %target-swift-frontend -primary-file %s -g -Xllvm -enable-trap-debug-info -emit-ir | %FileCheck %s
// RUN: %target-swift-frontend -primary-file %s -g -Xllvm -enable-trap-debug-info -O -emit-ir | %FileCheck %s
// REQUIRES: optimized_stdlib

// CHECK-LABEL: define hidden swiftcc i8 @"$s16condfail_message6testitys4Int8VADF"(i8)
// CHECK: call void @llvm.trap(), !dbg [[LOC:![0-9]+]]

func testit(_ a: Int8) -> Int8 {
  return a + 1
}

// CHECK: [[CALLER_LOC:![0-9]+]] = !DILocation(line: 9, column: 12, scope: !{{.*}})
// CHECK: [[LOC]] = !DILocation(line: 0, scope: [[FAILURE_FUNC:![0-9]+]], inlinedAt: [[CALLER_LOC]])
// CHECK: [[FAILURE_FUNC]] = distinct !DISubprogram(linkageName: "Swift runtime failure: arithmetic overflow", scope: {{.*}}, file: {{.*}}, type: [[FUNC_TYPE:![0-9]+]], flags: DIFlagArtificial, spFlags: DISPFlagDefinition, {{.*}})
// CHECK: [[FUNC_TYPE]] = !DISubroutineType(types: null)
 
