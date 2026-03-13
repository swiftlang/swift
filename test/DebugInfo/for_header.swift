// RUN: %target-swift-frontend -emit-irgen -module-name for_each -g %s | %FileCheck %s

// Make sure we have debug info locations for the header and end of the loop
// for the corresponding branches.
// CHECK-LABEL: define hidden swiftcc void @"$s8for_each3fooyySaySiGF"(ptr %0)
func foo(_ xs: [Int]) {
  for _ in xs {
    // CHECK: [[LOOP_HEADER:[0-9]+]]:
    // CHECK:   br i1 {{.*}}, label {{.*}}, label {{.*}}, !dbg [[HEADER_LOC:![0-9]+]]
    //
    // CHECK:   br label %[[LOOP_HEADER]], !dbg [[END_LOC:![0-9]+]]
  }
}
// CHECK-DAG: [[HEADER_LOC]] = !DILocation(line: [[@LINE-7]], column: 3
// CHECK-DAG: [[END_LOC]] = !DILocation(line: [[@LINE-3]], column: 3
