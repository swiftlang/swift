// RUN: %target-swift-frontend -O -primary-file %s -disable-availability-checking -emit-ir | %FileCheck %s

// CHECK: define{{( dllexport)?}}{{( protected)?}} i32 @main{{.*}} {
// CHECK:      store ptr %{{[0-9]+}}, ptr @"$s13rdar1140137091xQrvp"
actor Actor {}
let x: some Actor = Actor()

