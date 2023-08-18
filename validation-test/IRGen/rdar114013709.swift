// RUN: %target-swift-frontend -O -primary-file %s -disable-availability-checking -emit-ir | %FileCheck %s

// CHECK: define{{( dllexport)?}}{{( protected)?}} i32 @main{{.*}} {
// CHECK:      store %swift.refcounted* %{{[0-9]+}},
// CHECK-SAME:                          %swift.refcounted**
// CHECK-SAME:                          bitcast (
// CHECK-SAME:                            %T13rdar1140137095ActorC** @"$s13rdar1140137091xQrvp"
// CHECK-SAME:                            to %swift.refcounted**)
actor Actor {}
let x: some Actor = Actor()

