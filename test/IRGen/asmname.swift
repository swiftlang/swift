// RUN: %swift -target x86_64-apple-macosx10.9 %s -emit-ir | FileCheck %s

@asmname("atan2") func atan2test(a: Double, b: Double) -> Double

atan2test(0.0, 0.0)

// CHECK: call double @atan2(double {{.*}}, double {{.*}})
