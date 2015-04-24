// RUN: %target-swift-frontend %s -emit-ir | FileCheck %s

// REQUIRES: CPU=i386_or_x86_64

@asmname("atan2") func atan2test(a: Double, _ b: Double) -> Double

atan2test(0.0, 0.0)

// CHECK: call double @atan2(double {{.*}}, double {{.*}})
