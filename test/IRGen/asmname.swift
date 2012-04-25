// RUN: %swift -triple x86_64-apple-darwin10 -I %S/.. %s -emit-llvm | FileCheck %s

func [asmname="atan2"] atan2test(a : Double, b : Double) -> Double

atan2test(0.0, 0.0)

// CHECK: call double @atan2(double %{{.*}}, double %{{.*}})
