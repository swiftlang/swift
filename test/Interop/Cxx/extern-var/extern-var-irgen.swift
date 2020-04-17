// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -I %S/Inputs -emit-ir -o %t/extern-var.ir -Xfrontend -enable-cxx-interop
// RUN: %FileCheck < %t/extern-var.ir %s

// CHECK: @counter = external global i32, align 4

// CHECK: define hidden swiftcc i32 @"$s4main10getCounters5Int32VyF"() #0
// CHECK: load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @counter to %Ts5Int32V*), i32 0, i32 0), align 4

// CHECK: define hidden swiftcc void @"$s4main10setCounteryys5Int32VF"(i32 %0) #0
// CHECK: store i32 %0, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @counter to %Ts5Int32V*), i32 0, i32 0), align 4

// CHECK: define hidden swiftcc i32 @"$s4main20getNamespacedCounters5Int32VyF"() #0
//FIXME mangle non-top-level var names to prevent name collisions and check:
// load i32, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @Namespaced.counter to %Ts5Int32V*), i32 0, i32 0), align 4
// CHECK: define hidden swiftcc void @"$s4main20setNamespacedCounteryys5Int32VF"(i32 %0) #0
//FIXME mangle non-top-level var names to prevent name collisions and check:
// store i32 %0, i32* getelementptr inbounds (%Ts5Int32V, %Ts5Int32V* bitcast (i32* @Namespaced.counter to %Ts5Int32V*), i32 0, i32 0), align 4

import ExternVar

func getCounter() -> CInt {
  return counter
}

func setCounter(_ c: CInt) {
  counter = c
}

func getNamespacedCounter() -> CInt {
  return Namespaced.counter
}

func setNamespacedCounter(_ c: CInt) {
  Namespaced.counter = c
}
