// RUN: %target-swift-frontend -O -primary-file %s -emit-ir | FileCheck %s

// Check if the hashValue and == for an enum (without payload) are generated and
// check if that functions are compiled in an optimal way.

enum E {
  case E0
  case E1
  case E2
  case E3
}

// Check if the hashValue getter can be compiled to a simple zext instruction.

// CHECK-LABEL:define hidden i{{.*}} @_TFO12enum_derived1Eg9hashValueSi(i2)
// CHECK: %1 = zext i2 %0 to i{{.*}}
// CHECK: ret i{{.*}} %1

// Check if the == comparison can be compiled to a simple icmp instruction.

// CHECK-LABEL:define hidden i1 @_TZF12enum_derivedoi2eeFTOS_1ES0__Sb(i2, i2) 
// CHECK: %2 = icmp eq i2 %0, %1
// CHECK: ret i1 %2
