// RUN: rm -rf %t
// RUN: mkdir %t
// RUN: %target-swift-frontend -emit-module -module-name def_enum -o %t %S/Inputs/def_enum.swift
// RUN: %target-swift-frontend -I %t -O -primary-file %s -emit-ir | FileCheck -check-prefix=CHECK -check-prefix=CHECK-NORMAL %s
// RUN: %target-swift-frontend -I %t -O -primary-file %s -enable-testing -emit-ir | FileCheck -check-prefix=CHECK -check-prefix=CHECK-TESTABLE %s

import def_enum

// Check if the hashValue and == for an enum (without payload) are generated and
// check if that functions are compiled in an optimal way.

enum E {
  case E0
  case E1
  case E2
  case E3
}

// Check if the hashValue getter can be compiled to a simple zext instruction.

// CHECK-NORMAL-LABEL:define hidden i{{.*}} @_TFO12enum_derived1Eg9hashValueSi(i2)
// CHECK-TESTABLE-LABEL:define i{{.*}} @_TFO12enum_derived1Eg9hashValueSi(i2)
// CHECK: %1 = zext i2 %0 to i{{.*}}
// CHECK: ret i{{.*}} %1

// Check if the == comparison can be compiled to a simple icmp instruction.

// CHECK-NORMAL-LABEL:define hidden i1 @_TZF12enum_derivedoi2eeFTOS_1ES0__Sb(i2, i2)
// CHECK-TESTABLE-LABEL:define i1 @_TZF12enum_derivedoi2eeFTOS_1ES0__Sb(i2, i2)
// CHECK: %2 = icmp eq i2 %0, %1
// CHECK: ret i1 %2

// Derived conformances from extensions
// The actual enums are in Inputs/def_enum.swift

extension def_enum.TrafficLight : ErrorType {}

// CHECK-LABEL: define { i8*, i64, i64 } @_TFE12enum_derivedO8def_enum12TrafficLightg7_domainSS(i2) #0
// CHECK-LABEL: define i64 @_TFE12enum_derivedO8def_enum12TrafficLightg5_codeSi(i2)

extension def_enum.Term : ErrorType {}

// CHECK-NORMAL-LABEL: define hidden i64 @_TFO12enum_derived7Phantomg8rawValueSi(i1, %swift.type* nocapture readnone %T) #1
// CHECK-TESTABLE-LABEL: define i64 @_TFO12enum_derived7Phantomg8rawValueSi(i1, %swift.type* nocapture readnone %T) #1

enum Phantom<T> : Int {
  case Up
  case Down
}

extension Phantom : RawRepresentable {}
