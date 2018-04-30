// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -emit-module -module-name def_enum -o %t %S/Inputs/def_enum.swift
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -I %t -O -primary-file %s -emit-ir | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-NORMAL %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -I %t -O -primary-file %s -enable-testing -emit-ir | %FileCheck -check-prefix=CHECK -check-prefix=CHECK-TESTABLE %s

import def_enum

// Check if hashValue, hash(into:) and == for an enum (without payload) are
// generated and check that functions are compiled in an optimal way.

enum E {
  case E0
  case E1
  case E2
  case E3
}

// Check if the == comparison can be compiled to a simple icmp instruction.

// CHECK-NORMAL-LABEL:define hidden swiftcc i1 @"$S12enum_derived1EO02__b1_A7_equalsySbAC_ACtFZ"(i8, i8)
// CHECK-TESTABLE-LABEL:define{{( dllexport)?}}{{( protected)?}} swiftcc i1 @"$S12enum_derived1EO02__b1_A7_equalsySbAC_ACtFZ"(i8, i8)
// CHECK: %2 = icmp eq i8 %0, %1
// CHECK: ret i1 %2

// Check for the presence of the hashValue getter, calling Hasher.init() and
// Hasher.finalize().

// CHECK-NORMAL-LABEL:define hidden swiftcc i{{.*}} @"$S12enum_derived1EO9hashValueSivg"(i8)
// CHECK-TESTABLE-LABEL:define{{( dllexport)?}}{{( protected)?}} swiftcc i{{.*}} @"$S12enum_derived1EO9hashValueSivg"(i8)
// CHECK: call swiftcc void @"$Ss6HasherVABycfC"(%Ts6HasherV* {{.*}})
// CHECK: call swiftcc i{{[0-9]+}} @"$Ss6HasherV9_finalizeSiyF"(%Ts6HasherV* {{.*}})
// CHECK: ret i{{[0-9]+}} %{{[0-9]+}}

// Check if the hash(into:) method can be compiled to a simple zext instruction
// followed by a call to Hasher._combine(_:).

// CHECK-NORMAL-LABEL:define hidden swiftcc void @"$S12enum_derived1EO4hash4intoys6HasherVz_tF"
// CHECK-TESTABLE-LABEL:define{{( dllexport)?}}{{( protected)?}} swiftcc void @"$S12enum_derived1EO4hash4intoys6HasherVz_tF"
// CHECK: [[V:%.*]] = zext i8 %1 to i{{.*}}
// CHECK: tail call swiftcc void @"$Ss6HasherV8_combineyySuF"(i{{.*}} [[V]], %Ts6HasherV*
// CHECK: ret void

// Derived conformances from extensions
// The actual enums are in Inputs/def_enum.swift

extension def_enum.TrafficLight : Error {}

extension def_enum.Term : Error {}

// CHECK-NORMAL-LABEL: define hidden {{.*}}i64 @"$S12enum_derived7PhantomO8rawValues5Int64Vvg"(i8, %swift.type* nocapture readnone %T) local_unnamed_addr
// CHECK-TESTABLE-LABEL: define{{( dllexport)?}}{{( protected)?}} {{.*}}i64 @"$S12enum_derived7PhantomO8rawValues5Int64Vvg"(i8, %swift.type* nocapture readnone %T)

enum Phantom<T> : Int64 {
  case Up
  case Down
}
