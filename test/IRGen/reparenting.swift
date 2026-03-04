// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -enable-library-evolution -module-name A -primary-file %s -emit-ir -enable-experimental-feature Reparenting -o %t/out.ll
// RUN: %FileCheck %s --check-prefix=CHECK-%target-cpu --check-prefix=CHECK < %t/out.ll

// RUN: %target-swift-frontend -enable-relative-protocol-witness-tables -enable-library-evolution -module-name A -primary-file %s -emit-ir -enable-experimental-feature Reparenting -o %t/relative.ll
// RUN: %FileCheck %s --check-prefix=RELATIVE --check-prefix=CHECK < %t/relative.ll

// REQUIRES: swift_feature_Reparenting
// REQUIRES: CPU=x86_64 || CPU=arm64 || CPU=arm64e

@reparentable public protocol NewProto {
  associatedtype Thing: Equatable
  func new() -> Thing
}

extension Existing: @reparented NewProto where Thing == String {
  public func new() -> Thing { return "new" }
}

public protocol Existing: NewProto {
  associatedtype Thing: Equatable = String
  func existing() -> Thing
}

// CHECK: @"default associated conformance1A8NewProto" = linkonce_odr hidden constant
// CHECK-SAME:  ptr @"$s1A8ExistingPxAA8NewProtoTN"

// CHECK: @"$s1A8ExistingMp" =
// CHECK-SAME: @"default associated conformance1A8NewProto"

// CHECK-LABEL: define internal swiftcc ptr @"$s1A8ExistingPxAA8NewProtoTN"(ptr %0, ptr %1, ptr %2){{.*}} {
// CHECK:  %instantiationArgs = alloca ptr, align 8
// CHECK:  store ptr %2, ptr %instantiationArgs, align 8

// CHECK-x86_64:  %3 = call ptr @swift_getWitnessTable(ptr @"$s1A8Existing_pAA8NewProtoAASS5ThingRtzrlMc", ptr %0, ptr %instantiationArgs)
// CHECK-arm64:   %3 = call ptr @swift_getWitnessTable(ptr @"$s1A8Existing_pAA8NewProtoAASS5ThingRtzrlMc", ptr %0, ptr %instantiationArgs)
// CHECK-arm64e:  %3 = call ptr @swift_getWitnessTable(ptr @"$s1A8Existing_pAA8NewProtoAASS5ThingRtzrlMc.ptrauth", ptr %0, ptr %instantiationArgs)
// RELATIVE:      %3 = call ptr @swift_getWitnessTableRelative(ptr @"$s1A8Existing_pAA8NewProtoAASS5ThingRtzrlMc

// CHECK:  ret ptr %3
// CHECK:  }
