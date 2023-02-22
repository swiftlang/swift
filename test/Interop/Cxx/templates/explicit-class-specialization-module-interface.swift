// RUN: %target-swift-ide-test -print-module -module-to-print=ExplicitClassSpecialization -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct HasEmptySpecializationAndStaticDateMember<Int32> {
// CHECK:   static let value: Bool
// CHECK: }

// CHECK: struct HasEmptySpecializationAndStaticDateMember<Int8> {
// CHECK:   static let value: Bool
// CHECK: }
