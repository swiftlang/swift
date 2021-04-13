// RUN: %target-swift-ide-test -print-module -module-to-print=ExplicitClassSpecialization -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// CHECK: struct __CxxTemplateInst41HasEmptySpecializationAndStaticDateMemberIiE {
// CHECK:   static let value: Bool
// CHECK: }

// CHECK: struct __CxxTemplateInst41HasEmptySpecializationAndStaticDateMemberIcE {
// CHECK:   static let value: Bool
// CHECK: }
