// RUN: %target-swift-ide-test -print-module -module-to-print=Inheritance -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default  | %FileCheck %s

// CHECK-NOT: class ValueType {
// CHECK: struct ValueType {
// CHECK:   init()
// CHECK: }

// CHECK-NOT: func returnValueType() -> BasicInheritanceExample.ValueType
// CHECK: func returnValueType() -> UnsafeMutablePointer<BasicInheritanceExample.ValueType>

// CHECK-NOT: struct RefType {
// CHECK-NOT:   init()
// CHECK: class RefType {
// CHECK: }

// CHECK-NOT: func returnRefType() -> UnsafeMutablePointer<BasicInheritanceExample.RefType>
// CHECK: func returnRefType() -> BasicInheritanceExample.RefType

// CHECK-NOT: struct DerivedFromRefType {
// CHECK-NOT:   init()
// CHECK: class DerivedFromRefType {
// CHECK: }

// CHECK-NOT: func returnDerivedFromRefType() -> UnsafeMutablePointer<BasicInheritanceExample.DerivedFromRefType>
// CHECK: func returnDerivedFromRefType() -> BasicInheritanceExample.DerivedFromRefType
