// RUN: %target-swift-ide-test -print-module -module-to-print=Inheritance -I %S/Inputs -source-filename=x -cxx-interoperability-mode=default  | %FileCheck %s

// CHECK-NOT: class ValueType {
// CHECK: struct ValueType {
// CHECK:   init()
// CHECK: }

// CHECK-NOT: func returnValueType() -> ValueType
// CHECK: func returnValueType() -> UnsafeMutablePointer<ValueType>

// CHECK-NOT: struct RefType {
// CHECK-NOT:   init()
// CHECK: class RefType {
// CHECK: }
// CHECK: func RCRetain(_ v: RefType)
// CHECK: func RCRelease(_ v: RefType)

// CHECK-NOT: func returnRefType() -> UnsafeMutablePointer<RefType>
// CHECK: func returnRefType() -> RefType

// CHECK-NOT: struct DerivedFromRefType {
// CHECK-NOT:   init()
// CHECK: class DerivedFromRefType {
// CHECK: }

// CHECK-NOT: func returnDerivedFromRefType() -> UnsafeMutablePointer<DerivedFromRefType>
// CHECK: func returnDerivedFromRefType() -> DerivedFromRefType
