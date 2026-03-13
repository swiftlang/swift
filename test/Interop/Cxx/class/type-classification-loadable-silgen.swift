// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-experimental-cxx-interop %s | %FileCheck %s

// This test checks that we classify C++ types as loadable and address-only
// correctly.

import TypeClassification

// Tests for individual special members

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}EmptyStruct)
func pass(s: EmptyStruct) {
  // CHECK: bb0(%0 : $EmptyStruct):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithDefaultConstructor)
func pass(s: StructWithDefaultConstructor) {
  // CHECK: bb0(%0 : $StructWithDefaultConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithAdditionalConstructor)
func pass(s: StructWithAdditionalConstructor) {
  // CHECK: bb0(%0 : $StructWithAdditionalConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithCopyConstructor)
func pass(s: StructWithCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedCopyConstructor)
func pass(s: StructWithInheritedCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithInheritedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectCopyConstructor)
func pass(s: StructWithSubobjectCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithSubobjectCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithDefaultedCopyConstructor)
func pass(s: StructWithDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $StructWithDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedDefaultedCopyConstructor)
func pass(s: StructWithInheritedDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $StructWithInheritedDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectDefaultedCopyConstructor)
func pass(s: StructWithSubobjectDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $StructWithSubobjectDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithCopyAssignment)
func pass(s: StructWithCopyAssignment) {
  // CHECK: bb0(%0 : $StructWithCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedCopyAssignment)
func pass(s: StructWithInheritedCopyAssignment) {
  // CHECK: bb0(%0 : $StructWithInheritedCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectCopyAssignment)
func pass(s: StructWithSubobjectCopyAssignment) {
  // CHECK: bb0(%0 : $StructWithSubobjectCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithDestructor)
func pass(s: StructWithDestructor) {
  // CHECK: bb0(%0 : $*StructWithDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedDestructor)
func pass(s: StructWithInheritedDestructor) {
  // CHECK: bb0(%0 : $*StructWithInheritedDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectDestructor)
func pass(s: StructWithSubobjectDestructor) {
  // CHECK: bb0(%0 : $*StructWithSubobjectDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithDefaultedDestructor)
func pass(s: StructWithDefaultedDestructor) {
  // CHECK: bb0(%0 : $StructWithDefaultedDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedDefaultedDestructor)
func pass(s: StructWithInheritedDefaultedDestructor) {
  // CHECK: bb0(%0 : $StructWithInheritedDefaultedDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectDefaultedDestructor)
func pass(s: StructWithSubobjectDefaultedDestructor) {
  // CHECK: bb0(%0 : $StructWithSubobjectDefaultedDestructor):
}

// Tests for common sets of special member functions.

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructTriviallyCopyableMovable)
func pass(s: StructTriviallyCopyableMovable) {
  // CHECK: bb0(%0 : $StructTriviallyCopyableMovable):
}
