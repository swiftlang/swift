// RUN: %target-swift-emit-silgen -I %S/Inputs -enable-cxx-interop %s | %FileCheck %s

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

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithPrivateDefaultedCopyConstructor)
func pass(s: StructWithPrivateDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithPrivateDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedPrivateDefaultedCopyConstructor)
func pass(s: StructWithInheritedPrivateDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithInheritedPrivateDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectPrivateDefaultedCopyConstructor)
func pass(s: StructWithSubobjectPrivateDefaultedCopyConstructor) {
  // CHECK: bb0(%0 : $*StructWithSubobjectPrivateDefaultedCopyConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithMoveConstructor)
func pass(s: StructWithMoveConstructor) {
  // CHECK: bb0(%0 : $*StructWithMoveConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedMoveConstructor)
func pass(s: StructWithInheritedMoveConstructor) {
  // CHECK: bb0(%0 : $*StructWithInheritedMoveConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectMoveConstructor)
func pass(s: StructWithSubobjectMoveConstructor) {
  // CHECK: bb0(%0 : $*StructWithSubobjectMoveConstructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithCopyAssignment)
func pass(s: StructWithCopyAssignment) {
  // CHECK: bb0(%0 : $*StructWithCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedCopyAssignment)
func pass(s: StructWithInheritedCopyAssignment) {
  // CHECK: bb0(%0 : $*StructWithInheritedCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectCopyAssignment)
func pass(s: StructWithSubobjectCopyAssignment) {
  // CHECK: bb0(%0 : $*StructWithSubobjectCopyAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithMoveAssignment)
func pass(s: StructWithMoveAssignment) {
  // CHECK: bb0(%0 : $*StructWithMoveAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedMoveAssignment)
func pass(s: StructWithInheritedMoveAssignment) {
  // CHECK: bb0(%0 : $*StructWithInheritedMoveAssignment):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectMoveAssignment)
func pass(s: StructWithSubobjectMoveAssignment) {
  // CHECK: bb0(%0 : $*StructWithSubobjectMoveAssignment):
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

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithPrivateDefaultedDestructor)
func pass(s: StructWithPrivateDefaultedDestructor) {
  // CHECK: bb0(%0 : $*StructWithPrivateDefaultedDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithInheritedPrivateDefaultedDestructor)
func pass(s: StructWithInheritedPrivateDefaultedDestructor) {
  // CHECK: bb0(%0 : $*StructWithInheritedPrivateDefaultedDestructor):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructWithSubobjectPrivateDefaultedDestructor)
func pass(s: StructWithSubobjectPrivateDefaultedDestructor) {
  // CHECK: bb0(%0 : $*StructWithSubobjectPrivateDefaultedDestructor):
}

// Tests for common sets of special member functions.

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructTriviallyCopyableMovable)
func pass(s: StructTriviallyCopyableMovable) {
  // CHECK: bb0(%0 : $StructTriviallyCopyableMovable):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructNonCopyableTriviallyMovable)
func pass(s: StructNonCopyableTriviallyMovable) {
  // CHECK: bb0(%0 : $*StructNonCopyableTriviallyMovable):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructNonCopyableNonMovable)
func pass(s: StructNonCopyableNonMovable) {
  // CHECK: bb0(%0 : $*StructNonCopyableNonMovable):
}

// CHECK-LABEL: sil hidden [ossa] @$s4main4pass{{.*[ (]}}StructDeletedDestructor)
func pass(s: StructDeletedDestructor) {
  // CHECK: bb0(%0 : $*StructDeletedDestructor):
}
