// RUN: %target-swift-ide-test -print-module -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// Make sure we don't import objects that we can't copy or destroy.
// CHECK-NOT: StructWithPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedCopyConstructor
// CHECK-NOT: StructNonCopyableTriviallyMovable
// CHECK-NOT: StructNonCopyableNonMovable
// CHECK-NOT: StructWithMoveConstructor
// CHECK-NOT: StructWithInheritedMoveConstructor
// CHECK-NOT: StructWithSubobjectMoveConstructor
// CHECK-NOT: StructWithMoveAssignment
// CHECK-NOT: StructWithInheritedMoveAssignment
// CHECK-NOT: StructWithSubobjectMoveAssignment
// CHECK-NOT: StructWithPrivateDefaultedDestructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedDestructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedDestructor
// CHECK-NOT: StructWithDeletedDestructor
// CHECK-NOT: StructWithInheritedDeletedDestructor
// CHECK-NOT: StructWithSubobjectDeletedDestructor

// CHECK: struct Iterator {
// CHECK: }

// CHECK: struct HasMethodThatReturnsIterator {
// CHECK:   func __getIteratorUnsafe() -> Iterator
// CHECK: }

// CHECK: struct IteratorBox {
// CHECK: }

// CHECK: struct HasMethodThatReturnsIteratorBox {
// CHECK:   func __getIteratorBoxUnsafe() -> IteratorBox
// CHECK: }
