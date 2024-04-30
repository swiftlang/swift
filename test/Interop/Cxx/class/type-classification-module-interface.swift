// RUN: %target-swift-ide-test -print-module -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-5.9 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -skip-unsafe-cxx-methods -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -cxx-interoperability-mode=swift-6 | %FileCheck %s -check-prefix=CHECK-SKIP-UNSAFE
// RUN: %target-swift-ide-test -print-module -skip-unsafe-cxx-methods -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -cxx-interoperability-mode=upcoming-swift | %FileCheck %s -check-prefix=CHECK-SKIP-UNSAFE

// Make sure we don't import objects that we can't copy or destroy.
// CHECK-NOT: StructWithPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedCopyConstructor
// CHECK-NOT: StructNonCopyableNonMovable
// CHECK-NOT: StructWithMoveAssignment
// CHECK-NOT: StructWithInheritedMoveAssignment
// CHECK-NOT: StructWithSubobjectMoveAssignment
// CHECK-NOT: StructWithPrivateDefaultedDestructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedDestructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedDestructor
// CHECK-NOT: StructWithDeletedCopyConstructor
// CHECK-NOT: StructWithMoveConstructorAndDeletedCopyConstructor
// CHECK-NOT: StructWithDeletedDestructor
// CHECK-NOT: StructWithInheritedDeletedDestructor
// CHECK-NOT: StructWithSubobjectDeletedDestructor

// CHECK: struct StructWithMoveConstructor

// CHECK: struct StructWithInheritedMoveConstructor

// CHECK: struct StructWithSubobjectMoveConstructor

// CHECK: struct StructNonCopyableTriviallyMovable

// CHECK: struct StructWithPointerNonCopyableTriviallyMovable
// CHECK: struct StructWithPointerNonCopyableTriviallyMovableField

// CHECK: struct Iterator {
// CHECK: }

// CHECK: struct HasMethodThatReturnsIterator {
// CHECK:   func __getIteratorUnsafe() -> Iterator
// CHECK-SKIP-UNSAFE-NOT: func __getIteratorUnsafe() -> Iterator
// CHECK: }

// CHECK: struct IteratorBox {
// CHECK: }

// CHECK: struct HasMethodThatReturnsIteratorBox {
// CHECK:   func __getIteratorBoxUnsafe() -> IteratorBox
// CHECK-SKIP-UNSAFE-NOT: func __getIteratorBoxUnsafe() -> IteratorBox
// CHECK: }

// CHECK: struct StructCopyableMovableAnnotatedNonCopyable
