// RUN: %target-swift-ide-test -print-module -module-to-print=TypeClassification -I %S/Inputs -source-filename=x -enable-cxx-interop | %FileCheck %s

// Make sure we don't import objects that we can't copy or destroy.
// CHECK-NOT: StructWithPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedCopyConstructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedCopyConstructor
// CHECK-NOT: StructNonCopyableTriviallyMovable
// CHECK-NOT: StructWithPrivateDefaultedDestructor
// CHECK-NOT: StructWithInheritedPrivateDefaultedDestructor
// CHECK-NOT: StructWithSubobjectPrivateDefaultedDestructor
// CHECK-NOT: StructWithDeletedDestructor
// CHECK-NOT: StructWithInheritedDeletedDestructor
// CHECK-NOT: StructWithSubobjectDeletedDestructor
