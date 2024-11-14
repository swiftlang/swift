// RUN: %target-swift-ide-test -print-module -module-to-print=Constructors -I %S/Inputs -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s

// CHECK: struct TemplatedCopyConstructor
// CHECK: struct TemplatedCopyConstructorWithExtraArg

// Make sure we don't import non-copyable types because we will have no way to
// represent and copy/move these in swift with correct semantics.
// CHECK-NOT: DeletedCopyConstructor
