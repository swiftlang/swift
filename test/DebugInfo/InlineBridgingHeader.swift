// RUN: %target-swift-frontend \
// RUN:   -import-objc-header %S/Inputs/InlineBridgingHeader.h \
// RUN:   -emit-ir -g %s -o - | FileCheck %s
// REQUIRES: objc_interop
// CHECK: DISubprogram(
// CHECK-SAME: "Foo"

Foo()
