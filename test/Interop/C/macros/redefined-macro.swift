// RUN: %target-swift-ide-test -print-module -module-to-print=RedefinedMacro -I %S/Inputs -source-filename=x | %FileCheck %s

// We cannot import redefined macros.
// CHECK-NOT: FOO
// CHECK-NOT: TMP_FOO
// CHECK-NOT: THREE
// CHECK-NOT: THE_MAX

// CHECK: var BAR
// CHECK: var BAZ
