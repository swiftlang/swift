// RUN: not %swift -parse %s -sdk "" 2>&1 | FileCheck %s

// CHECK: error: no such module 'NonExistent'
// CHECK: note: did you forget to set an SDK using -sdk or SDKROOT?
// CHECK-NEXT: use "-sdk $(xcrun --show-sdk-path --sdk macosx)" to select the default OS X SDK installed with Xcode
// CHECK-NOT: error
// CHECK-NOT: warning
// CHECK-NOT: note
import NonExistent

// No subsequent diagnostics after fatal errors.
var x: NonExistent.Something
