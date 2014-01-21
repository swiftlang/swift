// RUN: %swift-demangle `nm %libdir/swift/macosx/libswift_runtime.a | sed -n 's/^[0-9a-f]* T __TTbX/_TTbX/ p'` | FileCheck %s
// REQUIRES: X86

// We must be able to demangle every block shim in the runtime,
// and there must be no unmangled suffixes.
// CHECK-NOT: ---> _
// CHECK-NOT: unmangled suffix
