// RUN: %target-swift-ide-test -print-module -module-to-print=_SwiftConcurrencyShims -source-filename=x -enable-experimental-cxx-interop | %FileCheck %s
// RUN: %target-swift-ide-test -print-module -module-to-print=_SwiftConcurrencyShims -source-filename=x | %FileCheck %s

// REQUIRES: concurrency

// Ensure that _SwiftConcurrencyShims defines the same `exit` regardless of whether
// C++ interoperability is enabled.

// CHECK: func exit(_: Int32) -> Never
// CHECK: var EXIT_SUCCESS: Int32 { get }
