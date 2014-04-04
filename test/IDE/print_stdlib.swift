// Make sure we don't crash while printing the standard library.
//
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s | FileCheck %s
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s -module-print-submodules | FileCheck %s

// CHECK: var true: Bool { get }
