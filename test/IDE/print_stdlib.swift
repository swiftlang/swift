// Make sure we don't crash while printing the standard library.
//
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s > %t.txt
// RUN: FileCheck -input-file %t.txt %s
// RUN: %swift-ide-test -print-module -module-to-print=Swift -source-filename %s -module-print-submodules | FileCheck %s

// CHECK: var true: Bool { get }

// RUN: FileCheck -check-prefix=CHECK-IMPORT -input-file %t.txt %s
// CHECK-IMPORT-NOT: {{^}}import

// RUN: FileCheck -check-prefix=CHECK-TRANSPARENT -input-file %t.txt %s
// CHECK-TRANSPARENT-NOT: @transparent
