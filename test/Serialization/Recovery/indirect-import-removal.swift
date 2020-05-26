// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module -o %t -module-name Lib -I %S/Inputs/custom-modules %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules | %FileCheck %s

// RUN: %target-swift-ide-test -source-filename=x -print-module -module-to-print Lib -I %t -I %S/Inputs/custom-modules -Xcc -DBAD | %FileCheck -check-prefix CHECK-RECOVERY %s

import IndirectImport

// CHECK: func baseline()
// CHECK-RECOVERY: func baseline()
public func baseline() {}

// CHECK: func test(_: IndirectlyImportedStruct)
// CHECK-RECOVERY-NOT: IndirectlyImportedStruct
public func test(_: IndirectlyImportedStruct) {}
