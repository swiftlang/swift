// Make sure we emit some kind of error message when we cannot find the module
// to print.

// RUN: not %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=NoSuchModule -source-filename=%s 2> %t.err
// RUN: %FileCheck %s -check-prefix=CHECK-MISSING < %t.err

// RUN: not %target-swift-ide-test -print-module -print-interface -no-empty-line-between-members -module-to-print=Swift.NoSuchSubModule -source-filename=%s 2> %t.suberr
// RUN: %FileCheck %s -check-prefix=CHECK-MISSING-SUBMODULE < %t.suberr

// CHECK-MISSING: 'NoSuchModule'
// CHECK-MISSING-SUBMODULE: 'Swift.NoSuchSubModule'
