// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name test -emit-module -o %t/test.swiftmodule -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=MODULE_OUTPUT
// RUN: %target-swift-frontend -module-name test -emit-sib -o %t/test.sib -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=SIB_OUTPUT

/// object files are "not" deterministic because the second run going to match the mod hash and skip code generation.
// RUN: not %target-swift-frontend -module-name test -c -o %t/test.o -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=OBJECT_OUTPUT

// MODULE_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.swiftmodule'
// SIB_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.sib'
// OBJECT_OUTPUT: error: output file '{{.*}}{{/|\\}}test.o' is missing from second run

public var x = 1
