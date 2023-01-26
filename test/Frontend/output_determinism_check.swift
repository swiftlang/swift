// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name test -emit-module -o %t/test.swiftmodule -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=MODULE_OUTPUT
// RUN: %target-swift-frontend -module-name test -emit-sib -o %t/test.sib -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=SIB_OUTPUT

/// object files are "not" deterministic because the second run going to match the mod hash and skip code generation.
// RUN: not %target-swift-frontend -module-name test -c -o %t/test.o -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=OBJECT_MISMATCH
/// object files should match when forcing object generation.
// RUN: %target-swift-frontend -module-name test -c -o %t/test.o -primary-file %s -enable-swift-deterministic-check -always-compile-output-files 2>&1 | %FileCheck %s --check-prefix=OBJECT_OUTPUT

// MODULE_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.swiftmodule'
// SIB_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.sib'
// OBJECT_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.o'
// OBJECT_MISMATCH: error: output file '{{.*}}{{/|\\}}test.o' is missing from second compilation for deterministic check

public var x = 1
public func test() {}
