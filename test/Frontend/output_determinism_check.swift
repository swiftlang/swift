// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -module-name test -emit-module -o %t/test.swiftmodule -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=MODULE_OUTPUT
// RUN: %target-swift-frontend -module-name test -emit-sib -o %t/test.sib -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=SIB_OUTPUT

/// object files are "not" deterministic because the second run going to match the mod hash and skip code generation.
// RUN: not %target-swift-frontend -module-name test -c -o %t/test.o -primary-file %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=OBJECT_MISMATCH
/// object files should match when forcing object generation.
// RUN: %target-swift-frontend -module-name test -c -o %t/test.o -primary-file %s -enable-swift-deterministic-check -always-compile-output-files 2>&1 | %FileCheck %s --check-prefix=OBJECT_OUTPUT

/// Explicit module build. Check building swiftmodule from interface file.
// RUN: %target-swift-frontend -scan-dependencies -module-name test -o %t/test.json %s -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=DEPSCAN_OUTPUT
/// TODO: Implicit module build use a different compiler instance so it doesn't support checking yet.
// RUN: %target-swift-frontend -typecheck -emit-module-interface-path %t/test.swiftinterface %s -O -enable-swift-deterministic-check 2>&1 | %FileCheck %s --check-prefix=INTERFACE_OUTPUT
/// Hit cache and not emit the second time.
// RUN: rm %t/test.swiftmodule
// RUN: not %target-swift-frontend -compile-module-from-interface %t/test.swiftinterface -explicit-interface-module-build -o %t/test.swiftmodule -enable-swift-deterministic-check 2>&1 | %FileCheck --check-prefix=MODULE_MISMATCH %s
/// Force swiftmodule generation.
// RUN: %target-swift-frontend -compile-module-from-interface %t/test.swiftinterface -explicit-interface-module-build -o %t/test.swiftmodule -enable-swift-deterministic-check -always-compile-output-files 2>&1 | %FileCheck --check-prefix=MODULE_OUTPUT %s

// MODULE_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.swiftmodule'
// SIB_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.sib'
// OBJECT_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.o'
// OBJECT_MISMATCH: error: output file '{{.*}}{{/|\\}}test.o' is missing from second compilation for deterministic check
// DEPSCAN_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.json'
// INTERFACE_OUTPUT: remark: produced matching output file '{{.*}}{{/|\\}}test.swiftinterface'
// MODULE_MISMATCH: error: output file '{{.*}}{{/|\\}}test.swiftmodule' is missing from second compilation for deterministic check

public var x = 1
public func test() {}
