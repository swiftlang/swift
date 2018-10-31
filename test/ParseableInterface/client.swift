// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
// RUN: %target-swift-frontend -enable-resilience -emit-parseable-module-interface-path %t/TestModule.swiftinterface -module-name TestModule %S/Inputs/other.swift -emit-module -o /dev/null
// RUN: test -f %t/TestModule.swiftinterface
// RUN: %target-swift-frontend -typecheck -enable-parseable-module-interface -module-cache-path %t/modulecache -I %t %s

// RUN: test -f %t/modulecache/TestModule-*.swiftmodule
// RUN: test ! %t/modulecache/TestModule-*.swiftmodule -ot %t/TestModule.swiftinterface
// RUN: llvm-bcanalyzer -dump %t/modulecache/TestModule-*.swiftmodule | %FileCheck %s -check-prefix=CHECK-SWIFTMODULE

// CHECK-SWIFTMODULE: {{MODULE_NAME.*blob data = 'TestModule'}}
// CHECK-SWIFTMODULE: {{FILE_DEPENDENCY.*Swift.swiftmodule'}}
// CHECK-SWIFTMODULE: FUNC_DECL
// CHECK-SWIFTMODULE: RESILIENCE_STRATEGY

import TestModule
func foo() {
    otherFileFunction()
}
