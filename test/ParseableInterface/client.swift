// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/modulecache)
// RUN: %target-swift-frontend -enable-resilience -emit-parseable-module-interface-path %t/TestModule.swiftinterface -module-name TestModule %S/Inputs/other.swift -emit-module -o /dev/null
// RUN: test -f %t/TestModule.swiftinterface
// RUN: %target-swift-frontend -typecheck -enable-parseable-module-interface -module-cache-path %t/modulecache -I %t %s

// RUN: test ! %t/modulecache/TestModule-*.swiftmodule -ot %t/TestModule.swiftinterface
// RUN: llvm-bcanalyzer -dump %t/modulecache/TestModule-*.swiftmodule | %FileCheck %s -check-prefix=CHECK-SWIFTMODULE
// RUN: %FileCheck %s -check-prefix=CHECK-SID <%t/modulecache/TestModule-*.sid

// Now add a "dependency" to the .sid file but set it to be quite old
// RUN: echo %t/fake-dep >>%t/modulecache/TestModule-*.sid
// RUN: touch -t 201401240005 %t/fake-dep
// RUN: %FileCheck %s -check-prefix=CHECK-SID2 <%t/modulecache/TestModule-*.sid

// Check that the cache does not rebuild
// RUN: %target-swift-frontend -typecheck -enable-parseable-module-interface -module-cache-path %t/modulecache -I %t %s
// RUN: %FileCheck %s -check-prefix=CHECK-SID2 <%t/modulecache/TestModule-*.sid

// Now touch the dependency a ways into the future, and check that the cache _does_ rebuild
// (Sorry, this test will need refreshing in the year 2035)
// RUN: touch -t 203501240005 %t/fake-dep
// RUN: %target-swift-frontend -typecheck -enable-parseable-module-interface -module-cache-path %t/modulecache -I %t %s
// RUN: %FileCheck %s -check-prefix=CHECK-SID3 <%t/modulecache/TestModule-*.sid

// CHECK-SWIFTMODULE: {{MODULE_NAME.*blob data = 'TestModule'}}
// CHECK-SWIFTMODULE: FUNC_DECL
// CHECK-SWIFTMODULE: RESILIENCE_STRATEGY
// CHECK-SID: Swift.swiftmodule
// CHECK-SID2: fake-dep
// CHECK-SID3-NOT: fake-dep

import TestModule
func foo() {
    otherFileFunction()
}
