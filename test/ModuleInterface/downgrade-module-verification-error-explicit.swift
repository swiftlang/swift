// RUN: %empty-directory(%t)
// RUN: echo "// swift-interface-format-version: 1.0" > %t/Main.swiftinterface
// RUN: echo "// swift-module-flags: -module-name Foo" >> %t/Main.swiftinterface
// RUN: echo "malfunctioned" >> %t/Main.swiftinterface

// Verify with '-downgrade-typecheck-interface-error'
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Main.swiftinterface -module-name Foo -downgrade-typecheck-interface-error -explicit-interface-module-build 2>&1 | %FileCheck %s

// Verify with a blocklist
// RUN: echo "---" > %t/blocklist.yml
// RUN: echo "DowngradeInterfaceVerificationFailure:" >> %t/blocklist.yml
// RUN: echo "  ModuleName:" >> %t/blocklist.yml
// RUN: echo "    - Foo" >> %t/blocklist.yml
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Main.swiftinterface -module-name Foo -blocklist-file %t/blocklist.yml -explicit-interface-module-build 2>&1 | %FileCheck %s

// CHECK: warning:
// CHECK-NOT: error:
