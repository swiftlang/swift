// RUN: %empty-directory(%t)
// RUN: echo "// swift-interface-format-version: 1.0" > %t/Main.swiftinterface
// RUN: echo "// swift-module-flags: -module-name Foo" >> %t/Main.swiftinterface
// RUN: echo "malfunctioned" >> %t/Main.swiftinterface
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/Main.swiftinterface -module-name Foo -downgrade-typecheck-interface-error &> %t/results.txt
// RUN: %FileCheck --input-file %t/results.txt %s

// CHECK: warning:
// CHECK-NOT: error:
