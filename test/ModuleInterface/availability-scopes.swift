// RUN: %empty-directory(%t)

// RUN: %target-swift-emit-module-interface(%t/Test.swiftinterface) %s -module-name Test
// RUN: %target-swift-typecheck-module-from-interface(%t/Test.swiftinterface) -module-name Test -dump-availability-scopes 2>&1 | %FileCheck --strict-whitespace %s
// RUN: %target-swift-frontend -compile-module-from-interface %t/Test.swiftinterface -o /dev/null -module-name Test -dump-availability-scopes 2>&1 | %FileCheck --strict-whitespace %s

// CHECK: {{^}}(root {{.*}} file={{.*}}{{/|\\}}availability-scopes.swift.tmp{{/|\\}}Test.swiftinterface
// CHECK: {{^}}  (decl {{.*}}unavailable=* decl=unavailable()
@available(*, unavailable)
public func unavailable() { }
