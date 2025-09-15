// RUN: %empty-directory(%t)
// RUN: %target-swift-emit-module-interface(%t/foo.swiftinterface) %s -module-name foo
// RUN: %target-swift-frontend -typecheck-module-from-interface %t/foo.swiftinterface -stats-output-dir %t
