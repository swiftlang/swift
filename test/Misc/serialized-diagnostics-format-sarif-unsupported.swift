// '=sarif' is rejected by a compiler that cannot serialize SARIF, rather than
// being accepted and silently producing no log.

// RUN: not %target-swift-frontend -typecheck -serialize-diagnostics=sarif %s 2>&1 | %FileCheck %s
// CHECK: error: serializing diagnostics to SARIF is not supported by this compiler build

// UNSUPPORTED: swift_sarif

func f() { let unused = 1 }
