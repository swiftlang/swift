// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t %s -module-name EmitWhileTypeChecking
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json --check-prefix PUB

// also try without the trailing slash on `-emit-symbol-graph-dir` and make sure it works

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t/ %s -module-name EmitWhileTypeChecking
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json --check-prefix PUB

// now run with -symbol-graph-minimum-access-level to change the available symbols

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t %s -module-name EmitWhileTypeChecking -symbol-graph-minimum-access-level private
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json
// RUN: %FileCheck %s --input-file %t/EmitWhileTypeChecking.symbols.json --check-prefix PRIV

/// Does a foo.
public func foo() {}

/// Does a bar.
func bar() {}

// CHECK: "precise":"s:21EmitWhileTypeChecking3fooyyF"
// PUB-NOT: "precise":"s:21EmitWhileTypeChecking3baryyF"
// PRIV: "precise":"s:21EmitWhileTypeChecking3baryyF"
