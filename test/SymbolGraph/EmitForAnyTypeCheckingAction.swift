// Symbol graphs are emitted by any action that typechecks the whole module.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/typecheck)
// RUN: %empty-directory(%t/silgen)
// RUN: %empty-directory(%t/sil)
// RUN: %empty-directory(%t/ir)

// Actions like -parse and -resolve-imports do not emit symbol graphs.

// RUN: %empty-directory(%t/parse)
// RUN: %empty-directory(%t/resolve)

// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t/typecheck %s -module-name M
// RUN: %target-swift-frontend -emit-silgen -emit-symbol-graph -emit-symbol-graph-dir %t/silgen %s -module-name M -o /dev/null
// RUN: %target-swift-frontend -emit-sil -emit-symbol-graph -emit-symbol-graph-dir %t/sil %s -module-name M -o /dev/null
// RUN: %target-swift-frontend -emit-ir -emit-symbol-graph -emit-symbol-graph-dir %t/ir %s -module-name M -o /dev/null

// Every type-checking action produces the same symbol graph.

// RUN: %FileCheck %s --input-file %t/typecheck/M.symbols.json
// RUN: diff %t/typecheck/M.symbols.json %t/silgen/M.symbols.json
// RUN: diff %t/typecheck/M.symbols.json %t/sil/M.symbols.json
// RUN: diff %t/typecheck/M.symbols.json %t/ir/M.symbols.json

// Actions that don't type-check the whole module do not emit a symbol graph.

// RUN: %target-swift-frontend -parse -emit-symbol-graph -emit-symbol-graph-dir %t/parse %s -module-name M
// RUN: %target-swift-frontend -resolve-imports -emit-symbol-graph -emit-symbol-graph-dir %t/resolve %s -module-name M
// RUN: not ls %t/parse/M.symbols.json
// RUN: not ls %t/resolve/M.symbols.json

public func foo() {}

// CHECK: "precise":"s:1M3fooyyF"
