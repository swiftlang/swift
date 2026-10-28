// Symbol graphs are emitted by any action that typechecks the whole module.

// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/typecheck)
// RUN: %empty-directory(%t/silgen)
// RUN: %empty-directory(%t/sil)
// RUN: %empty-directory(%t/ir)
// RUN: %empty-directory(%t/parse)
// RUN: %empty-directory(%t/resolve)

// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t/typecheck %s -module-name M
// RUN: %target-swift-frontend -emit-silgen -emit-symbol-graph -emit-symbol-graph-dir %t/silgen %s -module-name M -o /dev/null
// RUN: %target-swift-frontend -emit-sil -emit-symbol-graph -emit-symbol-graph-dir %t/sil %s -module-name M -o /dev/null
// RUN: %target-swift-frontend -emit-ir -emit-symbol-graph -emit-symbol-graph-dir %t/ir %s -module-name M -o /dev/null

// Every typechecking action produces the same symbol graph.

// RUN: %FileCheck %s --input-file %t/typecheck/M.symbols.json
// RUN: %FileCheck %s --input-file %t/typecheck/M.symbols.json --check-prefix PUB
// RUN: diff %t/typecheck/M.symbols.json %t/silgen/M.symbols.json
// RUN: diff %t/typecheck/M.symbols.json %t/sil/M.symbols.json
// RUN: diff %t/typecheck/M.symbols.json %t/ir/M.symbols.json

// A trailing slash on -emit-symbol-graph-dir also works.

// RUN: %empty-directory(%t/slash)
// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t/slash/ %s -module-name M
// RUN: %FileCheck %s --input-file %t/slash/M.symbols.json

// -symbol-graph-minimum-access-level changes the set of emitted symbols.

// RUN: %empty-directory(%t/private)
// RUN: %target-swift-frontend -typecheck -emit-symbol-graph -emit-symbol-graph-dir %t/private %s -module-name M -symbol-graph-minimum-access-level private
// RUN: %FileCheck %s --input-file %t/private/M.symbols.json --check-prefix PRIV

// Dependency scanning forwards -emit-symbol-graph in the module build commands it produces,
// so it must be allowed to carry the flag even though it does not typecheck the whole module.

// RUN: %target-swift-frontend -scan-dependencies -emit-symbol-graph -emit-symbol-graph-dir %t/scan %s -module-name M -o %t/deps.json -module-cache-path %t/mcp
// RUN: not ls %t/scan/M.symbols.json

// Actions that don't typecheck the whole module cannot emit a symbol graph.

// RUN: not %target-swift-frontend -parse -emit-symbol-graph -emit-symbol-graph-dir %t/parse %s -module-name M 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not %target-swift-frontend -resolve-imports -emit-symbol-graph -emit-symbol-graph-dir %t/resolve %s -module-name M 2>&1 | %FileCheck %s --check-prefix ERROR
// RUN: not ls %t/parse/M.symbols.json
// RUN: not ls %t/resolve/M.symbols.json

// ERROR: this mode does not support emitting symbol graph files

/// Does a foo.
public func foo() {}

/// Does a bar.
func bar() {}

// CHECK: "precise":"s:1M3fooyyF"
// PUB-NOT: "precise":"s:1M3baryyF"
// PRIV: "precise":"s:1M3baryyF"
