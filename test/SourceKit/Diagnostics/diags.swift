// Use -print-raw-response on the first request so we don't wait for semantic info which will never come.
// RUN: %sourcekitd-test -req=open %s -req-opts=syntactic_only=1 -print-raw-response -- %s == \
// RUN: -req=stats == \
// RUN: -req=diags %s -print-raw-response -- %s == \
// RUN: -req=stats == \
// RUN: -req=diags %s -print-raw-response -- %s == \
// RUN: -req=stats | %FileCheck %s

func foo(y: String) {
  foo(y: 1)
}

// We shouldn't build an AST for the syntactic open
// CHECK: 0 {{.*}} source.statistic.num-ast-builds

// Retrieving diagnostics should workd
// CHECK: {
// CHECK:   key.diagnostics: [
// CHECK:     {
// CHECK:       key.line: 10,
// CHECK:       key.column: 10,
// CHECK:       key.filepath: "{{.*}}",
// CHECK:       key.severity: source.diagnostic.severity.error,
// CHECK:       key.id: "cannot_convert_argument_value",
// CHECK:       key.description: "cannot convert value of type 'Int' to expected argument type 'String'"
// CHECK:     }
// CHECK:   ]
// CHECK: }

// ... and we should have built an AST for it
// CHECK: 1 {{.*}} source.statistic.num-ast-builds

// Retrieving diagnostics again should workd
// CHECK: {
// CHECK:   key.diagnostics: [
// CHECK:     {
// CHECK:       key.line: 10,
// CHECK:       key.column: 10,
// CHECK:       key.filepath: "{{.*}}",
// CHECK:       key.severity: source.diagnostic.severity.error,
// CHECK:       key.id: "cannot_convert_argument_value",
// CHECK:       key.description: "cannot convert value of type 'Int' to expected argument type 'String'"
// CHECK:     }
// CHECK:   ]
// CHECK: }

// ... but we shouldn't rebuild an AST
// CHECK: 1 {{.*}} source.statistic.num-ast-builds
// CHECK: 1 {{.*}} source.statistic.num-ast-cache-hits
