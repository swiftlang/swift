// RUN: %sourcekitd-test -req=sema %s -- %s | %FileCheck %s
Swift.String
Swift
// CHECK: key.kind: source.lang.swift.ref.struct,
// CHECK: key.severity: source.diagnostic.severity.error,
// CHECK-NEXT: key.description: "expected member name or constructor call after type name",
