// RUN: env %sourcekitd-test -req=diags %s -- %s -Xfrontend -Rmodule-api-import | %FileCheck %s
public typealias Foo = String

// CHECK:      {
// CHECK-NEXT:   key.diagnostics: [
// CHECK-NEXT:   {
// CHECK-NEXT:       key.line: 2,
// CHECK-NEXT:       key.column: 24,
// CHECK-NEXT:       key.filepath: "{{.*}}",
// CHECK-NEXT:       key.severity: source.diagnostic.severity.remark,
// CHECK-NEXT:       key.id: "module_api_import",
// CHECK-NEXT:       key.description: "struct 'String' is imported via 'Swift'"
// CHECK-NEXT:   }
// CHECK-NEXT:   ]
// CHECK-NEXT: }
