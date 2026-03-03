
// RUN: not %target-swift-frontend -typecheck -debug-cycles %s 2>&1 | %FileCheck %s --match-full-lines --strict-whitespace --color=false

class Outer2: Outer2.Inner {
  class Inner {}
}
// CHECK:===CYCLE DETECTED===
// CHECK-NEXT: `--TypeCheckPrimaryFileRequest({{.*}})
// CHECK-NEXT:     `--[0;32mSuperclassDeclRequest({{.*}})[0m
// CHECK-NEXT:         `--InheritedDeclsReferencedRequest({{.*}})
// CHECK-NEXT:             `--QualifiedLookupRequest({{.*}})
// CHECK-NEXT:                 `--[0;32mSuperclassDeclRequest({{.*}})[0;31m (cyclic dependency)[0m
