// RUN: %empty-directory(%t.mod)
// RUN: %target-swift-frontend -emit-module -o %t.mod/availability.swiftmodule %S/Inputs/availability_maccatalyst.swift -parse-as-library -emit-module-doc-path %t.mod/availability.swiftdoc
// RUN: %sourcekitd-test -req=doc-info -module availability -- -target %target-triple -I %t.mod | %FileCheck %s

// CHECK:      key.name: "isAlwaysDeprecated_catalyst()",
// CHECK:      key.attributes: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:     key.platform: source.availability.platform.maccatalyst,
// CHECK-NEXT:     key.is_deprecated: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ]

// CHECK:      key.name: "isAlwaysDeprecated_iOS()",
// CHECK:      key.attributes: [
// CHECK-NEXT:   {
// CHECK-NEXT:     key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:     key.platform: source.availability.platform.ios,
// CHECK-NEXT:     key.is_deprecated: 1
// CHECK-NEXT:   }
// CHECK-NEXT: ]
