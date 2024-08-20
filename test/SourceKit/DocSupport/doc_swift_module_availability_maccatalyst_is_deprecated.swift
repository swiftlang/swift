// REQUIRES: maccatalyst_support

// RUN: %empty-directory(%t.mod)
// RUN: %swift -emit-module -target %target-cpu-apple-ios13.1-macabi -o %t.mod/availability.swiftmodule %s -parse-as-library -emit-module-doc-path %t.mod/availability.swiftdoc
// RUN: %sourcekitd-test -req=doc-info -module availability -- -target %target-cpu-apple-ios13.1-macabi -I %t.mod | %FileCheck %s

@available(macCatalyst, deprecated: 20.0)
public func deprecatedInFutureVersion_catalyst() {}
// CHECK-LABEL: key.name: "deprecatedInFutureVersion_catalyst()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.maccatalyst,
// CHECK-NEXT:       key.deprecated: "20.0"
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }

@available(iOS, deprecated: 20.0)
public func deprecatedInFutureVersion_iOS() {}
// CHECK-LABEL: key.name: "deprecatedInFutureVersion_iOS()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.ios,
// CHECK-NEXT:       key.deprecated: "20.0"
// CHECK-NEXT:     }
// CHECK-NEXT:   ]
// CHECK-NEXT: }

@available(macCatalyst, deprecated: 1.0)
public func deprecatedInPastVersion_catalyst() {}
// CHECK-LABEL: key.name: "deprecatedInPastVersion_catalyst()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.maccatalyst,
// CHECK-NEXT:       key.deprecated: "1.0"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.is_deprecated: 1
// CHECK-NEXT: }

@available(iOS, deprecated: 1.0)
public func deprecatedInPastVersion_iOS() {}
// CHECK-LABEL: key.name: "deprecatedInPastVersion_iOS()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.ios,
// CHECK-NEXT:       key.deprecated: "1.0"
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.is_deprecated: 1
// CHECK-NEXT: }

@available(macCatalyst, deprecated)
public func isAlwaysDeprecated_catalyst() {}
// CHECK-LABEL: key.name: "isAlwaysDeprecated_catalyst()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.maccatalyst,
// CHECK-NEXT:       key.is_deprecated: 1
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.is_deprecated: 1
// CHECK-NEXT: }

@available(iOS, deprecated)
public func isAlwaysDeprecated_iOS() {}
// CHECK-LABEL: key.name: "isAlwaysDeprecated_iOS()"
// CHECK-NOT: {
// CHECK:        key.attributes: [
// CHECK-NEXT:     {
// CHECK-NEXT:       key.kind: source.lang.swift.attribute.availability,
// CHECK-NEXT:       key.platform: source.availability.platform.ios,
// CHECK-NEXT:       key.is_deprecated: 1
// CHECK-NEXT:     }
// CHECK-NEXT:   ],
// CHECK-NEXT:   key.is_deprecated: 1
// CHECK-NEXT: }
