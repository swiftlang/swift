// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name AvailabilityFilter -emit-module -emit-module-path %t/AvailabilityFilter.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix DEFAULT

// RUN: %target-swift-symbolgraph-extract -module-name AvailabilityFilter -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix DEFAULT

// Now checking the allowlist behavior...

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name AvailabilityFilter -emit-module -emit-module-path %t/AvailabilityFilter.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-allow-availability-platforms macOS,iOS
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix ALLOWLIST

// RUN: %target-swift-symbolgraph-extract -module-name AvailabilityFilter -I %t -pretty-print -output-dir %t -allow-availability-platforms macOS,iOS
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix ALLOWLIST

// Now checking the blocklist behavior...

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name AvailabilityFilter -emit-module -emit-module-path %t/AvailabilityFilter.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-block-availability-platforms macOS,iOS
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix BLOCKLIST

// RUN: %target-swift-symbolgraph-extract -module-name AvailabilityFilter -I %t -pretty-print -output-dir %t -block-availability-platforms macOS,iOS
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix BLOCKLIST

// Now test to ensure an empty allow list filters out all availability...

// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name AvailabilityFilter -emit-module -emit-module-path %t/AvailabilityFilter.swiftmodule -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-allow-availability-platforms ""
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix EMPTY

// RUN: %target-swift-symbolgraph-extract -module-name AvailabilityFilter -I %t -pretty-print -output-dir %t -allow-availability-platforms ""
// RUN: %FileCheck %s --input-file %t/AvailabilityFilter.symbols.json --check-prefix EMPTY

// REQUIRES: OS=macosx

@available(macOS 11.0, iOS 15.0, watchOS 15.0, *)
public struct S {}

/// Ensure that regardless of platforms being removed, that universal availability info,
/// like unconditional deprecation, still lands in the symbol graph.
@available(*, deprecated)
public class C {}

// DEFAULT-DAG: "domain":{{ ?}}"macOS"
// DEFAULT-DAG: "domain":{{ ?}}"iOS"
// DEFAULT-DAG: "domain":{{ ?}}"watchOS"
// DEFAULT-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// ALLOWLIST-NOT: "domain":{{ ?}}"watchOS"
// ALLOWLIST-DAG: "domain":{{ ?}}"macOS"
// ALLOWLIST-DAG: "domain":{{ ?}}"iOS"
// ALLOWLIST-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// BLOCKLIST-NOT: "domain":{{ ?}}"macOS"
// BLOCKLIST-NOT: "domain":{{ ?}}"iOS"
// BLOCKLIST-DAG: "domain":{{ ?}}"watchOS"
// BLOCKLIST-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// EMPTY-NOT: "domain":{{ ?}}"macOS"
// EMPTY-NOT: "domain":{{ ?}}"iOS"
// EMPTY-NOT: "domain":{{ ?}}"watchOS"
// EMPTY-DAG: "isUnconditionallyDeprecated":{{ ?}}true
