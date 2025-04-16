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

// DEFAULT-DAG: macOS
// DEFAULT-DAG: iOS
// DEFAULT-DAG: watchOS
// DEFAULT-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// ALLOWLIST-NOT: watchOS
// ALLOWLIST-DAG: macOS
// ALLOWLIST-DAG: iOS
// ALLOWLIST-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// BLOCKLIST-NOT: macOS
// BLOCKLIST-NOT: iOS
// BLOCKLIST-DAG: watchOS
// BLOCKLIST-DAG: "isUnconditionallyDeprecated":{{ ?}}true

// EMPTY-NOT: macOS
// EMPTY-NOT: iOS
// EMPTY-NOT: watchOS
// EMPTY-DAG: "isUnconditionallyDeprecated":{{ ?}}true
