// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -module-name CustomDomains -emit-module -emit-module-path %t/CustomDomains.swiftmodule \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain \
// RUN:   -emit-symbol-graph -emit-symbol-graph-dir %t/ -symbol-graph-pretty-print
// RUN: %FileCheck %s --input-file %t/CustomDomains.symbols.json

// REQUIRES: OS=macosx
// REQUIRES: swift_feature_CustomAvailability

// A custom domain used to land in the universal domain, so no availability
// entry may name it.

// CHECK-NOT: "domain": "*"

// A domain restriction carries no version and is neither a deprecation nor an
// unavailability, so there is nothing for the symbol graph format to represent.
// The symbol is still emitted, just without an availability mixin.

@available(EnabledDomain)
public struct AvailableInEnabledDomain {}

// CHECK:      "precise": "s:13CustomDomains24AvailableInEnabledDomainV"
// CHECK-NOT:  "availability"
// CHECK:      "location"

// A declaration that is unavailable in an enabled domain cannot be used in this
// compilation, but it is still documented with the domain it is unavailable in.

@available(EnabledDomain, unavailable)
public struct UnavailableInEnabledDomain {}

// CHECK:      "precise": "s:13CustomDomains26UnavailableInEnabledDomainV"
// CHECK:      "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "EnabledDomain",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]

@available(DynamicDomain, unavailable)
public struct UnavailableInDynamicDomain {}

// CHECK:      "precise": "s:13CustomDomains26UnavailableInDynamicDomainV"
// CHECK:      "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "DynamicDomain",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]

@available(DisabledDomain, unavailable)
public struct UnavailableInDisabledDomain {}

// CHECK:      "precise": "s:13CustomDomains27UnavailableInDisabledDomainV"
// CHECK:      "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "DisabledDomain",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]

@available(DynamicDomain, deprecated, message: "use something else")
public struct DeprecatedInDynamicDomain {}

// CHECK:      "precise": "s:13CustomDomains25DeprecatedInDynamicDomainV"
// CHECK:      "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "DynamicDomain",
// CHECK-NEXT:     "message": "use something else",
// CHECK-NEXT:     "isUnconditionallyDeprecated": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]

// A platform domain and a custom domain on the same declaration get separate
// entries.

@available(macOS 12.0, *)
@available(DynamicDomain, unavailable)
public struct UnavailableInDynamicDomainOnMacOS {}

// CHECK:      "precise": "s:13CustomDomains33UnavailableInDynamicDomainOnMacOSV"
// CHECK:      "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "DynamicDomain",
// CHECK-NEXT:     "isUnconditionallyUnavailable": true
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "introduced": {
// CHECK-NEXT:       "major": 12,
// CHECK-NEXT:       "minor": 0
// CHECK-NEXT:     }
// CHECK-NEXT:   }
// CHECK-NEXT: ]
