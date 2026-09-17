// REQUIRES: objc_interop, OS=macosx
// REQUIRES: swift_feature_CustomAvailability
// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend %s -typecheck -parse-as-library -module-name MyModule -swift-version 5 \
// RUN:   -enable-library-evolution -library-level api -emit-api-descriptor-path %t/api.json \
// RUN:   -enable-experimental-feature CustomAvailability \
// RUN:   -define-enabled-availability-domain EnabledDomain \
// RUN:   -define-disabled-availability-domain DisabledDomain \
// RUN:   -define-dynamic-availability-domain DynamicDomain
// RUN: %validate-json %t/api.json | %FileCheck %s

@available(DisabledDomain, unavailable)
public func unavailableInDisabledDomain() {}

@available(DynamicDomain, unavailable)
public func unavailableInDynamicDomain() {}

@available(EnabledDomain)
@available(*, unavailable)
public func domainThenUniversallyUnavailable() {}

@available(*, unavailable)
@available(EnabledDomain)
public func universallyUnavailableThenDomain() {}

// CHECK:       "globals": [
// CHECK-NEXT:      {
// CHECK-NEXT:          "name": "_$s8MyModule26unavailableInDynamicDomainyyF",
// CHECK-NEXT:          "access": "public",
// CHECK-NEXT:          "file": "SOURCE_DIR/test/APIJSON/availability_custom_domains.swift",
// CHECK-NEXT:          "linkage": "exported"
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:          "name": "_$s8MyModule27unavailableInDisabledDomainyyF",
// CHECK-NEXT:          "access": "public",
// CHECK-NEXT:          "file": "SOURCE_DIR/test/APIJSON/availability_custom_domains.swift",
// CHECK-NEXT:          "linkage": "exported"
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:          "name": "_$s8MyModule32domainThenUniversallyUnavailableyyF",
// CHECK-NEXT:          "access": "public",
// CHECK-NEXT:          "file": "SOURCE_DIR/test/APIJSON/availability_custom_domains.swift",
// CHECK-NEXT:          "linkage": "exported",
// CHECK-NEXT:          "unavailable": true
// CHECK-NEXT:      },
// CHECK-NEXT:      {
// CHECK-NEXT:          "name": "_$s8MyModule32universallyUnavailableThenDomainyyF",
// CHECK-NEXT:          "access": "public",
// CHECK-NEXT:          "file": "SOURCE_DIR/test/APIJSON/availability_custom_domains.swift",
// CHECK-NEXT:          "linkage": "exported",
// CHECK-NEXT:          "unavailable": true
// CHECK-NEXT:      }
// CHECK-NEXT:  ],
