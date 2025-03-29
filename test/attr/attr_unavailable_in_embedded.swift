// RUN: %target-swift-frontend -typecheck %s -parse-stdlib -print-ast | %FileCheck %s --check-prefix=CHECK-NON-EMBEDDED
// RUN: %target-swift-frontend -typecheck %s -parse-stdlib -enable-experimental-feature Embedded -print-ast | %FileCheck %s --check-prefix=CHECK-EMBEDDED

// REQUIRES: swift_feature_Embedded

// CHECK-NON-EMBEDDED-NOT: @available
// CHECK-NON-EMBEDDED-NOT: @_unavailableInEmbedded
// CHECK-NON-EMBEDDED:     public func unavailable()

// CHECK-EMBEDDED-NOT:  @available
// CHECK-EMBEDDED:      @_unavailableInEmbedded
// CHECK-EMBEDDED-NEXT: public func unavailable()

@_unavailableInEmbedded
public func unavailable() {}
