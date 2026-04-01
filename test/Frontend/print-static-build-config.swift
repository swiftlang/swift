// RUN: %target-swift-frontend -print-static-build-config -DSOMETHING | %FileCheck %s
// REQUIRES: swift_swift_parser

// CHECK: "attributes":
// CHECK-SAME: "escaping"

// CHECK-SAME: "customConditions":[{{.*"SOMETHING"[^]]*\]}}

// CHECK-SAME: "features":
// CHECK-SAME: "AttachedMacros"
