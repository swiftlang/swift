// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name MessageLastWins -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name MessageLastWins -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/MessageLastWins.symbols.json

// REQUIRES: OS=macosx

@available(macOS, deprecated, message: "first")
@available(macOS, deprecated, message: "second")
@available(iOS, deprecated, message: "iOS")
public func foo() {}

// CHECK-LABEL: "precise": "s:15MessageLastWins3fooyyF",
// CHECK: "availability": [
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "macOS",
// CHECK-NEXT:     "message": "second",
// CHECK-NEXT:     "isUnconditionallyDeprecated": true
// CHECK-NEXT:   },
// CHECK-NEXT:   {
// CHECK-NEXT:     "domain": "iOS",
// CHECK-NEXT:     "message": "iOS",
// CHECK-NEXT:     "isUnconditionallyDeprecated": true
// CHECK-NEXT:   }
// CHECK-NEXT: ]
