// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name OnlyActive -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name OnlyActive -I %t -pretty-print -active-platform-availability-only -output-dir %t
// RUN: %FileCheck %s --input-file %t/OnlyActive.symbols.json

// REQUIRES: OS=macosx

// CHECK-LABEL: "symbols": [
// CHECK-LABEL: "precise": "s:10OnlyActive1SV",
// CHECK: "availability": [
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "macOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: ]

@available(anyAppleOS 26, *)
public struct S {}
