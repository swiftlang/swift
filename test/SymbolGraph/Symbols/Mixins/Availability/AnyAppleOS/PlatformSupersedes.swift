// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name PlatformSupersedes -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name PlatformSupersedes -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/PlatformSupersedes.symbols.json

// CHECK-LABEL: "precise": "s:18PlatformSupersedes1SV",
// CHECK: "availability": [
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "iOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: }
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "macOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: }
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "tvOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "isUnconditionallyDeprecated": true
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "visionOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: }
// CHECK-NEXT: },
// FIXME: [availability] watchOS introduction should be 11
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "watchOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: ]

@available(anyAppleOS, introduced: 26)
@available(macOS, introduced: 26.4)
@available(iOS, introduced: 26.2, obsoleted: 26.4)
@available(watchOS 11, *)
@available(tvOS, deprecated)
public struct S {}
