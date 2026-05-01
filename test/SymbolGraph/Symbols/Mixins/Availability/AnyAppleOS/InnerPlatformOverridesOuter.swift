// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InnerPlatformOverridesOuter -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InnerPlatformOverridesOuter -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InnerPlatformOverridesOuter.symbols.json

// CHECK-LABEL: "symbols": [
// CHECK-LABEL: "precise": "s:27InnerPlatformOverridesOuter0D0V0A0V",
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
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "watchOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: ]

@available(anyAppleOS, introduced: 26)
public struct Outer {
  @available(macOS, introduced: 26.4)
  @available(iOS, introduced: 26.2, obsoleted: 26.4)
  @available(watchOS 26.2, *)
  @available(tvOS, deprecated)
  public struct Inner {}
}
