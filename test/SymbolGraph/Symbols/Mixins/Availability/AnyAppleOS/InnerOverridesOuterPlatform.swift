// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name InnerOverridesOuterPlatform -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name InnerOverridesOuterPlatform -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/InnerOverridesOuterPlatform.symbols.json

// CHECK-LABEL: "symbols": [
// CHECK-LABEL: "precise": "s:27InnerOverridesOuterPlatform0C0V0A0V",
// CHECK: "availability": [
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "iOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
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
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: }
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "visionOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: }
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "watchOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: }
// CHECK-NEXT: }
// CHECK-NEXT: ]

@available(macOS 26, *)
public struct Outer {
  @available(anyAppleOS 26.4, *)
  public struct Inner {}
}
