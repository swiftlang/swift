// RUN: %empty-directory(%t)
// RUN: %target-build-swift %s -module-name Expansion -emit-module -emit-module-path %t/
// RUN: %target-swift-symbolgraph-extract -module-name Expansion -I %t -pretty-print -output-dir %t
// RUN: %FileCheck %s --input-file %t/Expansion.symbols.json

// CHECK-LABEL: "symbols": [
// CHECK-LABEL: "precise": "s:9Expansion1SV",
// CHECK: "availability": [
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "iOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "deprecated": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: },
// CHECK-NEXT: "message": "Everyone makes mistakes",
// CHECK-NEXT: "renamed": "S2"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "macOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "deprecated": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: },
// CHECK-NEXT: "message": "Everyone makes mistakes",
// CHECK-NEXT: "renamed": "S2"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "tvOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "deprecated": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: },
// CHECK-NEXT: "message": "Everyone makes mistakes",
// CHECK-NEXT: "renamed": "S2"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "visionOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "deprecated": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: },
// CHECK-NEXT: "message": "Everyone makes mistakes",
// CHECK-NEXT: "renamed": "S2"
// CHECK-NEXT: },
// CHECK-NEXT: {
// CHECK-NEXT: "domain": "watchOS",
// CHECK-NEXT: "introduced": {
// CHECK-NEXT: "major": 26
// CHECK-NEXT: },
// CHECK-NEXT: "deprecated": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 2
// CHECK-NEXT: },
// CHECK-NEXT: "obsoleted": {
// CHECK-NEXT: "major": 26,
// CHECK-NEXT: "minor": 4
// CHECK-NEXT: },
// CHECK-NEXT: "message": "Everyone makes mistakes",
// CHECK-NEXT: "renamed": "S2"
// CHECK-NEXT: }
// CHECK-NEXT: ]
// CHECK-NOT: "domain": "Any Apple OS"

@available(anyAppleOS, introduced: 26, deprecated: 26.2, obsoleted: 26.4, message: "Everyone makes mistakes", renamed: "S2")
public struct S {}
