// Test that -print-target-info infers the host OS when that host OS is macOS
//
// We could duplicate this test for other host platforms.

// RUN: %swift_driver -print-target-info  | %FileCheck %s
// RUN: %target-swift-frontend -print-target-info | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: "triple": "{{.*}}-apple-macosx{{[0-9][0-9]}}
// CHECK: "unversionedTriple": "{{.*}}-apple-macosx"
