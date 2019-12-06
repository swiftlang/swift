// Test that -print-target-triple infers the host OS when that host OS is macOS
//
// We could duplicate this test for other host platforms.

// RUN: %swift_driver -print-target-triple  | %FileCheck %s
// RUN: %target-swift-frontend -print-target-triple | %FileCheck %s

// REQUIRES: OS=macosx

// CHECK: x86_64-apple-macosx
