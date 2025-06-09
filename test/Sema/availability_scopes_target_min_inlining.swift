// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -target %target-next-stable-abi-triple -typecheck -dump-availability-scopes -target-min-inlining-version min %s > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace --check-prefix CHECK-%target-os %s < %t.dump

// REQUIRES: swift_stable_abi

// Verify that -target-min-inlining-version min implies the correct OS version
// for the target OS.

// CHECK-macosx: {{^}}(root version=10.10.0
// CHECK-ios: {{^}}(root version=8.0
// CHECK-tvos: {{^}}(root version=9.0
// CHECK-watchos: {{^}}(root version=2.0
// CHECK-xros: {{^}}(root version=1.0

// CHECK-macosx-NEXT: {{^}}  (decl_implicit version=10.15 decl=foo()
// CHECK-ios-NEXT: {{^}}  (decl_implicit version=13 decl=foo()
// CHECK-tvos-NEXT: {{^}}  (decl_implicit version=13 decl=foo()
// CHECK-watchos-NEXT: {{^}}  (decl_implicit version=6 decl=foo()

func foo() {}
