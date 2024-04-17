// RUN: %target-swift-frontend -swift-version 5 -enable-library-evolution -target %target-next-stable-abi-triple -typecheck -dump-type-refinement-contexts -target-min-inlining-version min %s > %t.dump 2>&1
// RUN: %FileCheck --strict-whitespace --check-prefix CHECK-%target-os %s < %t.dump

// REQUIRES: swift_stable_abi

// Verify that -target-min-inlining-version min implies the correct OS version
// for the target OS.

// CHECK-macosx: {{^}}(root versions=[10.10.0,+Inf)
// CHECK-ios: {{^}}(root versions=[8.0,+Inf)
// CHECK-tvos: {{^}}(root versions=[9.0,+Inf)
// CHECK-watchos: {{^}}(root versions=[2.0,+Inf)
// CHECK-xros: {{^}}(root versions=[1.0,+Inf)

// CHECK-macosx-NEXT: {{^}}  (decl_implicit versions=[10.15,+Inf) decl=foo()
// CHECK-ios-NEXT: {{^}}  (decl_implicit versions=[13,+Inf) decl=foo()
// CHECK-tvos-NEXT: {{^}}  (decl_implicit versions=[13,+Inf) decl=foo()
// CHECK-watchos-NEXT: {{^}}  (decl_implicit versions=[6,+Inf) decl=foo()

func foo() {}
