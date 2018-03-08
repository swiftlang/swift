// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -parse-as-library -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -assume-parsing-unqualified-ownership-sil -parse-as-library -O -emit-ir %s | %FileCheck --check-prefix=OPT-CHECK %s

// Test that runtime functions are invoked using the new calling convention.

public class C {
}

// CHECK-LABEL: define {{(protected )?}}swiftcc void @"$S27runtime_calling_conventions3fooyyAA1CCF"(%T27runtime_calling_conventions1CC*)
// Check that runtime functions use a proper calling convention.
// CHECK: call void {{.*}} @swift_release

// OPT-CHECK-LABEL: define {{(protected )?}}swiftcc void @"$S27runtime_calling_conventions3fooyyAA1CCF"(%T27runtime_calling_conventions1CC*)
// Check that runtime functions use a proper calling convention.
// OPT-CHECK: tail call void @swift_release

public func foo(_ c: C) {
}
