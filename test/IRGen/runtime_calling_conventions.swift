
// RUN: %target-swift-frontend -module-name runtime_calling_conventions -assume-parsing-unqualified-ownership-sil -parse-as-library -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -module-name runtime_calling_conventions -assume-parsing-unqualified-ownership-sil -parse-as-library -O -emit-ir %s | %FileCheck --check-prefix=OPT-CHECK %s

// Test that runtime functions are invoked using the new calling convention.

public class C {
}

// CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s27runtime_calling_conventions3fooyyAA1CCF"(%T27runtime_calling_conventions1CC*)
// Check that runtime functions use a proper calling convention.
// CHECK-NOT: call void {{.*}} @swift_release

// OPT-CHECK-LABEL: define {{(dllexport )?}}{{(protected )?}}swiftcc void @"$s27runtime_calling_conventions3fooyyAA1CCF"(%T27runtime_calling_conventions1CC* nocapture)
// Check that runtime functions use a proper calling convention.
// OPT-CHECK-NOT: tail call void @swift_release

public func foo(_ c: C) {
}
