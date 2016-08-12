// RUN: %target-swift-frontend -parse-as-library -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -parse-as-library -O -emit-ir %s | %FileCheck --check-prefix=OPT-CHECK %s

// Test that runtime functions are invoked using the new calling convention.
// Test that wrappers are used to invoked them.

// TODO: Once the PreserveRegistersCC calling convention is enabled, check that it is really used.

public class C {
}

// CHECK-LABEL: define {{(protected )?}}void @_TF27runtime_calling_conventions3fooFCS_1CT_(%C27runtime_calling_conventions1C*)
// Check that runtime functions use a proper calling convention.
// CHECK: call void {{.*}} @rt_swift_release

// OPT-CHECK-LABEL: define {{(protected )?}}void @_TF27runtime_calling_conventions3fooFCS_1CT_(%C27runtime_calling_conventions1C*)
// Check that runtime functions use a proper calling convention.
// OPT-CHECK: tail call void @rt_swift_release

public func foo(_ c: C) {
}

// Check that wrappers were generated, have a proper calling convention, linkage
// and linkonce_odr flags.

// CHECK: define linkonce_odr hidden void @rt_swift_release(%swift.refcounted*) #[[ATTR_REF:[0-9]+]]
// CHECK: load void (%swift.refcounted*)*, void (%swift.refcounted*)** @_swift_release
// CHECK-NEXT: tail call void %load(%swift.refcounted* %0)
// CHECK-NEXT: ret void
// CHECK: attributes #[[ATTR_REF]] = { noinline nounwind }

// OPT-CHECK: define linkonce_odr hidden void @rt_swift_release(%swift.refcounted*) #[[ATTR_REF:[0-9]+]]
// OPT-CHECK: load void (%swift.refcounted*)*, void (%swift.refcounted*)** @_swift_release
// OPT-CHECK-NEXT: tail call void %load(%swift.refcounted* %0)
// OPT-CHECK-NEXT: ret void
// OPT-CHECK: attributes #[[ATTR_REF]] = { noinline nounwind }
