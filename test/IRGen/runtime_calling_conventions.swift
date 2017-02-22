// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -parse-as-library -emit-ir %s | %FileCheck %s
// RUN: %target-swift-frontend -Xllvm -new-mangling-for-tests -assume-parsing-unqualified-ownership-sil -parse-as-library -O -emit-ir %s | %FileCheck --check-prefix=OPT-CHECK %s

// Test that runtime functions are invoked using the new calling convention.
// Test that wrappers are used to invoked them.

// TODO: Once the PreserveRegistersCC calling convention is enabled, check that it is really used.

public class C {
}

// CHECK-LABEL: define {{(protected )?}}swiftcc void @_T027runtime_calling_conventions3fooyAA1CCF(%T27runtime_calling_conventions1CC*)
// Check that runtime functions use a proper calling convention.
// CHECK: call void {{.*}} @swift_rt_swift_release

// OPT-CHECK-LABEL: define {{(protected )?}}swiftcc void @_T027runtime_calling_conventions3fooyAA1CCF(%T27runtime_calling_conventions1CC*)
// Check that runtime functions use a proper calling convention.
// OPT-CHECK: tail call void @swift_rt_swift_release

public func foo(_ c: C) {
}

// Check that wrappers were generated, have a proper calling convention, linkage
// and linkonce_odr flags.

// CHECK: define linkonce_odr hidden void @swift_rt_swift_release(%swift.refcounted*) #[[ATTR_REF:[0-9]+]]
// CHECK: load void (%swift.refcounted*)*, void (%swift.refcounted*)** @_swift_release
// CHECK-NEXT: tail call void %load(%swift.refcounted* %0)
// CHECK-NEXT: ret void
// CHECK: attributes #[[ATTR_REF]] = { noinline nounwind }

// OPT-CHECK: define linkonce_odr hidden void @swift_rt_swift_release(%swift.refcounted*) local_unnamed_addr #[[ATTR_REF:[0-9]+]]
// OPT-CHECK: load void (%swift.refcounted*)*, void (%swift.refcounted*)** @_swift_release
// OPT-CHECK-NEXT: tail call void %load(%swift.refcounted* %0)
// OPT-CHECK-NEXT: ret void
// OPT-CHECK: attributes #[[ATTR_REF]] = { noinline nounwind }
