// RUN: %target-swift-frontend -parse-stdlib -primary-file %s -emit-ir -module-name M | %FileCheck %s --check-prefixes=CHECK,BACKDEPLOY
// RUN: %target-swift-frontend -parse-stdlib -target %target-future-triple -primary-file %s -emit-ir -module-name M | %FileCheck %s --check-prefixes=CHECK,CURRENT

// REQUIRES: OS=macosx

// The dynamic class of an allocation may have an `@allocator` unless the
// static class is final. When deploying to runtimes that predate class
// allocators, the entry point that finds the allocator is weakly linked and
// only called if it is present.

import Swift

public class NonFinal {}
public final class Final {}

// CHECK-LABEL: define {{.*}}@"$s1M12makeNonFinalyAA0bC0CADm_BwtF"(
// BACKDEPLOY: [[AVAILABLE:%.*]] = icmp ne ptr @swift_allocObjectMaybeClassAllocated, null
// BACKDEPLOY: br i1 [[AVAILABLE]], label %class-allocator, label %no-class-allocator
// BACKDEPLOY: class-allocator:
// BACKDEPLOY: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// BACKDEPLOY: no-class-allocator:
// BACKDEPLOY: call noalias ptr @swift_allocObject(
// BACKDEPLOY: phi ptr
// CURRENT-NOT: icmp ne ptr @swift_allocObjectMaybeClassAllocated
// CURRENT: call noalias ptr @swift_allocObjectMaybeClassAllocated(
// CURRENT-NOT: @swift_allocObject(
// CHECK: }
public func makeNonFinal(_ t: NonFinal.Type, _ n: Builtin.Word) -> NonFinal {
  return Builtin.allocWithTailElems_1(t, n, Final.self)
}

// BACKDEPLOY: declare extern_weak ptr @swift_allocObjectMaybeClassAllocated(
// CURRENT: declare ptr @swift_allocObjectMaybeClassAllocated(

// CHECK-LABEL: define {{.*}}@"$s1M9makeFinalyAA0B0CADm_BwtF"(
// CHECK-NOT: swift_allocObjectMaybeClassAllocated
// CHECK: call noalias ptr @swift_allocObject(
// CHECK: }
public func makeFinal(_ t: Final.Type, _ n: Builtin.Word) -> Final {
  return Builtin.allocWithTailElems_1(t, n, Final.self)
}
