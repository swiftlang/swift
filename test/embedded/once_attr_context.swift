// RUN: %target-swift-emit-ir %s -parse-as-library -wmo -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-ptrauth

// RUN: %target-swift-emit-ir %s -parse-as-library -wmo -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -D AT_MOST_ONCE | %FileCheck %s --check-prefix=AT-MOST-ONCE

// REQUIRES: swift_feature_Embedded
// REQUIRES: swift_feature_OnceFunctions

public struct Resource: ~Copyable {
  var x: Int
  deinit {}
}

public func make(_ resource: consuming Resource) -> @once () -> Void {
  return { _ = consume resource }
}

public func makeCaptureless() -> @once () -> Void {
  return {}
}

// CHECK-noptrauth: @__swift_exactly_once_closure_context = internal global %swift.refcounted { ptr @metadata{{[.0-9]*}}, i64 1 }
// CHECK-ptrauth: [[METADATA_PTRAUTH:@metadata[.0-9]*ptrauth]] = private constant { ptr, i32, i64, i64 } { ptr @metadata{{[.0-9]*}}, i32 2, i64 ptrtoint (ptr @__swift_exactly_once_closure_context to i64), i64 27361 }, section "llvm.ptrauth"
// CHECK-ptrauth: @__swift_exactly_once_closure_context = internal global %swift.refcounted { ptr [[METADATA_PTRAUTH]], i64 1 }
// CHECK-NOT: @llvm.global_ctors
// CHECK-NOT: call ptr @swift_initStackObject
// CHECK-NOT: swift_onceClosureDestroyedError

// CHECK-LABEL: define internal swiftcc void @__swift_exactly_once_closure_destructor(ptr swiftself %0)
// CHECK-NEXT: entry:
// CHECK-NEXT: call void @llvm.trap()
// CHECK-NEXT: unreachable
// CHECK-NOT: swift_onceClosureDestroyedError
// CHECK-NOT: call ptr @swift_initStackObject

#if AT_MOST_ONCE
public final class Tracker {}

public func callAtMostOnce(_ f: @callable(once) () -> Void) { f() }

// The context of a non-escaping at-most-once closure is on the stack, so its
// forwarder destroys the context without releasing it.
// AT-MOST-ONCE-LABEL: define {{.*}}swiftcc void @"$e{{.*}}16borrowedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// AT-MOST-ONCE-NOT: swift_release
// AT-MOST-ONCE: call swiftcc void @__swift_closure_destructor{{.*}}(ptr swiftself %0)
// AT-MOST-ONCE-NOT: swift_release
// AT-MOST-ONCE: ret void
public func borrowedCaptures(_ x: Int) {
  let t = Tracker()
  callAtMostOnce {
    _ = t
    _ = x
  }
}
#endif
