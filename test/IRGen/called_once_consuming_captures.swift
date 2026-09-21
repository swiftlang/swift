// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: PTRSIZE=64

// A `@called(once)` closure that consumes a noncopyable capture moves it
// directly into the closure's context (instead of boxing it), so the
// partial-apply forwarder that runs the closure's underlying implementation
// must take (not copy) the value out of the context, and must not let
// the context's normal, shared per-field destructor destroy it a second
// time when the context itself is released.

public func callOnce(_ f: @called(once) () -> ()) { f() }
public func dontCallOnce(_ f: @called(once) () -> ()) { /* never called */ }
public func callOnceEscaping(_ f: @escaping @called(once) () -> ()) { f() }
public func dontCallOnceEscaping(_ f: @escaping @called(once) () -> ()) { /* never called */ }

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test12dontCallOnceyyyyXEnF"(ptr %0, ptr %1)
// CHECK: [[CTX_ADDR:%.*]] = getelementptr inbounds{{.*}} %swift.function, ptr %f, i32 0, i32 1
// CHECK: [[CTX:%.*]] = load ptr, ptr [[CTX_ADDR]]
// CHECK: call void @swift_release(ptr [[CTX]])

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test20dontCallOnceEscapingyyyyXOnF"(ptr %0, ptr %1)
// CHECK: [[CTX_ADDR:%.*]] = getelementptr inbounds{{.*}} %swift.function, ptr %f, i32 0, i32 1
// CHECK: [[CTX:%.*]] = load ptr, ptr [[CTX_ADDR]]
// CHECK: call void @swift_release(ptr [[CTX]])

public struct Resource: ~Copyable {
  var x: Int
  public init(x: Int) { self.x = x }
  deinit { print("Resource") }
  public consuming func use() { print("used \(x)") }
}

final class Tracker {
  deinit { print("Tracker") }
}

// A closure whose only capture is consumed (`Direct_Owned`): the whole
// context is drained by the forwarder, so it's deallocated as if it were
// never initialized -- no field destructor runs on release.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test16allOwnedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
// CHECK:  tail call swiftcc void @"$s4test16allOwnedCapturesyySiFyyXEfU_"(i64 [[VALUE]])
public func allOwnedCaptures(_ x: Int) {
  let r = Resource(x: x)
  callOnce { r.use() }
}

// A closure that mixes a consumed capture (`Direct_Owned`, `r`) with a
// borrowed one (`Direct_Guaranteed`, `t`): the forwarder still needs to
// release the surviving `t` field, but must skip the already-taken `r`
// field, then free the context's memory as uninitialized rather than run
// its normal shared destructor over every field.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test13mixedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[TRACKER_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TRACKER:%.*]] = load ptr, ptr [[TRACKER_ADDR]]
// CHECK:  [[RESOURCE_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 2
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[RESOURCE_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call swiftcc void @"$s4test13mixedCapturesyySiFyyXEfU_"(ptr [[TRACKER]], i64 [[VALUE]])
// CHECK:  [[TO_DESTROY_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TO_DESTROY:%.*]] = load ptr, ptr [[TO_DESTROY_ADDR]]
// CHECK:  call void @swift_release(ptr [[TO_DESTROY]])
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
public func mixedCaptures(_ x: Int) {
  let r = Resource(x: x)
  let t = Tracker()
  callOnce {
    _ = t
    r.use()
  }
}

// A `@called(once)` closure that is never called: the context is never
// touched by a forwarder, so it's released through `dontCallOnce` above
// (i.e. `f`'s ordinary, whole-object release) -- which runs the context's
// normal shared destructor, since nothing was ever taken out of it.
public func neverCalled(_ x: Int) {
  let r = Resource(x: x)
  dontCallOnce { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test24allOwnedCapturesEscapingyySiFyyXOfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
// CHECK:  tail call swiftcc void @"$s4test24allOwnedCapturesEscapingyySiFyyXOfU_"(i64 [[VALUE]])
public func allOwnedCapturesEscaping(_ x: Int) {
  let r = Resource(x: x)
  callOnceEscaping { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test21mixedCapturesEscapingyySiFyyXOfU_TA"(ptr swiftself %0)
// CHECK:  [[TRACKER_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TRACKER:%.*]] = load ptr, ptr [[TRACKER_ADDR]]
// CHECK:  [[RESOURCE_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 2
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[RESOURCE_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call swiftcc void @"$s4test21mixedCapturesEscapingyySiFyyXOfU_"(ptr [[TRACKER]], i64 [[VALUE]])
// CHECK:  [[TO_DESTROY_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TO_DESTROY:%.*]] = load ptr, ptr [[TO_DESTROY_ADDR]]
// CHECK:  call void @swift_release(ptr [[TO_DESTROY]])
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
public func mixedCapturesEscaping(_ x: Int) {
  let r = Resource(x: x)
  let t = Tracker()
  callOnceEscaping {
    _ = t
    r.use()
  }
}

public func neverCalledEscaping(_ x: Int) {
  let r = Resource(x: x)
  dontCallOnceEscaping { r.use() }
}
