// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: PTRSIZE=64

// A `@called(atMostOnce)` closure that consumes a noncopyable capture moves it
// directly into the closure's context (instead of boxing it), so the
// partial-apply forwarder that runs the closure's underlying implementation
// must take (not copy) the value out of the context, and must not let
// the context's normal, shared per-field destructor destroy it a second
// time when the context itself is released.

public func callAtMostOnce(_ f: @called(atMostOnce) () -> ()) { f() }
public func dontCallAtMostOnce(_ f: @called(atMostOnce) () -> ()) { /* never called */ }
public func callAtMostOnceEscaping(_ f: @escaping @called(atMostOnce) () -> ()) { f() }
public func dontCallAtMostOnceEscaping(_ f: @escaping @called(atMostOnce) () -> ()) { /* never called */ }

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test18dontCallAtMostOnceyyyyXEnF"(ptr %0, ptr %1)
// CHECK: [[CTX_ADDR:%.*]] = getelementptr inbounds{{.*}} %swift.function, ptr %f, i32 0, i32 1
// CHECK: [[CTX:%.*]] = load ptr, ptr [[CTX_ADDR]]
// CHECK: call void @swift_release(ptr [[CTX]])

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test26dontCallAtMostOnceEscapingyyyyXOonF"(ptr %0, ptr %1)
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
// context is drained by the forwarder. Since the context is stack-allocated
// (as any `@called(atMostOnce)` closure now is), it's reclaimed by the caller's
// own `dealloc_stack` -- the forwarder has nothing left to release once the
// capture is taken.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test16allOwnedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK-NOT: call void @swift_deallocUninitializedObject
// CHECK:  tail call swiftcc void @"$s4test16allOwnedCapturesyySiFyyXEfU_"(i64 [[VALUE]])
// CHECK: ret void
public func allOwnedCaptures(_ x: Int) {
  let r = Resource(x: x)
  callAtMostOnce { r.use() }
}

// A closure that mixes a consumed capture (`Direct_Owned`, `r`) with a
// borrowed one (`Direct_Guaranteed`, `t`): the forwarder still needs to
// release the surviving `t` field, but must skip the already-taken `r`
// field. The context itself is stack-allocated, so there's no separate
// "free as uninitialized" call once `t` has been released.
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
// CHECK-NOT: call void @swift_deallocUninitializedObject
// CHECK: ret void
public func mixedCaptures(_ x: Int) {
  let r = Resource(x: x)
  let t = Tracker()
  callAtMostOnce {
    _ = t
    r.use()
  }
}

// A `@called(atMostOnce)` closure that is never called: the context is never
// touched by a forwarder, so it's released through `dontCallAtMostOnce` above
// (i.e. `f`'s ordinary, whole-object release) -- which runs the context's
// normal shared destructor, since nothing was ever taken out of it.
public func neverCalled(_ x: Int) {
  let r = Resource(x: x)
  dontCallAtMostOnce { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test24allOwnedCapturesEscapingyySiFyyXOofU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[FIELD_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
// CHECK:  tail call swiftcc void @"$s4test24allOwnedCapturesEscapingyySiFyyXOofU_"(i64 [[VALUE]])
public func allOwnedCapturesEscaping(_ x: Int) {
  let r = Resource(x: x)
  callAtMostOnceEscaping { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test21mixedCapturesEscapingyySiFyyXOofU_TA"(ptr swiftself %0)
// CHECK:  [[TRACKER_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TRACKER:%.*]] = load ptr, ptr [[TRACKER_ADDR]]
// CHECK:  [[RESOURCE_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 2
// CHECK:  [[X_ADDR:%.*]] = getelementptr inbounds{{.*}} %T4test8ResourceV, ptr [[RESOURCE_ADDR]], i32 0, i32 0
// CHECK:  [[VALUE:%.*]] = load i64, ptr [[X_ADDR]]
// CHECK:  call swiftcc void @"$s4test21mixedCapturesEscapingyySiFyyXOofU_"(ptr [[TRACKER]], i64 [[VALUE]])
// CHECK:  [[TO_DESTROY_ADDR:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %T4test8ResourceV }>, ptr %0, i32 0, i32 1
// CHECK:  [[TO_DESTROY:%.*]] = load ptr, ptr [[TO_DESTROY_ADDR]]
// CHECK:  call void @swift_release(ptr [[TO_DESTROY]])
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
public func mixedCapturesEscaping(_ x: Int) {
  let r = Resource(x: x)
  let t = Tracker()
  callAtMostOnceEscaping {
    _ = t
    r.use()
  }
}

public func neverCalledEscaping(_ x: Int) {
  let r = Resource(x: x)
  dontCallAtMostOnceEscaping { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test19allBorrowedCapturesyySiF"(i64 %0)
// CHECK: [[CTX:%.*]] = alloca i8, i64 32, align 16
// CHECK: call ptr @swift_initStackObject(ptr {{.*}}, ptr [[CTX]])
// CHECK: call swiftcc void @"$s4test14callAtMostOnceyyyyXEnF"(ptr @"$s4test19allBorrowedCapturesyySiFyyXEfU_TA{{(\.ptrauth)?}}", ptr %closure)

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test19allBorrowedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// CHECK: call swiftcc void @"$s4test19allBorrowedCapturesyySiFyyXEfU_"(ptr {{%.*}}, i64 {{%.*}})
// CHECK-NEXT: call void @swift_release(ptr %0)
// CHECK-NEXT: ret void
public func allBorrowedCaptures(_ x: Int) {
  let t = Tracker()
  callAtMostOnce {
    _ = t
    _ = x
  }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test30allBorrowedCapturesNeverCalledyySiF"(i64 %0)
// CHECK: [[CTX:%.*]] = alloca i8, i64 32, align 16
// CHECK: call ptr @swift_initStackObject(ptr {{.*}}, ptr [[CTX]])
// CHECK: call swiftcc void @"$s4test18dontCallAtMostOnceyyyyXEnF"(ptr @"$s4test30allBorrowedCapturesNeverCalledyySiFyyXEfU_TA{{(\.ptrauth)?}}", ptr %closure)
//
// CHECK-LABEL: define{{.*}} void @__swift_closure_destructor{{.*}}(ptr swiftself %0)
// CHECK: [[FIELD:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, ptr, %TSi }>, ptr %0, i32 0, i32 1
// CHECK: [[TRACKER:%.*]] = load ptr, ptr [[FIELD]]
// CHECK: call void @swift_release(ptr [[TRACKER]])
// CHECK-NOT: call void @swift_deallocObject
// CHECK: ret void
public func allBorrowedCapturesNeverCalled(_ x: Int) {
  let t = Tracker()
  dontCallAtMostOnce {
    _ = t
    _ = x
  }
}

struct BorrowableValue: ~Copyable {
  var t: Tracker
  borrowing func peek() -> Tracker { t }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test26noncopyableBorrowedCaptureyySiF"(i64 %0)
// CHECK: [[CTX:%.*]] = alloca i8, i64 24, align 16
// CHECK-NOT: call ptr @swift_retain
// CHECK: call ptr @swift_initStackObject(ptr {{.*}}, ptr [[CTX]])
// CHECK: call swiftcc void @"$s4test14callAtMostOnceyyyyXEnF"(ptr @"$s4test26noncopyableBorrowedCaptureyySiFyyXEfU_TA{{(\.ptrauth)?}}", ptr %closure)
public func noncopyableBorrowedCapture(_ x: Int) {
  let v = BorrowableValue(t: Tracker())
  callAtMostOnce {
    _ = v.peek()
  }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test26noncopyableBorrowedCaptureyySiFyyXEfU_TA"(ptr swiftself %0)
// CHECK: call swiftcc void @"$s4test26noncopyableBorrowedCaptureyySiFyyXEfU_"(ptr {{%.*}})
// CHECK-NEXT: call void @swift_release(ptr %0)
// CHECK-NEXT: ret void

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test37noncopyableBorrowedCaptureNeverCalledyySiF"(i64 %0)
// CHECK: [[CTX:%.*]] = alloca i8, i64 24, align 16
// CHECK-NOT: call ptr @swift_retain
// CHECK: call ptr @swift_initStackObject(ptr {{.*}}, ptr [[CTX]])
// CHECK: call swiftcc void @"$s4test18dontCallAtMostOnceyyyyXEnF"(ptr @"$s4test37noncopyableBorrowedCaptureNeverCalledyySiFyyXEfU_TA{{(\.ptrauth)?}}", ptr %closure)
public func noncopyableBorrowedCaptureNeverCalled(_ x: Int) {
  let v = BorrowableValue(t: Tracker())
  dontCallAtMostOnce {
    _ = v.peek()
  }
}

// CHECK-LABEL: define{{.*}} void @__swift_closure_destructor{{.*}}(ptr swiftself %0)
// CHECK-NOT: call void @swift_release
// CHECK-NOT: call void @swift_deallocObject
// CHECK: ret void

public struct GenericResource<T>: ~Copyable {
  var x: T
  public init(x: T) { self.x = x }
  deinit { print("GenericResource") }
  public consuming func use() { print("used") }
  public mutating func mutate() { print("mutated") }
}

// A noncopyable address-only value that the closure consumes is owned by the
// closure's context, even when the context is stack-allocated. The forwarder
// moves the value out of the context into a temporary for the call, so the
// context has nothing left to destroy or release.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test19genericOwnedCaptureyyx_AA15GenericResourceVyxGntlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr [[FIELD]], ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK-NOT: call void @swift_release
// CHECK-NOT: call void @swift_deallocUninitializedObject
// CHECK:  call swiftcc void @"$s4test19genericOwnedCaptureyyx_AA15GenericResourceVyxGntlFyyXEfU_"(ptr noalias %arg.temp, ptr %T)
// CHECK-NEXT: ret void
public func genericOwnedCapture<T>(_ x: T, _ r: consuming GenericResource<T>) {
  callAtMostOnce { r.use() }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test27genericOwnedCaptureEscapingyyx_AA15GenericResourceVyxGntlFyyXOofU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr [[FIELD]], ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
// CHECK:  call swiftcc void @"$s4test27genericOwnedCaptureEscapingyyx_AA15GenericResourceVyxGntlFyyXOofU_"(ptr noalias %arg.temp, ptr %T)
public func genericOwnedCaptureEscaping<T>(_ x: T, _ r: consuming GenericResource<T>) {
  callAtMostOnceEscaping { r.use() }
}

// A noncopyable address-only value that the closure mutates is captured inout,
// so the context only holds its address. The forwarder passes that address
// through without moving, copying, or destroying the value.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test19genericInoutCaptureyyAA15GenericResourceVyxGzlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD:%.*]] = getelementptr inbounds{{.*}} <{ %swift.refcounted, [8 x i8], ptr }>, ptr %0, i32 0, i32 2
// CHECK:  [[ADDR:%.*]] = load ptr, ptr [[FIELD]]
// CHECK-NOT: @"$s4test15GenericResourceVyxGlWO
// CHECK:  call swiftcc void @"$s4test19genericInoutCaptureyyAA15GenericResourceVyxGzlFyyXEfU_"(ptr [[ADDR]], ptr %T)
// CHECK-NEXT: ret void
public func genericInoutCapture<T>(_ r: inout GenericResource<T>) {
  callAtMostOnce { r.mutate() }
}

// The context owns the consumed value but only holds the address of the
// mutated value. Neither the caller nor the context's destructor destroys the
// mutated value, and the forwarder only moves the consumed value.
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlF"(ptr noalias %0, ptr noalias %1, ptr %T)
// CHECK:  [[CTX:%.*]] = call ptr @swift_initStackObject(
// CHECK:  [[OWNED_FIELD:%.*]] = getelementptr inbounds i8, ptr [[CTX]], i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr {{%.*}}, ptr [[OWNED_FIELD]], ptr %T, ptr {{%.*}})
// CHECK:  [[INOUT_FIELD:%.*]] = getelementptr inbounds i8, ptr [[CTX]], i64 {{%.*}}
// CHECK:  store ptr %1, ptr [[INOUT_FIELD]]
// CHECK:  call swiftcc void @"$s4test14callAtMostOnceyyyyXEnF"(ptr @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlFyyXEfU_TA{{(\.ptrauth)?}}", ptr [[CTX]])
// CHECK-NOT: @"$s4test15GenericResourceV
// CHECK:  ret void
//
// CHECK-LABEL: define{{.*}} void @__swift_closure_destructor{{.*}}(ptr swiftself %0)
// CHECK:  [[OWNED_FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK-NEXT: call swiftcc void @"$s4test15GenericResourceVfD"(ptr {{%.*}}, ptr noalias swiftself [[OWNED_FIELD]])
// CHECK-NEXT: ret void
//
// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[OWNED_FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr [[OWNED_FIELD]], ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK:  [[INOUT_FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  [[INOUT_ADDR:%.*]] = load ptr, ptr [[INOUT_FIELD]]
// CHECK-NOT: call void @swift_release
// CHECK-NOT: @"$s4test15GenericResourceVyxGlWO
// CHECK:  call swiftcc void @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlFyyXEfU_"(ptr noalias %arg.temp, ptr [[INOUT_ADDR]], ptr %T)
// CHECK-NEXT: ret void
public func genericOwnedAndInoutCaptures<T>(_ c: consuming GenericResource<T>, _ r: inout GenericResource<T>) {
  callAtMostOnce {
    c.use()
    r.mutate()
  }
}
