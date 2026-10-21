// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature OnceFunctions -target %target-future-triple %s | %FileCheck %s

// REQUIRES: swift_feature_OnceFunctions
// REQUIRES: PTRSIZE=64

public final class Box {}

public struct Resource: ~Copyable {
  var box: Box
  deinit {}
  public consuming func use() {}
  public borrowing func peek() {}
}

// An exactly-once closure never adopts one of its captures as its context, even
// when that capture is a single reference-counted object.
public func makeWithBox(_ box: Box) -> @once () -> Void {
  return { _ = box }
}

public func makeWithResource(
  _ resource: consuming Resource, _ box: Box
) -> @once () -> Void {
  return { _ = box; _ = consume resource }
}

// CHECK-LABEL: define{{.*}} swiftcc {{.*}} @"$s4test11makeWithBox{{.*}}"
// CHECK: call noalias ptr @swift_allocObject(ptr getelementptr inbounds{{.*}} @metadata, i32 0, i32 2), i64 24

// The destructor terminates the program when the closure is dropped.
// CHECK-LABEL: define internal swiftcc void @__swift_exactly_once_closure_destructor(ptr swiftself %0)
// CHECK: call void @swift_onceClosureDestroyedError(ptr %0)
// CHECK-NEXT: unreachable

// Even when the call consumes no field, the forwarder destroys the fields and
// deallocates the context as uninitialized. It never releases the context.
// CHECK-LABEL: define internal swiftcc void @"$s4test11makeWithBox{{.*}}TA"(ptr swiftself %0)
// CHECK-NOT: swift_release(ptr %0)
// CHECK: call void @swift_release(ptr %{{.*}})
// CHECK: call void @swift_deallocUninitializedObject(ptr %0,

// The forwarder moves the consumed capture into the call, destroys the capture
// that the call didn't consume, and deallocates the context as uninitialized.
// CHECK-LABEL: define internal swiftcc void @"$s4test16makeWithResource{{.*}}TA"(ptr swiftself %0)
// CHECK-NOT: swift_release(ptr %0)
// CHECK: call swiftcc void @"$s4test16makeWithResource{{.*}}"(
// CHECK: call void @swift_release(ptr %{{.*}})
// CHECK: call void @swift_deallocUninitializedObject(ptr %0,

// A stack-promoted context is initialized with the same trapping destructor. The
// caller never releases it, because the callee consumes the closure. The
// forwarder destroys the fields, but it must not release or deallocate the
// context, since the caller's frame owns the storage.
public func take(_ f: @once () -> Void) { f() }

public func makeOnStack(_ box: Box) {
  take { _ = box }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test11makeOnStackyyAA3BoxCF"(ptr %0)
// CHECK: call ptr @swift_initStackObject(ptr getelementptr inbounds{{.*}} @metadata.{{[0-9]+}}
// CHECK-NOT: swift_release
// CHECK: ret void

// CHECK-LABEL: define internal swiftcc void @"$s4test11makeOnStack{{.*}}TA"(ptr swiftself %0)
// CHECK-NOT: swift_release(ptr %0)
// CHECK: call void @swift_release(ptr %{{.*}})
// CHECK-NOT: swift_deallocUninitializedObject
// CHECK: ret void

public func takeEscaping(_ f: @escaping @once () -> Void) { f() }

public struct GenericResource<T>: ~Copyable {
  var x: T
  deinit {}
  public consuming func use() {}
  public mutating func mutate() {}
}

// A closure that mixes a consumed noncopyable capture with a borrowed one. The
// stack-allocated context only holds a bitwise copy of the borrowed value,
// because the enclosing function still owns it, so the forwarder must not
// destroy that field once the call has taken the consumed one.
//
// CHECK-LABEL: define internal swiftcc void @"$s4test24mixedNoncopyableCapturesyyAA3BoxCFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  call swiftcc void @"$s4test24mixedNoncopyableCapturesyyAA3BoxCFyyXEfU_"(
// CHECK-NEXT: ret void
public func mixedNoncopyableCaptures(_ box: Box) {
  let consumed = Resource(box: box)
  let borrowed = Resource(box: box)
  take {
    borrowed.peek()
    consumed.use()
  }
}

// A noncopyable address-only value that the closure consumes is owned by the
// closure's context, even when the context is stack-allocated. The forwarder
// moves the value out of the context into a temporary for the call, so there
// is nothing left to destroy or release.
//
// CHECK-LABEL: define internal swiftcc void @"$s4test19genericOwnedCaptureyyx_AA15GenericResourceVyxGntlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr [[FIELD]], ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK-NOT: call void @swift_release
// CHECK-NOT: call void @swift_deallocUninitializedObject
// CHECK:  call swiftcc void @"$s4test19genericOwnedCaptureyyx_AA15GenericResourceVyxGntlFyyXEfU_"(ptr noalias %arg.temp, ptr %T)
// CHECK-NEXT: ret void
public func genericOwnedCapture<T>(_ x: T, _ r: consuming GenericResource<T>) {
  take { r.use() }
}

// CHECK-LABEL: define internal swiftcc void @"$s4test27genericOwnedCaptureEscapingyyx_AA15GenericResourceVyxGntlFyyXOfU_TA"(ptr swiftself %0)
// CHECK:  [[FIELD:%.*]] = getelementptr inbounds i8, ptr %0, i64 {{%.*}}
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr [[FIELD]], ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK:  call void @swift_deallocUninitializedObject(ptr %0,
// CHECK:  call swiftcc void @"$s4test27genericOwnedCaptureEscapingyyx_AA15GenericResourceVyxGntlFyyXOfU_"(ptr noalias %arg.temp, ptr %T)
public func genericOwnedCaptureEscaping<T>(_ x: T, _ r: consuming GenericResource<T>) {
  takeEscaping { r.use() }
}

// A closure that mutates a noncopyable value captures its address, so the
// context doesn't own the value and the forwarder only passes the address
// along.
//
// CHECK-LABEL: define internal swiftcc void @"$s4test19genericInoutCaptureyyAA15GenericResourceVyxGzlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  [[ADDR_FIELD:%.*]] = getelementptr inbounds nuw <{ %swift.refcounted, [8 x i8], ptr }>, ptr %0, i32 0, i32 2
// CHECK-NEXT: [[ADDR:%.*]] = load ptr, ptr [[ADDR_FIELD]]
// CHECK-NEXT: tail call swiftcc void @"$s4test19genericInoutCaptureyyAA15GenericResourceVyxGzlFyyXEfU_"(ptr [[ADDR]], ptr %T)
// CHECK-NEXT: ret void
public func genericInoutCapture<T>(_ r: inout GenericResource<T>) {
  take { r.mutate() }
}

// A closure that consumes one value and mutates another owns only the
// consumed value.
//
// CHECK-LABEL: define internal swiftcc void @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlFyyXEfU_TA"(ptr swiftself %0)
// CHECK:  call ptr @"$s4test15GenericResourceVyxGlWOb"(ptr {{%.*}}, ptr %arg.temp, ptr %T, ptr {{%.*}})
// CHECK:  [[ADDR:%.*]] = load ptr, ptr {{%.*}}
// CHECK-NOT: call void @swift_release
// CHECK-NOT: call void @swift_deallocUninitializedObject
// CHECK:  call swiftcc void @"$s4test28genericOwnedAndInoutCapturesyyAA15GenericResourceVyxGn_AEztlFyyXEfU_"(ptr noalias %arg.temp, ptr [[ADDR]], ptr %T)
// CHECK-NEXT: ret void
public func genericOwnedAndInoutCaptures<T>(_ c: consuming GenericResource<T>, _ r: inout GenericResource<T>) {
  take {
    c.use()
    r.mutate()
  }
}

// Every context in the module has the same destructor, so there's only one.
// CHECK-NOT: define internal swiftcc void @__swift_exactly_once_closure_destructor.{{[0-9]+}}
