// RUN: %target-swift-emit-ir %s -parse-as-library -wmo -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions | %FileCheck %s --check-prefixes=CHECK,CHECK-%target-ptrauth

// RUN: %target-swift-emit-ir %s -parse-as-library -wmo -enable-experimental-feature Embedded -enable-experimental-feature OnceFunctions -D AT_MOST_ONCE | %FileCheck %s --check-prefixes=AT-MOST-ONCE,AT-MOST-ONCE-%target-ptrauth

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

// The context of a non-escaping at-most-once closure is on the stack, and it
// borrows the captures that the closure doesn't consume, so its forwarder
// neither destroys nor releases the context.
// AT-MOST-ONCE-LABEL: define {{.*}}swiftcc void @"$e{{.*}}16borrowedCapturesyySiFyyXEfU_TA"(ptr swiftself %0)
// AT-MOST-ONCE-NOT: swift_release
// AT-MOST-ONCE-NOT: __swift_closure_destructor
// AT-MOST-ONCE: call swiftcc void @"$e{{.*}}16borrowedCapturesyySiFyyXEfU_"(
// AT-MOST-ONCE-NEXT: ret void
public func borrowedCaptures(_ x: Int) {
  let t = Tracker()
  callAtMostOnce {
    _ = t
    _ = x
  }
}

@inline(never)
public func dropIf(_ flag: Bool, _ f: @callable(once) () -> Void) {
  if flag { f() }
}

// Dropping an at-most-once closure calls the destructor of its context
// directly instead of releasing the context.
// AT-MOST-ONCE-LABEL: define {{.*}}swiftcc void @"$e{{.*}}6dropIfyySb_yyXEntF"(i1 %0, ptr %1, ptr %2)
// AT-MOST-ONCE-NOT: swift_release
// AT-MOST-ONCE: call void @__swift_destroy_noncopyable_closure_context(ptr %{{.*}})
// AT-MOST-ONCE-NOT: swift_release
// AT-MOST-ONCE: ret void

// The context of a closure without captures is null.
// AT-MOST-ONCE-LABEL: define linkonce_odr hidden void @__swift_destroy_noncopyable_closure_context(ptr %0)
// AT-MOST-ONCE: [[IS_NULL:%.*]] = icmp eq ptr %0, null
// AT-MOST-ONCE: br i1 [[IS_NULL]], label %done, label %destroy
// AT-MOST-ONCE: destroy:
// AT-MOST-ONCE-noptrauth: [[METADATA:%.*]] = load ptr, ptr %0
// AT-MOST-ONCE-ptrauth: [[SIGNED_METADATA:%.*]] = load ptr, ptr %0
// AT-MOST-ONCE-ptrauth: [[OBJECT_INT:%.*]] = ptrtoint ptr %0 to i64
// AT-MOST-ONCE-ptrauth: [[METADATA_DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[OBJECT_INT]], i64 27361)
// AT-MOST-ONCE-ptrauth: [[SIGNED_METADATA_INT:%.*]] = ptrtoint ptr [[SIGNED_METADATA]] to i64
// AT-MOST-ONCE-ptrauth: [[METADATA_INT:%.*]] = call i64 @llvm.ptrauth.auth(i64 [[SIGNED_METADATA_INT]], i32 2, i64 [[METADATA_DISC]])
// AT-MOST-ONCE-ptrauth: [[METADATA:%.*]] = inttoptr i64 [[METADATA_INT]] to ptr
// AT-MOST-ONCE: [[SLOT:%.*]] = getelementptr inbounds ptr, ptr [[METADATA]], i64 1
// AT-MOST-ONCE: [[DESTRUCTOR:%.*]] = load ptr, ptr [[SLOT]]
// AT-MOST-ONCE-noptrauth: call swiftcc void [[DESTRUCTOR]](ptr swiftself %0)
// AT-MOST-ONCE-ptrauth: [[SLOT_INT:%.*]] = ptrtoint ptr [[SLOT]] to i64
// AT-MOST-ONCE-ptrauth: [[DISC:%.*]] = call i64 @llvm.ptrauth.blend(i64 [[SLOT_INT]], i64 48063)
// AT-MOST-ONCE-ptrauth: call swiftcc void [[DESTRUCTOR]](ptr swiftself %0) {{.*}}[ "ptrauth"(i32 0, i64 [[DISC]]) ]
// AT-MOST-ONCE: done:
// AT-MOST-ONCE-NEXT: ret void

// A single class reference capture still gets a context of its own, because
// the destructor of the class must not run when the closure is dropped.
// AT-MOST-ONCE-LABEL: define {{.*}}swiftcc void @"$e{{.*}}18singleClassCaptureyyAA7TrackerCF"(ptr %0)
// AT-MOST-ONCE: [[CONTEXT:%.*]] = call ptr @swift_initStackObject(
// AT-MOST-ONCE: call swiftcc void @"$e{{.*}}6dropIfyySb_yyXEntF"(i1 false, ptr @"$e{{.*}}18singleClassCaptureyyAA7TrackerCFyyXEfU_TA{{[^"]*}}", ptr [[CONTEXT]])
@inline(never)
public func singleClassCapture(_ t: Tracker) {
  dropIf(false) { _ = t }
}

#endif
