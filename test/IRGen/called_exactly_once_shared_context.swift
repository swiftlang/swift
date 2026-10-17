// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute -target %target-future-triple %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: PTRSIZE=64

// CHECK: @metadata{{.*}} = private constant %swift.full_boxmetadata { ptr {{.*}}@__swift_exactly_once_closure_destructor
// CHECK: @__swift_exactly_once_closure_context = internal global %swift.refcounted { ptr {{.*}}@metadata{{.*}}, i64 0 }
// CHECK: @llvm.global_ctors = {{.*}}i32 60000, ptr {{.*}}@__swift_exactly_once_closure_context_init

public struct Empty {}

// A closure with no captures doesn't allocate a context.
public func makeCaptureless() -> @called(exactlyOnce) () -> Void {
  return {}
}

// CHECK-LABEL: define{{.*}} swiftcc { ptr, ptr } @"$s4test15makeCapturelessyyXOyF"()
// CHECK-NOT: swift_allocObject
// CHECK-NOT: swift_retain
// CHECK: ret { ptr, ptr } { ptr {{.*}}@"{{.*}}", ptr @__swift_exactly_once_closure_context }

// CHECK-LABEL: define private void @__swift_exactly_once_closure_context_init()
// CHECK-NEXT: entry:
// CHECK-NEXT: call ptr @swift_initStackObject(ptr {{.*}}@metadata{{.*}}, ptr @__swift_exactly_once_closure_context)
// CHECK-NEXT: ret void

// A closure whose captures are all empty also doesn't allocate a context. Its
// forwarder doesn't release or deallocate the shared context.
public func makeEmptyCapture(_ empty: Empty) -> @called(exactlyOnce) () -> Void {
  return { _ = empty }
}

// CHECK-LABEL: define{{.*}} swiftcc { ptr, ptr } @"$s4test16makeEmptyCapture{{.*}}"()
// CHECK-NOT: swift_allocObject
// CHECK: ret { ptr, ptr } { ptr @"$s4test16makeEmptyCapture{{.*}}TA{{(\.ptrauth)?}}", ptr @__swift_exactly_once_closure_context }

// CHECK-LABEL: define internal swiftcc void @"$s4test16makeEmptyCapture{{.*}}TA"(ptr swiftself %0)
// CHECK-NOT: swift_release
// CHECK-NOT: swift_deallocUninitializedObject
// CHECK: ret void

// A stack-promoted closure with no captures uses the shared context too.
public func take(_ f: @called(exactlyOnce) () -> Void) { f() }

public func makeOnStack() {
  take {}
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test11makeOnStackyyF"()
// CHECK-NOT: swift_initStackObject
// CHECK: call swiftcc void @"$s4test4takeyyyyXEnF"(ptr {{.*}}@"{{.*}}", ptr @__swift_exactly_once_closure_context)
