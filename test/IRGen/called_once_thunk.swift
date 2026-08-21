// RUN: %target-swift-emit-ir -module-name test -enable-experimental-feature CalledAttribute %s | %FileCheck %s

// REQUIRES: swift_feature_CalledAttribute
// REQUIRES: PTRSIZE=64

public func identity<T>(_ f: @escaping (T) -> Void) -> (T) -> Void { f }

public func makeCalledOnce(_ f: @escaping (Int) -> Void) -> @called(once) (Int) -> Void {
  return identity(f)
}

public func callOnceThroughThunk(_ f: @called(once) (Int) -> ()) { f(1) }
public func dontCallOnceThroughThunk(_ f: @called(once) (Int) -> ()) { /* never called */ }

final class Tracker {
  deinit { print("Tracker") }
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test33calledThroughThunkReleasesCaptureyyF"()
// CHECK: [[TRACKER:%.*]] = call swiftcc ptr @"$s4test7TrackerCACycfC"
// CHECK: call ptr @swift_retain(ptr returned [[TRACKER]])
// CHECK: [[CLOSURE:%.*]] = call swiftcc { ptr, ptr } @"$s4test14makeCalledOnceyySiXOySicF"(ptr @"$s4test33calledThroughThunkReleasesCaptureyyFySicfU_TA{{.*}}", ptr [[TRACKER]])
// CHECK: [[CLOSURE_FN:%.*]] = extractvalue { ptr, ptr } [[CLOSURE]], 0
// CHECK: [[CLOSURE_CTX:%.*]] = extractvalue { ptr, ptr } [[CLOSURE]], 1
// CHECK: call void @swift_release(ptr [[TRACKER]])
// CHECK: call swiftcc void @"$s4test20callOnceThroughThunkyyySiXEnF"(ptr [[CLOSURE_FN]], ptr [[CLOSURE_CTX]])
// CHECK: call void @swift_release(ptr [[TRACKER]])
// CHECK: ret void
public func calledThroughThunkReleasesCapture() {
  let t = Tracker()
  callOnceThroughThunk(makeCalledOnce { _ in _ = t })
}

// CHECK-LABEL: define{{.*}} swiftcc void @"$s4test38neverCalledThroughThunkReleasesCaptureyyF"()
// CHECK: [[TRACKER:%.*]] = call swiftcc ptr @"$s4test7TrackerCACycfC"
// CHECK: call ptr @swift_retain(ptr returned [[TRACKER]])
// CHECK: [[CLOSURE:%.*]] = call swiftcc { ptr, ptr } @"$s4test14makeCalledOnceyySiXOySicF"(ptr @"$s4test38neverCalledThroughThunkReleasesCaptureyyFySicfU_TA{{.*}}", ptr [[TRACKER]])
// CHECK: [[CLOSURE_FN:%.*]] = extractvalue { ptr, ptr } [[CLOSURE]], 0
// CHECK: [[CLOSURE_CTX:%.*]] = extractvalue { ptr, ptr } [[CLOSURE]], 1
// CHECK: call void @swift_release(ptr [[TRACKER]])
// CHECK: call swiftcc void @"$s4test24dontCallOnceThroughThunkyyySiXEnF"(ptr [[CLOSURE_FN]], ptr [[CLOSURE_CTX]])
// CHECK: call void @swift_release(ptr [[TRACKER]])
// CHECK: ret void
public func neverCalledThroughThunkReleasesCapture() {
  let t = Tracker()
  dontCallOnceThroughThunk(makeCalledOnce { _ in _ = t })
}
