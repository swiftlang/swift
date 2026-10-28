// RUN: %target-swift-frontend -primary-file %s -emit-irgen -target %target-swift-5.1-abi-triple | %FileCheck %s

// REQUIRES: concurrency
// UNSUPPORTED: CPU=wasm32

// rdar://187088035
// An async call to a generic callee with an indirectly-returned typed error
// that depends on the callee's generic parameters must pass the same error
// slot that the error edge of the try_apply loads from: the one allocated
// for the substituted error type.

public protocol Stage {
  associatedtype FailureReason
}

// Larger than the direct-return limit, so the typed error goes through the
// caller-allocated indirect error slot.
public struct StageFailure<S: Stage>: Error {
  public var reason: S.FailureReason
  public var a: String
  public var b: String
  public var c: String
}

public struct MyStage<Output>: Stage {
  public typealias FailureReason = String
  public init() {}

  public func run() async throws(StageFailure<Self>) {
    throw StageFailure(reason: "could not run", a: "a", b: "b", c: "c")
  }
}

// CHECK-LABEL: define {{.*}}swifttailcc void @"$s{{.*}}6callerSSyYaF"(
// CHECK-NOT:     alloca %T{{.*}}12StageFailureV,
// CHECK:         [[SLOT:%.*]] = alloca %T{{.*}}12StageFailureVy{{.*}}GG,
// CHECK-NOT:     alloca %T{{.*}}12StageFailureV,
// CHECK:         call {{.*}} @llvm.coro.suspend.async{{.*}}@"$s{{.*}}7MyStageV3runyyYa{{.*}}YKF", {{.*}}, ptr [[SLOT]])
// CHECK:         getelementptr inbounds{{.*}} %T{{.*}}12StageFailureVy{{.*}}GG, ptr [[SLOT]], i32 0, i32 0
// CHECK:       }
public func caller() async -> String {
  do {
    try await MyStage<Int>().run()
    return "no error"
  } catch {
    return error.reason
  }
}
