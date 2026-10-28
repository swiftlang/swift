// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s -c -o %t/executor.o

// REQUIRES: swift_feature_Embedded

nonisolated(unsafe) var queue: UnsafeMutablePointer<SwiftJob>? = nil

func privateData(_ job: UnsafeMutablePointer<SwiftJob>) -> UnsafeMutableRawPointer? {
  let field: UnsafeMutableRawPointer? = job.pointee.schedulerPrivate.0
  let data: UnsafeMutablePointer<UnsafeMutableRawPointer?> = swift_job_getPrivateData(job)
  data[1] = nil
  return data[0] ?? field
}

func jobHelpers(_ job: UnsafeMutablePointer<SwiftJob>) -> Bool {
  let kind: SwiftJobKind = swift_job_getKindFromFlags(job)
  let priority: SwiftJobPriority = swift_job_getPriorityFromFlags(job)
  return kind == SwiftTaskJobKind && priority != SwiftUnspecifiedJobPriority
}

func executorHelpers(_ executor: SwiftExecutorRef) -> Bool {
  let identity: UnsafeMutablePointer<SwiftHeapObject>? =
    swift_executor_getIdentity(executor)
  let table: OpaquePointer? = swift_executor_getWitnessTable(executor)
  let ordinary = swift_executor_ordinary(identity, table)
  let complex = swift_executor_complexEquality(identity, table)
  let kind: SwiftExecutorKind = swift_executor_getKind(complex)
  return kind == SwiftExecutorComplexEqualityKind
    && swift_executor_hasWitnessTable(ordinary)
    && !swift_executor_isDefaultActor(ordinary)
}

@c(_swift_concurrency_enqueueGlobal)
func enqueueGlobal(_ job: UnsafeMutablePointer<SwiftJob>) {
  if jobHelpers(job) && privateData(job) == nil {
    queue = job
  }
}

@c(_swift_concurrency_enqueueMainExecutor)
func enqueueMainExecutor(_ job: UnsafeMutablePointer<SwiftJob>) {
  queue = job
}

@c(_swift_concurrency_getMainExecutor)
func getMainExecutor() -> SwiftExecutorRef {
  swift_executor_generic()
}

@c(_swift_concurrency_isMainExecutor)
func isMainExecutor(_ executor: SwiftExecutorRef) -> Bool {
  swift_executor_isGeneric(executor) || executorHelpers(executor)
}

@c(_swift_concurrency_asyncMainDrainQueue)
func asyncMainDrainQueue() -> Never {
  while true {
    if let job = queue {
      queue = nil
      swift_job_runSynchronously(job, swift_executor_generic())
    }
  }
}

@c(_swift_concurrency_donateThreadToGlobalExecutorUntil)
func donateThreadToGlobalExecutorUntil(
    _ condition: @convention(c) (UnsafeMutableRawPointer?) -> Bool,
    _ context: UnsafeMutableRawPointer?) {
  while !condition(context) {
    guard let job = queue else { return }
    queue = nil
    swift_job_runSynchronously(job, swift_executor_generic())
  }
}

// The platform header declares the main-executor functions.
let mainExecutorFunctions = (
  _swift_concurrency_enqueueMainExecutor,
  _swift_concurrency_getMainExecutor,
  _swift_concurrency_isMainExecutor)
