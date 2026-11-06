// RUN: %empty-directory(%t)
// RUN: %target-typecheck-verify-swift -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform+Concurrency.h
// RUN: %target-swift-frontend -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform+Concurrency.h %s -c -o %t/executor.o
// RUN: not %target-swift-frontend -typecheck -D NOT_IMPORTED -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform+Concurrency.h %s 2>&1 | %FileCheck %s --check-prefix=NOT-IMPORTED
// RUN: not %target-swift-frontend -typecheck -enable-experimental-feature Embedded -parse-as-library -import-bridging-header %S/../../stdlib/public/EmbeddedPlatform/swift/EmbeddedPlatform.h %s 2>&1 | %FileCheck %s --check-prefix=PLATFORM-ONLY

// REQUIRES: swift_feature_Embedded

// NOT-IMPORTED: error: cannot find '_swift_job_run_c' in scope
// PLATFORM-ONLY: error: cannot find type 'SwiftJob' in scope

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
  return kind == .task && priority != .unspecified
}

func executorHelpers(_ executor: SwiftExecutorRef) -> Bool {
  let identity: UnsafeMutablePointer<SwiftHeapObject>? =
    swift_executor_getIdentity(executor)
  let table: OpaquePointer? = swift_executor_getWitnessTable(executor)
  let ordinary = swift_executor_ordinary(identity, table)
  let complex = swift_executor_complexEquality(identity, table)
  let kind: SwiftExecutorKind = swift_executor_getKind(complex)
  return kind == .complexEquality
    && swift_executor_getKind(ordinary) == .ordinary
    && swift_executor_hasWitnessTable(ordinary)
    && !swift_executor_isDefaultActor(ordinary)
}

func priorityRank(_ priority: SwiftJobPriority) -> Int {
  switch priority {
  case .userInteractive: 5
  case .userInitiated: 4
  case .default: 3
  case .utility: 2
  case .background: 1
  case .unspecified: 0
  @unknown default: -1
  }
}

func isReserved(_ kind: SwiftJobKind) -> Bool {
  kind.rawValue >= SwiftJobKind.firstReserved.rawValue
}

#if NOT_IMPORTED
func runJob(_ job: UnsafeMutablePointer<SwiftJob>) {
  _swift_job_run_c(job, swift_executor_generic())
}
#endif

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

@c(_swift_concurrency_asyncMain)
func asyncMain() -> Never {
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
