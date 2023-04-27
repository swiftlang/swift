
// This simulates a pre-Swift5.9 concurrency library with `Job` and `ExecutorJob` not being defined at all.
// This is used to verify missing type handling in the SDK when a latest compiler is used.

public struct UnownedJob {}

public protocol SerialExecutor {
  // pretend to be a broken, empty serial executor
  func enqueue(_ job: UnownedJob)
}