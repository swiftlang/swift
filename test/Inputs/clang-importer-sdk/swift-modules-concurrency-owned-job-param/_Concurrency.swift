
// This simulates a pre-Swift5.9 concurrency library with an __owned declaration rather than consuming.
// This allows us to confirm the compiler and sdk can be mismatched but we'll build correctly.

public struct ExecutorJob: ~Copyable {}

public protocol SerialExecutor {
  // pretend old SDK with `__owned` param rather than ``
  func enqueue(_ job: __owned ExecutorJob)
}
