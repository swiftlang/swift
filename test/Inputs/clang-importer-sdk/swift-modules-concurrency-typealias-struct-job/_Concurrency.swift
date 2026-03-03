
public struct ExecutorJob: ~Copyable {}

public typealias Job = ExecutorJob

public protocol SerialExecutor {
  func enqueue(_ job: __owned ExecutorJob)
}
