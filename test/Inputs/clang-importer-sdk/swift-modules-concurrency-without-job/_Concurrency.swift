
// This simulates a pre-Swift5.9 concurrency library with `Job` and `ExecutorJob` not being defined at all.
// This is used to verify missing type handling in the SDK when a latest compiler is used.
//
// This also looks exactly like the Back deployment library which also does not
// know about Job, and therefore is missing the Job based (and ExecutorJob based),
// protocol requirements. This helps confirm we don't crash assuming that those
// protocol requirements would always be there.

public struct UnownedJob {}

public protocol Executor {
  func enqueue(_ job: UnownedJob)
}
public protocol SerialExecutor: Executor { }
