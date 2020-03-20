// RUN: %target-typecheck-verify-swift

enum Time {
  case milliseconds(Int)
}

struct Scheduled<T> {
  let futureResult: EventLoopFuture<T>
}

struct EventLoopFuture<T> {
  func flatMap<V>(_ body: @escaping (T) -> EventLoopFuture<V>) -> EventLoopFuture<V> {
    fatalError()
  }
}

struct EventLoop {
  func makeSucceededFuture<T>(_ v: T) -> EventLoopFuture<T> {
    fatalError()
  }

  func scheduleTask<T>(in t: Time, _ body: @escaping () -> T) -> Scheduled<T> {
    fatalError()
  }
}

struct Thing {}

func bar(eventLoop: EventLoop, process: @escaping (Thing) -> EventLoopFuture<Void>) -> EventLoopFuture<Void> {
  func doIt(thingsLeft: ArraySlice<Thing>) -> EventLoopFuture<Void> {
    var thingsLeft = thingsLeft
    guard let first = thingsLeft.popFirst() else {
      return eventLoop.makeSucceededFuture(())
    }

    return process(first).flatMap { [thingsLeft] in
      return eventLoop.scheduleTask(in: .milliseconds(100)) {
        return doIt(thingsLeft: thingsLeft)
        // expected-error@-1 {{cannot convert value of type 'EventLoopFuture<Void>' to closure result type 'Void'}}
      }.futureResult
    }
  }

  return doIt(thingsLeft: [][...])
}
