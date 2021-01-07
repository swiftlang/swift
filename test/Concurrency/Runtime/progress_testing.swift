// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

import Swift

import Darwin

struct ProgressBox {
  let callMe: (ProgressValue) -> ()

  func claim(file: String, line: UInt) {
    print("Progress box: claimed at \(file):\(line)")
  }
}

struct ProgressReporter {
  let callMe: () -> ()
  let total: Int
  init(parent: ProgressValue, callMe: () -> (), total: Int) {
    self.callMe = callme
    self.total = total
  }

  mutating func increment(by: Int = 1) {
    callMe()
  }
}

public struct ProgressValue {
  public var total: Int
  public var completed: Int
  public var fractionCompleted: Double { get }

  public enum Phase {
    case active
    case cancelled
    case finished
  }
  public var phase: Phase
}

extension TaskLocalValues {
  var progress: ProgressKey { .init() }
  enum ProgressKey: TaskLocalKey {
    static var defaultValue: ProgressValue? { nil }

    static var inherit: TaskLocalInheritance { .never }
  }
}

extension Task {

  func withProgressObserver<T>(
    _ onProgressUpdate: (ProgressValue) -> (),
    operation: () async throws -> T
  ) -> async rethrows T {
    let box = ProgressBox(callMe: onProgressUpdate)
    return try await Task.withLocal(\.progress, boundTo: box) {
      try await operation
    }
  }

  func withProgress(pending: Int) async {
    if let parentBox = await Task.local(\.progress) {

    }
  }

  func reportingProgress<T>(
    pending: Int,
    file: String = #file, line: UInt = #line,
    body: (inout ProgressReporter) -> T) async -> T {
    if let parentProgress = Task.local(\.progress) {
      parentProgress.claim(file: file, line: line)

      return await Task.withLocal(\.progress, boundTo: nil) {
        // unbind the progress!
        // As we're reporting things in this "leaf" task no other operation
        // may report things.
        var reporter = ProgressReporter(total: parentProgress)
        return body(&reporter)
      }
    } else {
      var reporter = ProgressReporter(total: 1) // or "noop"
      return body(&reporter)
    }
  }
}

func main() async {
  func makeDinner() async throws -> Meal {
    var progress = await Task.reportingProgress(pending: 5)

    async let veggies = progress.withPendingProgress(1) {
      await chopVegetables()
    }
    async let meat = marinateMeat()

    async let oven = progress.withPendingProgress(3) {
      await preheatOven(temperature: 350)
    }

    let dish = Dish(ingredients: await [veggies, meat])
    let dinner = await progress.withPendingProgress(1) {
      await oven.cook(dish, duration: .hours(3))
    }

    return dinner
  }

  func chopVegetables() async -> [String] {
    return []
  }

  func marinateMeat() async -> String {
    ""
  }

  func preheatOven(temperature: Int) -> String {
    ""
  }

  struct Dish {
    init(ingredients: [String]) {}
  }

}
