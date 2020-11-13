// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency
// REQUIRES: concurrency

import Foundation

extension Task.LocalValues {

  // TODO: can't get away with static var...
  // TODO: can't get with just a phantom type?
  public var example: ExampleKey { .init() }

  public struct ExampleKey: TaskLocalKey {
    public typealias Value = String
  }
}

func async() async {
  let thing = await Task.local(\.example)
  _ = thing
}

// ==== ------------------------------------------------------------------------

struct APIOnTaskStyle {
  func foo() async {
    await Task.withTaskLocal(\.example, boundTo: "A") {
      await bar(expected: "A")
      await baz(expectedInBar: "B")
      await bar(expected: "A")
    }
  }

  func bar(expected: String) async {
    let got = await Task.local(\.example)
    assert(got == expected)
  }

  func baz(expectedInBar: String) async {
    await Task.withTaskLocal(\.example, boundTo: "B") {
      let got = await Task.local(\.example)
      assert(got == "B")
      await bar(expected: expectedInBar)
    }
  }
}

// ==== ------------------------------------------------------------------------

struct VariableStyle {

  let exampleValue = TaskLocalValue<String>()

  // or with statically known default value, rather than defaulting to `nil`
  // let exampleValue: TaskLocalValue<String>.Defaulted =
  //   TaskLocalValue<String>.withDefault("not-set")

  func foo() async {
    await exampleValue.boundTo("A") {
      await bar(expected: "A")
      await baz(expectedInBar: "B")
      await bar(expected: "A")
    }
  }

  func bar(expected: String) async {
    let got = await exampleValue.get()
    assert(got == expected)
  }

  func baz(expectedInBar: String) async {
    await exampleValue.boundTo("B") {
      let got = await exampleValue.get()
      assert(got == "B")
      await bar(expected: expectedInBar)
    }
  }
}

// ==== ----------------------------------------------------------------------------------------------------------------
// ==== ----------------------------------------------------------------------------------------------------------------
// ==== ----------------------------------------------------------------------------------------------------------------
// MARK: Progress - exploring what APIs might look like

// FIXME: Remove all this, just experimenting IF or how things could be used to store and report progress as well

let exampleProgress = TaskLocalValue<Progress>()

class ExampleProgress {
  let worker = Worker()

  func run() async throws {
    let overallProgress = Progress(totalUnitCount: 100)

    await exampleProgress.boundTo(overallProgress) {
      async let result1 = await overallProgress.childTask(pendingUnitCount: 50) {
         await work()
      } // once this returns, definitely 50 units have progressed

      async let result2 = await overallProgress.childTask(pendingUnitCount: 50) {
        // do some work in different actor
        await worker.work()
      }  // once this returns, definitely the remaining 50 units have progressed

      _ = await result1
      _ = await result2
    }
  }

  // @TaskProgress(totalUnitCount: 10)
  func work() async -> Int {
    await exampleProgress.get()?.completedUnitCount = 10

    return 42
  }

}

actor class Worker {
  // @TaskProgress(totalUnitCount: 20)
  func work() async -> Int {
    await exampleProgress.get()?.completedUnitCount = 20

    return 42
  }
}

extension Progress {

  // See: https://developer.apple.com/documentation/foundation/progress#1661050

  // TODO: we can't really build this... can we?
  //       we never have real access to child tasks, or any tasks at all.
  //       we'd want to say "this is a child task, set a new progress value for the scope"
  //
  // This API mirrors the `addChild(_:withPendingUnitCount:)` explicit API for the Task oriented world.
  //
  // > As of iOS 9.0 and OS X v10.11 you can explicitly add a child to a progress tree.
  // >
  // > To add a child, call addChild(_:withPendingUnitCount:) on the parent.
  // > The value for pending unit count is the amount of the parent’s totalUnitCount
  // > consumed by the child. The child usually follows the ProgressReporting protocol.
  func childTask<BodyResult>(
    pendingUnitCount: Int,
    body: () async throws -> BodyResult
  ) async rethrows -> BodyResult {
    fatalError("\(#function) not implemented")
  }
}

// ==== ----------------------------------------------------------------------------------------------------------------
// MARK: Progress - How it looks with thread locals today.
class Sync_ThreadLocals {

  func run() throws {
    let overallProgress = Progress(totalUnitCount: 100)
    print("\(#file):\(#line) > overall: \(overallProgress.completedUnitCount) / \(overallProgress.totalUnitCount)")

    // Set the overallProgress as the current progress object of the current thread and specifies
    // the portion of work to be performed by the next child progress object of the receiver.
    overallProgress.becomeCurrent(withPendingUnitCount: 50)
    print("overall", overallProgress)
    work1()
    print("overall", overallProgress)
    // Balance the most recent previous invocation of becomeCurrent(withPendingUnitCount:)
    // on the same thread by restoring the current progress object to what it was before
    // becomeCurrent(withPendingUnitCount:) was invoked.
    overallProgress.resignCurrent()
    print("overall", overallProgress)


    overallProgress.becomeCurrent(withPendingUnitCount: 50)
    print("overall", overallProgress)
    work2()
    print("overall", overallProgress)
    overallProgress.resignCurrent()
    print("overall", overallProgress)
  }

  func work1() {
    let firstTaskProgress = Progress(totalUnitCount: 10) // attaches to 'current'
    print(#function, firstTaskProgress)

    DispatchQueue.global().async {
      // ...
    }

    firstTaskProgress.completedUnitCount = 5
    print(#function, firstTaskProgress)

    firstTaskProgress.completedUnitCount += 5
    print(#function, firstTaskProgress)
  }

  func work2() {
    let secondTaskProgress = Progress(totalUnitCount: 10) // attaches to 'current'
    print(#function, secondTaskProgress)

    DispatchQueue.global().async {
      // ...
    }

    secondTaskProgress.completedUnitCount = 5
    print(#function, secondTaskProgress)

    secondTaskProgress.completedUnitCount += 5
    print(#function, secondTaskProgress)
  }
}

// ==== ----------------------------------------------------------------------------------------------------------------
// MARK: Progress - How it looks with explicit passing today

class Sync_Explicit {

  func run() throws {
    let overallProgress = Progress(totalUnitCount: 100)
    print("overall", overallProgress)

    work1(overallProgress: overallProgress)
    print("overall", overallProgress)

    work2(overallProgress: overallProgress)
    print("overall", overallProgress)
  }

  func work1(overallProgress: Progress) {
    if #available(macOS 10.11, *) {
      let firstTaskProgress = Progress(totalUnitCount: 10)
      overallProgress.addChild(firstTaskProgress, withPendingUnitCount: 50)

      DispatchQueue.global().async {
        // ...
      }

      firstTaskProgress.completedUnitCount = 5
      print(#function, firstTaskProgress)
      print("overall", overallProgress)

      firstTaskProgress.completedUnitCount += 5
      print(#function, firstTaskProgress)
      print("overall", overallProgress)
    }
  }

  func work2(overallProgress: Progress) {
    if #available(macOS 10.11, *) {
      let secondTaskProgress = Progress(totalUnitCount: 10) // attaches to 'current'
      overallProgress.addChild(secondTaskProgress, withPendingUnitCount: 50)

      DispatchQueue.global().async {
        // ...
      }

      secondTaskProgress.completedUnitCount = 5
      print(#function, secondTaskProgress)
      print("overall", overallProgress)

      secondTaskProgress.completedUnitCount += 5
      print(#function, secondTaskProgress)
      print("overall", overallProgress)
    }
  }

}

