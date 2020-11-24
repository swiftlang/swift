//// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
//// REQUIRES: executable_test
//// REQUIRES: concurrency
//// REQUIRES: OS=macosx
//// REQUIRES: CPU=x86_64
//
//import Dispatch
//
//// ==== ------------------------------------------------------------------------
//// MARK: "Infrastructure" for the tests
//
//extension DispatchQueue {
//  func async<R>(priority: Task.Priority, operation: @escaping () async -> R) -> Task.Handle<R> {
//    let handle = Task.runDetached(priority: priority, operation: operation)
//
//    // Run the task
//    _ = { self.async { handle.run() } }() // force invoking the non-async version
//
//    return handle
//  }
//}
//
//// ==== ------------------------------------------------------------------------
//// MARK: Tests
//
//func test_currentPriority() {
//  DispatchQueue.main.async(priority: .default) {
//    let p1Name = await try! DispatchQueue.main.async(priority: .background) { () async in
//      await "\(Task.currentPriority())"
//    }.get()
//    // CHECK: priority: background
//    print("priority: \(p1Name)")
//    assert(p1Name == "background", "got: [\(p1Name)]")
//
//    let p2Name = await try! DispatchQueue.main.async(priority: .utility) { () async in
//      await "\(Task.currentPriority())"
//    }.get()
//    // CHECK: priority: utility
//    print("priority: \(p2Name)")
//    assert(p1Name == "utility", "got: [\(p2Name)]")
//
//    exit(0)
//  }
//}
//
//test_currentPriority()
//
//dispatchMain()
