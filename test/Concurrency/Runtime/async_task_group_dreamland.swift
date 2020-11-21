//// RUN: %target-run-simple-swift(-Xfrontend -enable-experimental-concurrency) | %FileCheck %s --dump-input always
//// REQUIRES: executable_test
//// REQUIRES: concurrency
//// REQUIRES: OS=macosx
//
//import Dispatch
//
///// Reliably and quickly perform work.
//func work(_ n: Int) async -> Int { n }
//
///// Pretend that the work takes the specified amount of time.
//func slowWork(_ n: Int, seconds: Int) async -> Int { n }
//
///// Pretend that sometimes the work takes a long time.
//func randomlySlowWork(_ n: Int) async -> Int { n }
//
///// Throw error.
//func boom() async -> Int { throw Boom() }
//
//
//// ==== ----------------------------------------------------------------------------------------------------------------
//// MARK: Select (exactly 1, cancel the rest)
//
//let first: Int = await Task.Group<Int>.select { group in
//    await group.add { await work() }
//    await group.add { await sleep(.seconds(3)) }
//}
//
//// ==== ----------------------------------------------------------------------------------------------------------------
//// MARK: Collect (first n elements, cancel the rest)
//// Collect works in submission order, the results are returned in the order they are submitted,
//// NOT in the order they are completed.
//
//let collected: [Int] = await Task.Group<Int>.collectAll { group in
//  await group.add { await randomlySlowWork() }
//  await group.add { await randomlySlowWork() }
//  await group.add { await randomlySlowWork() }
//}
//
//// CHECK: [1, 2, 3]
//// Order is guaranteed, regardless of timing
//print("\(collected)")
//
//// ==== ----------------------------------------------------------------------------------------------------------------
//// MARK: Scatter & Gather (keep collecting values until we get 10 elements, errors dropped)
//// Gather's notion of "first" is by *completion order*.
//
//let gathered: [Int] = Task.Group<Int>.gather(first: 2) { group in
//  await group.add { await randomlySlowWork() }
//  await group.add { await randomlySlowWork() }
//  await group.add { await randomlySlowWork() }
//}
//
//// CHECK: [1, 2, 3]
//// Order is NOT guaranteed, the values are collected as they complete
//print("\(Set(gathered))")
//
//// ==== ----------------------------------------------------------------------------------------------------------------
//// MARK: Task.Group.Lifetime, which can be used as a field
//
//actor class Worker {
//  let lifecycle: Task.Group<Int>.Lifetime = .create()
//
//  func performWork(w: Int) {
//      await lifecycle.add { await slowWork() }
//  }
//
//  func cancel() {
//    lifecycle.cancelAll()
//  }
//
//  deinit {
//    lifecycle.cancelAll()
//  }
//}
