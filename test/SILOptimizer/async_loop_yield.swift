// RUN: %target-swift-frontend -emit-sil -enable-async-loop-yield %s | %FileCheck %s --check-prefixes=CHECK,ENABLED
// RUN: %target-swift-frontend -emit-sil -disable-async-loop-yield %s | %FileCheck %s --check-prefixes=CHECK,DISABLED

// REQUIRES: concurrency

func compute() async -> Int { 1 }
func syncCompute() -> Int { 1 }

struct Counting: AsyncSequence {
  typealias Element = Int
  let n: Int
  struct AsyncIterator: AsyncIteratorProtocol {
    var i = 0
    let n: Int
    mutating func next() async -> Int? {
      guard i < n else { return nil }
      defer { i += 1 }
      return i
    }
  }
  func makeAsyncIterator() -> AsyncIterator { AsyncIterator(n: n) }
}

// CHECK-LABEL: sil hidden @$s{{.*}}7forLoop{{.*}}F :
// ENABLED:       builtin "yieldToCurrentExecutor"() : $()
// DISABLED-NOT:  yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}7forLoop{{.*}}F'
func forLoop(n: Int) async -> Int {
  var sum = 0
  for _ in 0..<n {
    sum += await compute()
  }
  return sum
}

// CHECK-LABEL: sil hidden @$s{{.*}}9whileLoop{{.*}}F :
// ENABLED:       builtin "yieldToCurrentExecutor"() : $()
// DISABLED-NOT:  yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}9whileLoop{{.*}}F'
func whileLoop(n: Int) async -> Int {
  var sum = 0
  while sum < n {
    sum += await compute()
  }
  return sum
}

// CHECK-LABEL: sil hidden @$s{{.*}}10repeatLoop{{.*}}F :
// ENABLED:       builtin "yieldToCurrentExecutor"() : $()
// DISABLED-NOT:  yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}10repeatLoop{{.*}}F'
func repeatLoop(n: Int) async -> Int {
  var sum = 0
  repeat {
    sum += await compute()
  } while sum < n
  return sum
}

// CHECK-LABEL: sil hidden @$s{{.*}}12forAwaitLoop{{.*}}F :
// ENABLED:       builtin "yieldToCurrentExecutor"() : $()
// DISABLED-NOT:  yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}12forAwaitLoop{{.*}}F'
func forAwaitLoop(n: Int) async -> Int {
  var sum = 0
  for await x in Counting(n: n) {
    sum += x
  }
  return sum
}

// A loop in an async function that never suspends is left alone.
//
// CHECK-LABEL: sil hidden @$s{{.*}}15noSuspendInLoop{{.*}}F :
// CHECK-NOT:     yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}15noSuspendInLoop{{.*}}F'
func noSuspendInLoop(n: Int) async -> Int {
  var sum = await compute()
  for _ in 0..<n {
    sum += syncCompute()
  }
  return sum
}

// An await inside a closure created in the loop is not a suspension of the
// loop itself.
//
// CHECK-LABEL: sil hidden @$s{{.*}}13closureInLoop{{.*}}F :
// CHECK-NOT:     yieldToCurrentExecutor
// CHECK:       } // end sil function '$s{{.*}}13closureInLoop{{.*}}F'
func closureInLoop(n: Int) async -> [() async -> Int] {
  var closures: [() async -> Int] = []
  for _ in 0..<n {
    closures.append { await compute() }
  }
  return closures
}
