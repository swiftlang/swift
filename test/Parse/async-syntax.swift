// RUN: %target-typecheck-verify-swift  -disable-availability-checking -verify-syntax-tree

// REQUIRES: concurrency

func asyncGlobal1() async { }
func asyncGlobal2() async throws { }

typealias AsyncFunc1 = () async -> ()
typealias AsyncFunc2 = () async throws -> ()
typealias AsyncFunc3 = (_ a: Bool, _ b: Bool) async throws -> ()

func testTypeExprs() {
  let _ = [() async -> ()]()
  let _ = [() async throws -> ()]()
}

func testAwaitOperator() async {
  let _ = await asyncGlobal1()
}

func testAsyncClosure() {
  let _ = { () async in 5 }
  let _ = { () throws in 5 }
  let _ = { () async throws in 5 }
}

func testAwait() async {
  let _ = await asyncGlobal1()
}
