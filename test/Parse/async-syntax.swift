// RUN: %target-typecheck-verify-swift -enable-experimental-concurrency -verify-syntax-tree

func asyncGlobal1() async { }
func asyncGlobal2() async throws { }

typealias AsyncFunc1 = () async -> ()
typealias AsyncFunc2 = () async throws -> ()

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
