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
  let _ = __await asyncGlobal1()
}
