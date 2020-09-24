// Verify that async parses correctly via the parser lib even without the
// experimental flag being set in LangOpts.
//
// REQUIRES: syntax_parser_lib
// RUN: %swift-syntax-parser-test %s -dump-diags 2>&1 | %FileCheck %s

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

// CHECK: 0 error(s) 0 warnings(s) 0 note(s)
