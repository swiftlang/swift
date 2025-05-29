enum Err : Error {
  case wat
}

func throwsSomething() throws { throw Err.wat }
func consumesErrClosure(_ fn: () throws -> Void) {}
func rethrowsErrClosure(_ fn: () throws -> Void) rethrows {}

func testThrowingClosure() throws {
  consumesErrClosure { throw Err.wat }
  consumesErrClosure { try throwsSomething() }
  try rethrowsErrClosure { try throwsSomething() }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=10:1 -end-pos=11:47 >> %t.result/consumes_err.swift
// RUN: diff -u %S/Outputs/throw_errors3/consumes_err.swift.expected %t.result/consumes_err.swift
// RUN: %refactor -extract-function -source-filename %s -pos=10:1 -end-pos=12:51 >> %t.result/rethrows_err.swift
// RUN: diff -u %S/Outputs/throw_errors3/rethrows_err.swift.expected %t.result/rethrows_err.swift
// REQUIRES: swift_swift_parser
