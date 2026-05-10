func longLongLongJourney() async -> Int { 0 }
func longLongLongAwryJourney() async throws -> Int { 0 }
func consumesAsync(_ fn: () async throws -> Void) rethrows {}

func testThrowingClosure() async throws -> Int {
  let x = await longLongLongJourney()
  let y = try await longLongLongAwryJourney() + 1
  try consumesAsync { try await longLongLongAwryJourney() }
  return x + y
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=6:11 -end-pos=6:38 >> %t.result/async1.swift
// RUN: diff -u %S/Outputs/await/async1.swift.expected %t.result/async1.swift
// RUN: %refactor -extract-function -source-filename %s -pos=7:11 -end-pos=7:50 >> %t.result/async2.swift
// RUN: diff -u %S/Outputs/await/async2.swift.expected %t.result/async2.swift
// RUN: %refactor -extract-function -source-filename %s -pos=8:1 -end-pos=8:60 >> %t.result/consumes_async.swift
// RUN: diff -u %S/Outputs/await/consumes_async.swift.expected %t.result/consumes_async.swift
// REQUIRES: swift_swift_parser
