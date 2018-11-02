func foo1() throws -> Int { return 0 }
enum MyError : Error {
  case E1
  case E2
}

func foo2() throws {
  try foo1()
  try! foo1()
  do {
    try foo1()
  } catch {}
  do {
    try foo1()
  } catch MyError.E1 {
  } catch MyError.E2 {
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=8:1 -end-pos=8:13 >> %t.result/L8-8.swift
// RUN: diff -u %S/Outputs/throw_errors/L8-8.swift.expected %t.result/L8-8.swift
// RUN: %refactor -extract-function -source-filename %s -pos=9:1 -end-pos=9:14 >> %t.result/L9-9.swift
// RUN: diff -u %S/Outputs/throw_errors/L9-9.swift.expected %t.result/L9-9.swift
// RUN: %refactor -extract-function -source-filename %s -pos=10:1 -end-pos=12:13 >> %t.result/L10-12.swift
// RUN: diff -u %S/Outputs/throw_errors/L10-12.swift.expected %t.result/L10-12.swift
// RUN: %refactor -extract-function -source-filename %s -pos=13:1 -end-pos=17:4 >> %t.result/L13-17.swift
// RUN: diff -u %S/Outputs/throw_errors/L13-17.swift.expected %t.result/L13-17.swift
