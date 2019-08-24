enum MyError : Error {
  case customError
}

struct RefactorExtractProblem {
  func testExtract() {
    do {
      throw MyError.customError
    } catch {
      print(error)
    }
  }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=7:1 -end-pos=11:6 >> %t.result/L7-11.swift
// RUN: diff -u %S/Outputs/throw_errors2/L7-11.swift.expected %t.result/L7-11.swift
