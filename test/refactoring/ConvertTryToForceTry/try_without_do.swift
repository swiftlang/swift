func testTryToForceTry() {
  func throwingFunc() throws -> Int { return 3 }
  let _ = try throwingFunc()
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -convert-to-force-try -source-filename %s -pos=3:12 > %t.result/L3.swift
// RUN: diff -u %S/Outputs/try_without_do/L3.swift.expected %t.result/L3.swift
