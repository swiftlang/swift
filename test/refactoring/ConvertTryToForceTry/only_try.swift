func testTryToForceTry() {
  func throwingFunc() throws -> Int { return 3 }
  do {
    let _ = try throwingFunc()
  } catch {
    let _ = error
  }
}
// RUN: rm -rf %t.result && mkdir -p %t.result
// RUN: %refactor -convert-to-force-try -source-filename %s -pos=4:14 > %t.result/L3.swift
// RUN: diff -u %S/Outputs/only_try/L3.swift.expected %t.result/L3.swift
