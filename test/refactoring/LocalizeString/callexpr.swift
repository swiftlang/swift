func testStringLiteral() -> String {
  return "abc".foo()
}

extension String {
  func foo() -> String { return self }
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -localize-string -source-filename %s -pos=2:10 > %t.result/L2.swift
// RUN: diff -u %S/Outputs/callexpr/L2.swift.expected %t.result/L2.swift
