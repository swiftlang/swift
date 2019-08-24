func testStringLiteral() -> String {
  let name = "Jason"
  print("Hello, \(name)!")
  return "abc"
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -localize-string -source-filename %s -pos=4:12 > %t.result/L4.swift
// RUN: diff -u %S/Outputs/basic/L4.swift.expected %t.result/L4.swift
