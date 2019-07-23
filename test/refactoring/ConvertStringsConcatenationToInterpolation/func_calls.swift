func testStringConcatenation() {
  let firstName = "Jason"
  let number = 3
  let closure: () -> String = { return "FOO" }
  let _ = "Mr. " + firstName.debugDescription + closure() + "number: \(number)"
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -strings-concatenation-to-interpolation -source-filename %s -pos=5:11 -end-pos=5:80 > %t.result/L5.swift
// RUN: diff -u %S/Outputs/func_calls/L5.swift.expected %t.result/L5.swift
