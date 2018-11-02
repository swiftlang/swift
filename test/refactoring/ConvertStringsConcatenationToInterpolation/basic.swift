func testStringConcatenation() {
  let firstName = "Jason"
  let bornYear = "1888"
  let _ = "Mr. " + firstName + bornYear
}
// RUN: %empty-directory(%t.result)
// RUN: %refactor -strings-concatenation-to-interpolation -source-filename %s -pos=4:11 -end-pos=4:40 > %t.result/L4.swift
// RUN: diff -u %S/Outputs/basic/L4.swift.expected %t.result/L4.swift
