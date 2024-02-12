func returnFifteen() -> Int {
  var y = 10
  func add() {
    y += 5
    if y > 50 {
      print("bigger than 50")
    } else {
      print("less")
    }
  }
  add()
  return y
}

// RUN: %empty-directory(%t.result)
// RUN: %refactor -extract-function -source-filename %s -pos=5:1 -end-pos=9:6 >> %t.result/AvoidFilePrivate.swift
// RUN: diff -u %S/Outputs/extract_local/AvoidFilePrivate.swift.expected %t.result/AvoidFilePrivate.swift
// REQUIRES: swift_swift_parser
