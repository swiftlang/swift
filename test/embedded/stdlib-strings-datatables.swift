// Test String operations that require unicode data tables. This is not an executable test yet, because the data tables
// are not available for linking yet.

// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

public func test1() {
  let string = "string"
  let other = "other"
  let appended = string + other
  _ = appended

  let _ = "aa" == "bb"
  let dict: [String:Int] = [:]
  _ = dict

  let _ = "aaa".uppercased()

  let space: Character = " "
  let split = appended.split(separator: space)
  _ = split
}
