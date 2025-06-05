// RUN: %target-run-simple-swift(                            -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s
// RUN: %target-run-simple-swift(-Osize -Xlinker -dead_strip -enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xlinker %swift_obj_root/lib/swift/embedded/%target-cpu-apple-macos/libswiftUnicodeDataTables.a) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public func test1() {
  let string = "string"
  let other = "other"
  let appended = string + other
  print(appended) // CHECK: stringother

  let _ = "aa" == "bb"
  var dict: [String:Int] = [:]
  dict["aa"] = 42
  print(dict["aa"]!) // CHECK: 42

  let u = "aaa".uppercased()
  print(u) // CHECK: AAA

  let space: Character = " "
  let split = appended.split(separator: space)
  print(split[0]) // CHECK: stringother
}

test1()
