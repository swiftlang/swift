// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -enable-strings -verify
// RUN: %target-swift-frontend -emit-ir %s -enable-experimental-feature Embedded -enable-strings-full-unicode-data-tables

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

public func test1() {
  print("x")
  let x = "string"
  print(x)
  let _ = "abc \(42)"
  func takesString(s: String) { }
  takesString(s: "hi")
  let _ = "abc" + "def"
  for _ in "abc" + "def" { }
  print("abc" + "def")
}

public func test2(s1: String, s2: String) {
  let _ = s1 == s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 != s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 < s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 <= s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 > s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 >= s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}

  var hasher = Hasher()
  let _ = s1.hash(into: &hasher) // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1.hashValue // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  var d: [String:Int] // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  d = ["abc": 42] // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  d["def"] = 52 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  print(d["def"]!) // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}

  var s: Set<String> // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  s = .init() // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  s.insert("abc") // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}

  let _ = s1.split(separator: "\n") // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1.firstIndex(of: "\n") // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
}

public func test3(s1: Character, s2: Character) {
  let _ = s1 == s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 != s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 < s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 <= s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 > s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}
  let _ = s1 >= s2 // expected-error {{This operation on String needs Unicode data tables; pass -enable-string=full-unicode-data-tables to acknowledge the associated codesize cost}}  
}

@_unavailableInEmbedded
public func test4(s1: String, s2: String) {
  let _ = s1 == s2
  let _ = s1 != s2
  let _ = s1 < s2
  let _ = s1 <= s2
  let _ = s1 > s2
  let _ = s1 >= s2

  var hasher = Hasher()
  let _ = s1.hash(into: &hasher)
  let _ = s1.hashValue
  var d: [String:Int]
  d = ["abc": 42]
  d["def"] = 52
  print(d["def"]!)

  var s: Set<String>
  s = .init()
  s.insert("abc")

  let _ = s1.split(separator: "\n")
}
