// RUN: %swift -parse -parse-as-library %s %S/Inputs/multi-file-2.swift -module-name=MultiFile -I=%S/Inputs -sdk= -verify

import ambiguous_left

struct SomeStruct {
  var value: Int
}

func test() {
  funcOrVar()
  var a: Int = funcOrVar // expected-error{{'() -> ()' is not convertible to 'Int'}}

  var s = SomeStruct(42) // use the local SomeStruct
}
