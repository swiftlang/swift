// Part of the multi-file.swift test.

import ambiguous_right

func test() {
  funcOrVar = 1
  var f: () -> () = funcOrVar // expected-error{{'Int' is not convertible to '() -> ()'}}

  var s = SomeStruct(42) // use the SomeStruct from multi-file.swift
}
