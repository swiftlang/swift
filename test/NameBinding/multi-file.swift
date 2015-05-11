// RUN: %target-swift-frontend -parse -parse-as-library -enable-source-import %s %S/Inputs/multi-file-2.swift %S/Inputs/multi-file-3.swift -module-name MultiFile -I %S/Inputs -sdk "" -verify

import ambiguous_left
import tilde_tilde_low_precedence

struct SomeStruct {
  var value: Int
}

func test() {
  funcOrVar()
  var _: Int = funcOrVar // expected-error{{'() -> ()' is not convertible to 'Int'}}

  var _ = SomeStruct(value: 42) // use the local SomeStruct
  
  var _: Bool = 1 + 2 ~~ 3 + 4 // (1 + 2) ~~ (3 + 4)
}

func conformsToItself(inout x: P3, y: P3) {
  x = y
}

func testOverrides(obj: Sub) {
  obj.foo()
  obj.prop = 5
}
