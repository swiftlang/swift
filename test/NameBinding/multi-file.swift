// RUN: %target-swift-frontend -typecheck -parse-as-library -enable-source-import %s %S/Inputs/multi-file-2.swift %S/Inputs/multi-file-3.swift -module-name MultiFile -I %S/Inputs -sdk "" -verify

import ambiguous_left
import tilde_tilde_low_precedence

struct SomeStruct {
  var value: Int
}

func test() {
  funcOrVar()
  var _: Int = funcOrVar // expected-error{{cannot convert value of type '() -> ()' to specified type 'Int'}}

  _ = SomeStruct(value: 42) // use the local SomeStruct
  
  var _: Bool = 1 + 2 ~~ 3 + 4 // (1 + 2) ~~ (3 + 4)
}

func conformsToItself(x: inout P3, y: P3) {
  x = y
}

func testOverrides(obj: Sub) {
  obj.foo()
  obj.prop = 5
}
