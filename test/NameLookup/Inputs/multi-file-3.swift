// Part of the multi-file.swift test.

func ~~~(x: Int, y: Int) -> Bool { // expected-error{{operator implementation without matching operator declaration}}
  return x <= y
}

func test3() {
  var a = funcOrVar // expected-error{{cannot find 'funcOrVar' in scope}}

  var s = SomeStruct(value: 42) // use the SomeStruct from multi-file.swift

  var tilde: Bool = 1 ~~~ 2 // expected-error{{operator is not a known binary operator}}

  var di = DefaultInit()
}

protocol P3 {
  func foo()
}

class Superclass {
  func method() { }
}

class Subclass : Superclass {
  override func method() { }
}
