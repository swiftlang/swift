// Part of the multi-file.swift test.

func ~~~(x: Int, y: Int) -> Bool { // expected-error{{operator implementation without matching operator declaration}}
  return x <= y
}

func test3() {
  var a = funcOrVar // expected-error{{cannot find 'funcOrVar' in scope}}

  var s = SomeStruct(value: 42) // use the SomeStruct from multi-file.swift

  // Reporting an error here isn't helpful - the source of the issue
  // is the operator implementation above missing an operator declaration,
  // which has already been diagnosed at this point.
  var tilde: Bool = 1 ~~~ 2

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
