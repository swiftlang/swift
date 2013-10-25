// Part of the multi-file.swift test.

func ~~~(x: Int, y: Int) -> Bool { // expected-error{{operator implementation without matching operator declaration}}
  return x <= y
}

func test() {
  var a = funcOrVar // expected-error{{use of unresolved identifier 'funcOrVar'}}

  var s = SomeStruct(42) // use the SomeStruct from multi-file.swift

  var tilde: Bool = 1 ~~~ 2 // expected-error{{operator is not a known binary operator}}
}
