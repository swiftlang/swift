// RUN: %target-typecheck-verify-swift

struct MyStruct {
  static subscript(_ i: Int) -> String {
    get { return "get \(i)" }
    set { print("set \(i) = \(newValue)") }
  }
}

func useMyStruct() {
  print(MyStruct.self[0])
  print(MyStruct[0])

  MyStruct.self[1] = "zyzyx"
  MyStruct[2] = "asdfg"
  
  MyStruct()[0] // expected-error {{static member 'subscript' cannot be used on instance of type 'MyStruct'}}
  MyStruct()[1] = "zyzyx" // expected-error {{static member 'subscript' cannot be used on instance of type 'MyStruct'}}
}

struct BadStruct {
  static subscript(_ i: Int) -> String {
    nonmutating get { fatalError() } // expected-error{{static functions must not be declared mutating}}
    mutating set { fatalError() } // expected-error{{static functions must not be declared mutating}}
  }
}

@dynamicMemberLookup
class Dyn {
  static subscript(dynamicMember name: String) -> String {
    return "Dyn.\(name)"
  }
}

func useDyn() {
  _ = Dyn.foo
  _ = Dyn().bar // expected-error{{static member 'bar' cannot be used on instance of type 'Dyn'}}
}

class BadBase {
  static subscript(_ i: Int) -> String { return "Base" } // expected-note{{overridden declaration is here}}
}
class BadDerived: BadBase {
  override static subscript(_ i: Int) -> String { return "DerivedGood" } // expected-error{{cannot override static subscript}}
}
