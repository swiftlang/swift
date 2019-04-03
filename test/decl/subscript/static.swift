// RUN: %target-typecheck-verify-swift

// FIXME: Write proper tests (this file is mainly suitable for interactive testing)

struct MyStruct {
  static subscript(_ i: Int) -> String {
    get { return "get \(i)" }
    set { print("set \(i)") }
  }
}

print(MyStruct.self[0])
print(MyStruct[0])

MyStruct.self[1] = "zyzyx"
MyStruct[2] = "asdfg"

@dynamicMemberLookup
class Dyn {
  static subscript(dynamicMember name: String) -> String {
    return "Dyn.\(name)"
  }
}

print(Dyn.foo)
print(Dyn.bar)

class BadBase {
  static subscript(_ i: Int) -> String { return "Base" } // expected-note{{overridden declaration is here}}
}
class BadDerived: BadBase {
  override static subscript(_ i: Int) -> String { return "DerivedGood" } // expected-error{{cannot override static subscript}}
}
