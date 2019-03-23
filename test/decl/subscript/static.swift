// RUN: %target-typecheck-verify-swift

// FIXME: Write proper tests (this file is mainly suitable for interactive testing)

struct MyStruct {
  static subscript(_ i: Int) -> String {
    get { return "get \(i)" }
    set { print("set \(i)") }
  }
  
  static var prop: Int = 0
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

class NSThingy {
  @objc class subscript(_ i: Int) -> AnyObject? { // expected-error{{class subscript cannot be marked @objc}}
    return nil
  }
}
let kp = \MyStruct.Type.prop
print(kp)
print(MyStruct.self[keyPath: kp])
print(MyStruct[keyPath: kp])

let dynKP = \Dyn.Type.foo
print(dynKP)
print(Dyn.self[keyPath: dynKP])
class Base {
  static subscript(_ i: Int) -> String { return "Base" }
}
class DerivedGood: Base {
  override static subscript(_ i: Int) -> String { return "DerivedGood" }
}

print(DerivedGood[0])
