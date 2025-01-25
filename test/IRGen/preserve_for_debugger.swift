// RUN: %target-swiftc_driver %s -g -Onone -emit-ir | %FileCheck %s


// Check that unused globals are preserved at Onone.

private let number = 42
// CHECK: distinct !DIGlobalVariable(name: "number",

// Check that unused functions are preserved at Onone.
func unused() {
}
// CHECK: !DISubprogram(name: "unused", linkageName: "$s21preserve_for_debugger6unusedyyF"

// Property wrappers generate transparent getters, which we would like to check still exist at Onone.
@propertyWrapper
struct IntWrapper {
  private var storage = 42
    var wrappedValue: Int {
      return storage
    }
}

public class User {
  @IntWrapper private var number: Int

  func f() {
    // Force the generation of the getter
    _ = self.number 
  }
}
let c = User()
c.f()

// CHECK: !DISubprogram(name: "number.get"

protocol Foo {}

@propertyWrapper
struct Bar<ObjectType: Foo> {
    var storage: ObjectType

    public init(wrappedValue: ObjectType) {
      storage = wrappedValue
    }

    public var wrappedValue: ObjectType {
        return storage
    }

}

class Baz: Foo {
  let x = 42
}

struct Qux {
  @Bar(wrappedValue: Baz()) private var baz: Baz
  // Baz instance that is never accessed.
  @Bar(wrappedValue: Baz()) private var baz2: Baz

    func f() {
        print(self.baz) // break here
    }
}
let qux = Qux()
qux.f()

// CHECK: !DISubprogram(name: "baz.get"
// CHECK: !DISubprogram(name: "baz2.get"
