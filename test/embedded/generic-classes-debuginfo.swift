// RUN: %target-run-simple-swift(-g -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s
// RUN: %target-run-simple-swift(-g -Osize -enable-experimental-feature Embedded -parse-as-library -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

struct User {
  let o: BaseClass
}

class BaseClass {
  func foo() {}
}

protocol Protocol {
  func protofoo()
}

class Implementor: Protocol {
  func protofoo() { print("Implementor.protofoo") }
}

class GenericSubClass<P: Protocol>: BaseClass {
  let p: P

  init(p: P) {
    self.p = p
  }

  override func foo() {
    let x = { self.p.protofoo() }
    x()
  }
}

@main
struct Main {
  static func main() {
    let p = Implementor()
    let o = GenericSubClass(p: p)
    let user = User(o: o)
    user.o.foo()
  }
}

// CHECK: Implementor.protofoo

