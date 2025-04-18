// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_feature_Embedded

protocol ClassBound: AnyObject {
  func foo()
}

protocol OtherProtocol {
  func bar()
}

class MyClass: ClassBound, OtherProtocol {
  func foo() { print("MyClass.foo()") }
  func bar() { print("MyClass.bar()") }
}

// Currently we don't support this
func test(existential: any ClassBound & OtherProtocol) {
}

@main
struct Main {
  static func main() {
    test(existential: MyClass()) // expected-error {{cannot use a value of protocol type 'any OtherProtocol' in embedded Swift}}
  }
}
