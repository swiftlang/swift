// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: swift_in_compiler
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

protocol ClassBound: AnyObject {
    func foo()
}

protocol NotClassBound {
    func foo()
}

class MyClass {}
extension MyClass: ClassBound, NotClassBound {
    func foo() { print("MyClass.foo()") }
}

func test(existential: any ClassBound) {
    existential.foo() // ok
}

func test(existential: any NotClassBound) {
    existential.foo() // expected-error {{cannot use a value of protocol type 'any NotClassBound' in embedded Swift}}
}

@main
struct Main {
    static func main() {
        test(existential: MyClass() as (any ClassBound)) // ok
        test(existential: MyClass() as (any NotClassBound)) // expected-error {{cannot use a value of protocol type 'any NotClassBound' in embedded Swift}}
    }
}
