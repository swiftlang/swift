// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

protocol ClassBound: AnyObject {
    func foo()
}

protocol OtherProtocol: AnyObject {
    func bar()
}

class MyClass: ClassBound, OtherProtocol {
    func foo() { print("MyClass.foo()") }
    func bar() { print("MyClass.bar()") }
}

func test(existential: any ClassBound & OtherProtocol) {
    existential.foo()
    existential.bar()
}

@main
struct Main {
    static func main() {
        test(existential: MyClass())
        // CHECK: MyClass.foo()
        // CHECK: MyClass.bar()
    }
}
