// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -O) | %FileCheck %s
// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo -Osize) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

protocol ClassBound: AnyObject {
    func foo()
    func bar()
}

class MyClass {}
extension MyClass: ClassBound {
    func foo() { print("MyClass.foo()") }
    func bar() { print("MyClass.bar()") }
}

class MyOtherClass {}
extension MyOtherClass: ClassBound {
    func foo() { print("MyOtherClass.foo()") }
    func bar() { print("MyOtherClass.bar()") }
}

func test(existential: any ClassBound) {
    existential.foo()
    existential.bar()
}

@main
struct Main {
    static func main() {
        test(existential: MyClass())
        // CHECK: MyClass.foo()
        // CHECK: MyClass.bar()
        test(existential: MyOtherClass())
        // CHECK: MyOtherClass.foo()
        // CHECK: MyOtherClass.bar()
    }
}

