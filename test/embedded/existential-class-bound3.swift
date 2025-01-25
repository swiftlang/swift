// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

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

@main
struct Main {
    static func main() {
        var array: [any ClassBound] = []
        array.append(MyClass())
        array.append(MyOtherClass())

        for e in array {
            e.foo()
            e.bar()
        }

        // CHECK: MyClass.foo()
        // CHECK: MyClass.bar()

        // CHECK: MyOtherClass.foo()
        // CHECK: MyOtherClass.bar()
    }
}

