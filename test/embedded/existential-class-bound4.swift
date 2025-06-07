// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -parse-as-library -wmo) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu
// REQUIRES: swift_feature_Embedded

public protocol Base: AnyObject {
    func foo()
}

protocol ClassBound: Base {
    func bar()
}

class MyGenericClass<T> {
    var typ: String
    init(typ: String) { self.typ = typ }
}
extension MyGenericClass: ClassBound {
    func foo() { print("MyGenericClass<\(typ)>.foo()") }
    func bar() { print("MyGenericClass<\(typ)>.bar()") }
}

@main
struct Main {
    static func main() {
        var array: [any ClassBound] = []
        array.append(MyGenericClass<Int>(typ: "Int"))
        array.append(MyGenericClass<String>(typ: "String"))

        for e in array {
            e.foo()
            e.bar()
        }

        // CHECK: MyGenericClass<Int>.foo()
        // CHECK: MyGenericClass<Int>.bar()

        // CHECK: MyGenericClass<String>.foo()
        // CHECK: MyGenericClass<String>.bar()
    }
}

