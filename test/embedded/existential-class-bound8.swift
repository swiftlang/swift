// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// REQUIRES: swift_in_compiler
// REQUIRES: swift_feature_Embedded

// BEGIN MyModule.swift

public protocol Base: AnyObject {
    func bar()
}

public protocol ClassBound: Base {
    func foo()
}

class MyGenericClass<T> {
    var typ: String
    init(typ: String) { self.typ = typ }
}
extension MyGenericClass: ClassBound {
    func bar() { print("MyGenericClass<\(typ)>.bar()") }
    func foo() { print("MyGenericClass<\(typ)>.foo()") }
}

public func factory() -> any ClassBound {
    return MyGenericClass<String>(typ: "String")
}

// BEGIN Main.swift

import MyModule

var arr: [any ClassBound] = [factory()]
arr[0].foo()
// CHECK: MyGenericClass<String>.foo()
arr[0].foo()
// CHECK: MyGenericClass<String>.bar()
