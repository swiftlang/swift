// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

// RUN: %target-swift-frontend -emit-module -o %t/MyModule.swiftmodule %t/MyModule.swift -enable-experimental-feature Embedded -parse-as-library
// RUN: %target-swift-frontend -c -I %t %t/Main.swift -enable-experimental-feature Embedded -o %t/a.o

// REQUIRES: swift_in_compiler
// REQUIRES: OS=macosx || OS=linux-gnu

// BEGIN MyModule.swift

public protocol ClassBound: AnyObject {
    func foo()
}

class MyGenericClass<T> {
    var typ: String
    init(typ: String) { self.typ = typ }
}
extension MyGenericClass: ClassBound {
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
