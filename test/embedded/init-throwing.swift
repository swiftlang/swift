// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public struct FooError: Error {}

public class PrintingClass {
    init() { print("PrintingClass.init") }
    deinit { print("PrintingClass.deinit") }
}

public class Foo {
    var a: PrintingClass
    var b: PrintingClass
    init(shouldThrow: Bool) throws(FooError) {
        a = PrintingClass()
        if shouldThrow { throw FooError() }
        b = PrintingClass()
    }
}

public class Bar: Foo {
  var value: Int = 17
}

public class Wibble: Bar {
  var c: PrintingClass = .init()
}

_ = try? Wibble(shouldThrow: true)
print("OK 1")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: PrintingClass.deinit
// CHECK: OK 1

_ = try? Wibble(shouldThrow: false)
print("OK 2")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.init
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: PrintingClass.deinit
// CHECK: PrintingClass.deinit
// CHECK: OK 2

public class Base {
    var baseMember: PrintingClass
    init(shouldThrow: Bool) throws(FooError) {
        if shouldThrow { throw FooError() }
        baseMember = PrintingClass()
    }
}

class SubClass: Base {
    var subClassMember: PrintingClass = PrintingClass()
    override init(shouldThrow: Bool) throws(FooError) {
        try super.init(shouldThrow: shouldThrow)
    }
}

_ = try? SubClass(shouldThrow: true)
print("OK 3")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: OK 3

_ = try? SubClass(shouldThrow: false)
print("OK 4")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: PrintingClass.deinit
// CHECK: OK 4
