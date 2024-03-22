// RUN: %target-run-simple-swift(-enable-experimental-feature Embedded -runtime-compatibility-version none -wmo -Xfrontend -disable-objc-interop) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: OS=macosx || OS=linux-gnu

struct FooError: Error {}

class PrintingClass {
    init() { print("PrintingClass.init") }
    deinit { print("PrintingClass.deinit") }
}

class Foo {
    var a: PrintingClass
    var b: PrintingClass
    init(shouldThrow: Bool) throws(FooError) {
        a = PrintingClass()
        if shouldThrow { throw FooError() }
        b = PrintingClass()
    }
}

_ = try? Foo(shouldThrow: true)
print("OK 1")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: OK 1

_ = try? Foo(shouldThrow: false)
print("OK 2")
// CHECK: PrintingClass.init
// CHECK: PrintingClass.init
// CHECK: PrintingClass.deinit
// CHECK: PrintingClass.deinit
// CHECK: OK 2
