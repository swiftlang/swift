// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -runtime-compatibility-version none) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

@dynamicMemberLookup
public struct Box<T>: ~Copyable {
    init() {
        self.value = UnsafeMutablePointer<T>.allocate(capacity: 1)
    }

    public subscript<U>(dynamicMember member: WritableKeyPath<T, U>) -> U {
        @_transparent
        get { return self.value.pointer(to: member)!.pointee }

        @_transparent
        set { self.value.pointer(to: member)!.pointee = newValue }
    }

    @usableFromInline
    var value: UnsafeMutablePointer<T>
}

public struct Foo {
    var a: Int
    var b: Int
}

public func test_read(x: inout Box<Foo>) -> Int {
    return x.b
}

public func test_write(x: inout Box<Foo>) {
    x.b = 42
}

var box = Box<Foo>()
_ = test_read(x: &box)
test_write(x: &box)
print(box.b)
// CHECK: 42
