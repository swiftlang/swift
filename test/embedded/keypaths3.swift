// RUN: %target-run-simple-swift(   -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none) | %FileCheck %s
// RUN: %target-run-simple-swift(-O -enable-experimental-feature Embedded -wmo -Xfrontend -disable-access-control -runtime-compatibility-version none) | %FileCheck %s

// REQUIRES: swift_in_compiler
// REQUIRES: executable_test
// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

@dynamicMemberLookup
public struct Box<T>: ~Copyable {
    internal init(from t: T) {
        self.value = UnsafeMutablePointer<InnerBox<T>>.allocate(capacity: 1)
        self.value.pointee = .init(t)
    }

    public subscript<Value>(dynamicMember member: WritableKeyPath<InnerBox<T>, Value>) -> Value
    {
        @_transparent
        @inline(__always)
        get { return self.value.pointer(to: member)!.pointee }

        @_transparent
        @inline(__always)
        set { self.value.pointer(to: member)!.pointee = newValue }
    }

    public struct InnerBox<V> {
        public init(_ value: V) { self.innerValue = value }
        public var innerValue: V
    }

    @usableFromInline
    internal var value: UnsafeMutablePointer<InnerBox<T>>
}

struct Foo {
    var a: Int
    var b: Int
}

@inline(never)
public func test() -> Int {
    var x = Box<Foo>(from: Foo(a: 0, b: 0))
    x.innerValue.b = 42
    return x.innerValue.b
}

precondition(test() == 42)
print("OK!")
// CHECK: OK!

public func test_read(x: inout Box<Foo>) -> Int {
    return x.innerValue.b
}

public func test_write(x: inout Box<Foo>) {
    x.innerValue.b = 42
}

var box = Box<Foo>(from: Foo(a: 0, b: 0))
_ = test_read(x: &box)
test_write(x: &box)
print(box.innerValue.b)
// CHECK: 42
