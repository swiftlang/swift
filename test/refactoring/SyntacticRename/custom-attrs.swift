@propertyWrapper
struct /*wrapper:def*/Foo<T> {
    public var wrappedValue: T
    init(initialValue: T) {
        wrappedValue = initialValue
    }
    init(initialValue: T, otherThing: Bool) {
        self.init(initialValue: initialValue)
    }
    var projectedValue: Projection<T> {
      get { Projection(item: wrappedValue) }
    }
}

struct Projection<T> {
    var item: T
}

@resultBuilder
struct /*builder:def*/Other {
    public static func buildBlock(_ components: String...) -> String {
        return components.joined()
    }
}

struct Bar {
    @/*wrapper*/Foo
    var /*wrapped:def*/foo: Int = 10
    @/*wrapper*/Foo(initialValue: "hello")
    var bar: String
    @/*wrapper*/Foo
    var jim: String = {
        struct Bar {
            @/*wrapper*/Foo
            var inner: String = "It's 42"
        }
        return Bar().inner
    }()

    func combined(@/*builder*/Other _ a: () -> String) -> String {
        return a()
    }

     @/*builder*/Other
    func hello() -> String {
        "hello"
        "there"
    }

    func baz() {
        let _: /*wrapper*/Foo<Int> = /*wrapped+1*/_foo
        let _: Int = /*wrapped+1*/_foo.wrappedValue
        let _: Int = /*wrapped*/foo
        let _: Projection<Int> = /*wrapped+1*/$foo
        let _: /*wrapper*/Foo<String> = _bar
    }
}


// REQUIRES: swift_swift_parser
// RUN: %empty-directory(%t.result)
// RUN: %empty-directory(%t.ranges)
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="wrapper" -is-non-protocol-type -old-name "Foo" >> %t.ranges/custom-attrs-Foo.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="wrapped" -old-name "foo" >> %t.ranges/custom-attrs-wrapped.swift
// RUN: %refactor -find-rename-ranges -source-filename %s -pos="builder" -is-non-protocol-type -old-name "Other" >> %t.ranges/custom-attrs-Other.swift
// RUN: diff -u %S/Outputs/custom-attrs/Foo.swift.expected %t.ranges/custom-attrs-Foo.swift
// RUN: diff -u %S/Outputs/custom-attrs/wrapped.swift.expected %t.ranges/custom-attrs-wrapped.swift
// RUN: diff -u %S/Outputs/custom-attrs/Other.swift.expected %t.ranges/custom-attrs-Other.swift
