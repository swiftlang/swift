// RUN: %target-typecheck-verify-swift -solver-scope-threshold=10000 -solver-enable-short-conversions

// https://github.com/swiftlang/swift/issues/73885

enum Dimension: String {
    case dim1
    case dim2
    case dim3
    case dim4
    case dim5
    case dim6
    case dim7
    case dim8
    case dim9
    case dim10
}

struct Event {
    let properties: [Dimension: String]
    let foo: Int
    let bar: String

    init(properties: [Dimension: String] = [:], foo: Int? = nil) {
        self.init(properties: properties, foo: foo, bar: "bar")
    }

    init(properties: [Dimension: String], foo: Int?, bar: String) {
        self.properties = properties
        self.foo = foo ?? 0
        self.bar = bar
    }
}

struct Foo {
    let bar: Bar?
    let baz: Int?
}

struct Bar {
    let str: String
    let bar: String?
    let baz: Int
    let bad: Double?
    let isBar: Bool?
}

extension Event {
    static func test1(foo: Foo) -> Event {
        // Invalid expression, because the dictionary literal has the
        // wrong type.
        .init(properties: [
            .dim1: foo.bar?.bar,
            .dim2: foo.bar?.baz,
            .dim3: foo.bar?.bad,
            .dim4: foo.bar?.isBar,
            .dim5: foo.bar?.bar,
            .dim6: foo.bar?.baz,
            .dim7: foo.bar?.bad,
            .dim8: foo.bar?.isBar,
            .dim9: foo.bar?.bar,
            .dim10: foo.bar.str
            // expected-error@-1 {{value of optional type 'Bar?' must be unwrapped to refer to member 'str' of wrapped base type 'Bar'}}
            // expected-note@-2 {{chain the optional using '?' to access member 'str' only for non-'nil' base values}}
            // expected-note@-3 {{force-unwrap using '!' to abort execution if the optional value contains 'nil'}}
        ].compactMapValues { $0 })
        // expected-error@-1 {{cannot convert value of type 'Any' to closure result type 'String?'}}
    }
}
