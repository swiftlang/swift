// RUN: %target-swift-frontend -emit-ir %s

final class Baz {}

final class Bar {
    private let x: Baz
    init(x: Baz) {
        self.x = x
    }
}

final class Foo {
    private var bar: Bar?

    private func navigate(with baz: Baz?) {
        bar = nil
        guard let baz = baz else { return }
        let bar = Bar(x: baz)
        self.bar = bar
    }
}
