// RUN: %target-swift-frontend -typecheck %s

extension Sequence {
    func sorted<T: Comparable, K: KeyPath<Element, T>>(by keyPath: K) -> Array<Element> {
        self.sorted { $0[keyPath:keyPath] < $1[keyPath:keyPath] }
    }
}

struct Foo {
    let a: Int
}

func main() {
    print([Foo(a: 2), Foo(a:1), Foo(a:4), Foo(a:3)].sorted(by: \Foo.a))
}

main()
