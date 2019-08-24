// RUN: not %target-swift-frontend %s -typecheck
struct Foo: Strideable {
    // typealias Stride = Int
    let x: Int

    func distance(to other: Foo) -> Foo.Stride { return abs(other.x - x) }
    func advanced(by n: Foo.Stride) -> Foo { return Foo(x: x + n) }
}
