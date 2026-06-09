// RUN: %target-swift-frontend -O -emit-sil %s -o /dev/null
// RUN: %target-swift-frontend -Osize -wmo -emit-sil %s -o /dev/null

// Make sure CSE does not crash when inlining a `lazy var` getter whose body
// binds dynamic Self into a caller (e.g. an escaping `[weak self]` closure)
// that has no self-metadata parameter. Previously, `processLazyPropertyGetters`
// inlined unconditionally and the cloner asserted in
// `SILFunction::getDynamicSelfMetadata()`.

public class B { public init() {} }

public class Repro {
    @inline(never) private static func make() -> B { B() }
    private lazy var lazyValue: B = Self.make()

    public func go() {
        let f: () -> Void = { [weak self] in
            guard let self else { return }
            _ = self.lazyValue
            _ = self.lazyValue
        }
        f()
    }
}
