// RUN: %target-swift-frontend -O -emit-sil %s | %FileCheck %s
// RUN: %target-swift-frontend -Osize -wmo -emit-sil %s -o /dev/null

// Make sure CSE does not crash when inlining a `lazy var` getter whose body
// binds dynamic Self into a caller (e.g. an escaping `[weak self]` closure)
// that has no self-metadata parameter. Previously, `processLazyPropertyGetters`
// inlined unconditionally and the cloner asserted in
// `SILFunction::getDynamicSelfMetadata()`.

public class B { public init() {} }

public class ReproClassType {
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

// This is the trivial-return variant: when the lazy getter returns a
// value-typed result (Int here), the closure's two getter calls aren't
// separated by a destroy_value, so CSE's lazy-getter-inlining path picks them
// up and inlines the getter — including its `metatype $@thick @dynamic_self ...`
// instruction — into the closure asserting in `SILFunction::getDynamicSelfMetadata()`.

public class ReproTrivialType {
    @inline(never) private static func make() -> Int { 0 }
    private lazy var lazyValue: Int = Self.make()

    public func go() {
        let f: () -> Void = { [weak self] in
            guard let self else { return }
            _ = self.lazyValue
            _ = self.lazyValue
        }
        f()
    }
}

// Positive case: the apply's self IS the caller's own self argument, so the
// callee's `Self` resolves to the same runtime type the caller's `Self`
// already names. CSE is allowed to fold the second getter call into the first.
//
// Without the fix this still inlines (and crashes for closures); with the
// fix's guard satisfied, the second apply is replaced with a direct load
// from the lazy storage and only one apply to the getter survives.
//
// CHECK-LABEL: sil {{.*}}@$s{{.*}}11CSEPositiveC12twoUsedReadsSiyF :
// CHECK: [[FUNC:%.*]] = function_ref @$s30cse_lazy_property_dynamic_self11CSEPositiveC0B5ValueSivg : $@convention(method) (@guaranteed CSEPositive) -> Int
// CHECK: apply [[FUNC]]({{%[0-9]+}}) : $@convention(method) (@guaranteed CSEPositive) -> Int
// CHECK-NOT: apply [[FUNC]]({{%[0-9]+}}) : $@convention(method) (@guaranteed CSEPositive) -> Int
// CHECK-LABEL: } // end sil function '$s{{.*}}11CSEPositiveC12twoUsedReadsSiyF'
open class CSEPositive {
    @inline(never) open class func make() -> Int { 0 }
    public lazy var lazyValue: Int = Self.make()

    public func twoUsedReads() -> Int {
        let m = Self.make()
        let a = self.lazyValue
        let b = self.lazyValue
        return m &+ a &+ b
    }
}
