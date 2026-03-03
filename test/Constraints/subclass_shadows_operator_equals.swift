// RUN: %target-swift-emit-silgen %s | %FileCheck %s

// rdar://156454697

// The appearance of DynamicSelfType as an argument type to ==
// would lead us to choose the Base.== overload, which is less
// "specific" than Derived.==. There was a similar bug with
// archetypes.
//
// This is a SILGen test, to check the overload choice.

public class Base: Equatable {
    public static func ==(lhs: Base, rhs: Base) -> Bool {
        return false
    }
}

public class Derived: Base {
    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2f1SbyF : $@convention(method) (@guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func f1() -> Bool {
        return self == self
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2f2SbyF : $@convention(method) (@guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func f2() -> Bool {
        let c = self as! Self
        return self == c
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2f3SbyF : $@convention(method) (@guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func f3() -> Bool {
        let c = self as! Self
        return c == self
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2f4SbyF : $@convention(method) (@guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func f4() -> Bool {
        let c = self as! Self
        return c == c
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2g1ySbxACRbzlF : $@convention(method) <T where T : Derived> (@guaranteed T, @guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func g1<T: Derived>(_ c: T) -> Bool {
        return self == c
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2g2ySbxACRbzlF : $@convention(method) <T where T : Derived> (@guaranteed T, @guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func g2<T: Derived>(_ c: T) -> Bool {
        return c == self
    }

    // CHECK-LABEL: sil [ossa] @$s32subclass_shadows_operator_equals7DerivedC2g3ySbxACRbzlF : $@convention(method) <T where T : Derived> (@guaranteed T, @guaranteed Derived) -> Bool {
    // CHECK: function_ref @$s32subclass_shadows_operator_equals7DerivedC2eeoiySbAC_ACtFZ : $@convention(method) (@guaranteed Derived, @guaranteed Derived, @thick Derived.Type) -> Bool
    // CHECK: return
    public func g3<T: Derived>(_ c: T) -> Bool {
        return c == c
    }

    public static func ==(lhs: Derived, rhs: Derived) -> Bool {
        return false
    }
}
