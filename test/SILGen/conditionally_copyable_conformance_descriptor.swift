// RUN: %target-swift-emit-silgen-ossa -o /dev/null -enable-sil-opaque-values %s
// RUN: %target-swift-emit-silgen -enable-library-evolution %s | %FileCheck %s

public struct ConditionallyCopyable<T: ~Copyable>: ~Copyable {
}

extension ConditionallyCopyable: Copyable where T: Copyable {}

public struct NeverCopyable: ~Copyable {}

public struct Index<U: ~Copyable>: Hashable { }

extension ConditionallyCopyable where T: ~Copyable {
    // CHECK-LABEL: sil_property #ConditionallyCopyable.sometimesCopyableBase_sometimesCopyableValue
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>) -> @out τ_0_0
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConditionallyCopyable<τ_0_0>) -> ()
    public private(set) var sometimesCopyableBase_sometimesCopyableValue: T {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.sometimesCopyableBase_alwaysCopyableValue
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>) -> @out Int
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed Int, @inout ConditionallyCopyable<τ_0_0>) -> ()
    public private(set) var sometimesCopyableBase_alwaysCopyableValue: Int {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.neverCopyableBase_alwaysCopyableValue
    public private(set) var sometimesCopyableBase_neverCopyableValue: NeverCopyable {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableVAARi_zrlE09sometimesf5Base_gF10IndexValueqd__AA0I0Vyqd__G_tcRi_d__luir
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out τ_1_0
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0> (@in_guaranteed τ_1_0, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ()
    public private(set) subscript<U: ~Copyable>(
        sometimesCopyableBase_sometimesCopyableIndexValue _: Index<U>
    ) -> U {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableVAARi_zrlE09sometimesf5Base_gF5ValuexAA5IndexVyqd__G_tcRi_d__luir
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out τ_0_0
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed τ_0_0, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ()
    public private(set) subscript<U: ~Copyable>(
        sometimesCopyableBase_sometimesCopyableValue _: Index<U>
    ) -> T {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableVAARi_zrlE09sometimesf11Base_alwaysF5ValueSiAA5IndexVyqd__G_tcRi_d__luig
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out Int
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed Int, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ()
    public private(set) subscript<U: ~Copyable>(
        sometimesCopyableBase_alwaysCopyableValue _: Index<U>
    ) -> Int {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.subscript{{.*}}never
    public private(set) subscript<U: ~Copyable>(
        sometimesCopyableBase_neverCopyableValue _: Index<U>
    ) -> NeverCopyable {
        get { }
        set { }
    }
}

extension ConditionallyCopyable where T == NeverCopyable, T: ~Copyable {
    // CHECK-NOT: sil_property #ConditionallyCopyable.neverCopyableBase_sometimesCopyableValue
    public private(set) var neverCopyableBase_sometimesCopyableValue: T {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.neverCopyableBase_alwaysCopyableValue
    public private(set) var neverCopyableBase_alwaysCopyableValue: Int {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.neverCopyableBase_neverCopyableValue
    public private(set) var neverCopyableBase_neverCopyableValue: NeverCopyable {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.subscript{{.*}}never
    public private(set) subscript<U: ~Copyable>(
        neverCopyableBase_sometimesCopyableIndexValue _: Index<U>
    ) -> U {
        get { }
        set { }
    }
    public private(set) subscript<U: ~Copyable>(
        neverCopyableBase_sometimesCopyableValue _: Index<U>
    ) -> T {
        get { }
        set { }
    }
    public private(set) subscript<U: ~Copyable>(
        neverCopyableBase_alwaysCopyableValue _: Index<U>
    ) -> Int {
        get { }
        set { }
    }
    public private(set) subscript<U: ~Copyable>(
        neverCopyableBase_neverCopyableValue _: Index<U>
    ) -> NeverCopyable {
        get { }
        set { }
    }
}

extension ConditionallyCopyable where T: Copyable {
    // CHECK-LABEL: sil_property #ConditionallyCopyable.alwaysCopyableBase_sometimesCopyableValue
    // CHECK-SAME:    getter {{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>) -> @out τ_0_0
    // CHECK-SAME:    setter {{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed τ_0_0, @inout ConditionallyCopyable<τ_0_0>) -> ()
    public private(set) var alwaysCopyableBase_sometimesCopyableValue: T {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.alwaysCopyableBase_alwaysCopyableValue
    // CHECK-SAME:    getter {{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>) -> @out Int
    // CHECK-SAME:    setter {{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0> (@in_guaranteed Int, @inout ConditionallyCopyable<τ_0_0>) -> ()
    public private(set) var alwaysCopyableBase_alwaysCopyableValue: Int {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property alwaysCopyableBase_neverCopyableValue
    public private(set) var alwaysCopyableBase_neverCopyableValue: NeverCopyable {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableV06alwaysf14Base_sometimesF10IndexValueqd__AA0J0Vyqd__G_tcRi_d__luir
    // CHECK-SAME:    getter {{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out τ_1_0
    // CHECK-SAME:    setter {{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0> (@in_guaranteed τ_1_0, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ())
    public private(set) subscript<U: ~Copyable>(
        alwaysCopyableBase_sometimesCopyableIndexValue _: Index<U>
    ) -> U {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableV06alwaysf14Base_sometimesF5ValuexAA5IndexVyqd__G_tcRi_d__luig
    // CHECK-SAME:    getter {{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out τ_0_0
    // CHECK-SAME:    setter {{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed τ_0_0, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ()
    public private(set) subscript<U: ~Copyable>(
        alwaysCopyableBase_sometimesCopyableValue _: Index<U>
    ) -> T {
        get { }
        set { }
    }

    // CHECK-LABEL: sil_property #ConditionallyCopyable.subscript{{.*}}, id @$s45conditionally_copyable_conformance_descriptor21ConditionallyCopyableV06alwaysf5Base_gF5ValueSiAA5IndexVyqd__G_tcRi_d__luig
    // CHECK-SAME:    getter {{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> @out Int
    // CHECK-SAME:    setter {{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0><τ_1_0 where τ_1_0 : ~Copyable> (@in_guaranteed Int, @inout ConditionallyCopyable<τ_0_0>, @in_guaranteed Index<τ_1_0>) -> ()
    public private(set) subscript<U: ~Copyable>(
        alwaysCopyableBase_alwaysCopyableValue _: Index<U>
    ) -> Int {
        get { }
        set { }
    }

    // CHECK-NOT: sil_property #ConditionallyCopyable.subscript{{.*}}never
    public private(set) subscript<U: ~Copyable>(
        alwaysCopyableBase_neverCopyableValue _: Index<U>
    ) -> NeverCopyable {
        get { }
        set { }
    }
}

public protocol MaybeCopyableProto: ~Copyable {
    associatedtype Value: ~Copyable
    init(_ value: consuming Value)
    var value: Value { get }
}

// The key path accessors must require that 'Self' be copyable, since they copy
// the root.
extension MaybeCopyableProto where Self: ~Copyable, Value: Copyable {
    // CHECK-LABEL: sil_property #MaybeCopyableProto.sometimesCopyableSelf_alwaysCopyableValue
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0 where τ_0_0 : MaybeCopyableProto, τ_0_0.Value : Copyable> (@in_guaranteed τ_0_0) -> @out τ_0_0.Value
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0 where τ_0_0 : MaybeCopyableProto, τ_0_0.Value : Copyable> (@in_guaranteed τ_0_0.Value, @inout τ_0_0) -> ()
    public var sometimesCopyableSelf_alwaysCopyableValue: Value {
        get { value }
        set { self = Self(newValue) }
    }
}

public protocol MaybeEscapableProto: ~Escapable {}

extension MaybeEscapableProto where Self: ~Escapable {
    // CHECK-LABEL: sil_property #MaybeEscapableProto.sometimesEscapableSelf
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0 where τ_0_0 : MaybeEscapableProto> (@in_guaranteed τ_0_0) -> @out Int
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0 where τ_0_0 : MaybeEscapableProto> (@in_guaranteed Int, @inout τ_0_0) -> ()
    public var sometimesEscapableSelf: Int {
        get { 0 }
        set { }
    }
}

public protocol MaybeCopyableOrEscapableProto: ~Copyable, ~Escapable {}

extension MaybeCopyableOrEscapableProto where Self: ~Copyable & ~Escapable {
    // CHECK-LABEL: sil_property #MaybeCopyableOrEscapableProto.sometimesCopyableOrEscapableSelf
    // CHECK-SAME:    getter @{{[^ ]*}} : $@convention(keypath_accessor_getter) <τ_0_0 where τ_0_0 : MaybeCopyableOrEscapableProto> (@in_guaranteed τ_0_0) -> @out Int
    // CHECK-SAME:    setter @{{[^ ]*}} : $@convention(keypath_accessor_setter) <τ_0_0 where τ_0_0 : MaybeCopyableOrEscapableProto> (@in_guaranteed Int, @inout τ_0_0) -> ()
    public var sometimesCopyableOrEscapableSelf: Int {
        get { 0 }
        set { }
    }
}

public struct NeverCopyableConformer: MaybeCopyableProto, ~Copyable {
    public init(_ value: consuming Int) {}
    public var value: Int { 0 }
}

// 'Self' is never copyable here, so there is no descriptor, like for the members
// of a non-copyable type.
extension MaybeCopyableProto where Self: ~Copyable, Self == NeverCopyableConformer {
    // CHECK-NOT: sil_property #MaybeCopyableProto.neverCopyableSelf
    public var neverCopyableSelf: Int { 0 }
}
