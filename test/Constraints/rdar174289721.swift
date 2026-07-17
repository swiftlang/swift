// RUN: %target-swift-frontend -emit-silgen %s | %FileCheck %s

struct MyStruct<T> {
	let desiredInitializerCalled: Bool

	init(_ keyPath: KeyPath<T, Int>) where T: BaseType {
		self.desiredInitializerCalled = true
	}

	init(_ keyPath: KeyPath<T, Int>) {
		self.desiredInitializerCalled = false
	}
}

func genericMethod<T: Subtype>(_ arg: MyStruct<T>) {
	print("\(arg.desiredInitializerCalled)")
}

class BaseType {}
class Subtype: BaseType {
    let number = 42
}
class LeafType: Subtype {}

// CHECK-LABEL: sil [ossa] @main : $@convention(c) (Int32, UnsafeMutablePointer<Optional<UnsafeMutablePointer<Int8>>>) -> Int32 {

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
let kp = \LeafType.number
genericMethod(MyStruct(kp))

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
genericMethod(MyStruct(\.number))

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
genericMethod(MyStruct(\LeafType.number))

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
genericMethod(MyStruct<LeafType>(\LeafType.number))

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
let _ = MyStruct(\LeafType.number)

// CHECK: function_ref @$s13rdar1742897218MyStructVyACyxGs7KeyPathCyxSiGcAA8BaseTypeCRbzlufC : $@convention(method) <τ_0_0 where τ_0_0 : BaseType> (@owned KeyPath<τ_0_0, Int>, @thin MyStruct<τ_0_0>.Type) -> MyStruct<τ_0_0>
_ = MyStruct(\LeafType.number).desiredInitializerCalled

// CHECK: return
// CHECK: }
