// RUN: %target-swift-emit-silgen -primary-file %s | %FileCheck %s

@propertyWrapper
public struct Horse<Value> {
	private var stored: Value
	
	public var wrappedValue: Value {
		get { stored }
		set { stored = newValue }
	}		

	public init(wrappedValue initialValue: Value) {
		stored = initialValue
	}

	public var projectedValue: Self {
		mutating get { self }
		set { self = newValue }
	}
}

public protocol Fuzzball {}

public struct Cat : Fuzzball {}

public struct Dog<Pet : Fuzzball> {
  @Horse public var foo: Int = 17
}

extension Dog where Pet == Cat {
  public init() {}
}

// CHECK-LABEL: sil [ossa] @$s39property_wrappers_constrained_extension3DogVA2A3CatVRszrlEACyAEGycfC : $@convention(method) (@thin Dog<Cat>.Type) -> Dog<Cat> {
// CHECK: [[FN:%.*]] = function_ref @$s39property_wrappers_constrained_extension3DogV4_foo33_{{.*}}LLAA5HorseVySiGvpfi : $@convention(thin) <τ_0_0 where τ_0_0 : Fuzzball> () -> Int
// CHECK: apply [[FN]]<Cat>() : $@convention(thin) <τ_0_0 where τ_0_0 : Fuzzball> () -> Int