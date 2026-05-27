// RUN: %target-swift-frontend -emit-ir %s

public struct LowerModel {
	public typealias Index = Int
}

public protocol LowerChildA {
	typealias Model = LowerModel
	typealias ModelIndex = Model.Index
}

public protocol LowerChildB {
	typealias Model = LowerModel
	typealias ModelIndex = Model.Index
}

public protocol Parent: LowerChildA & LowerChildB {}

public func foo<T : Parent>(_: T, _: T.Model, _: T.ModelIndex) {}
