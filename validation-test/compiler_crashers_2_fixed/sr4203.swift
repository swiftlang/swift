// RUN: %target-swift-frontend -emit-ir %s

public protocol SomeProto {
    associatedtype ThingType
	func getThing() -> ThingType
}

public protocol SpecialThing: RandomAccessCollection {
}

public protocol Castable {
	associatedtype Source
	static func cast(from: Source) -> Self
}

public struct ThingGetter<P: SomeProto, T> {
	public let thing: P
}

extension ThingGetter where P.ThingType: SpecialThing, T: Castable, P.ThingType.Iterator.Element == T.Source {

	public func getView() -> ThingView {
		return ThingView(thing: thing.getThing())
	}
	
	public struct ThingView: SpecialThing {
		let thing: P.ThingType

		public typealias Index = P.ThingType.Index
		public var startIndex: Index    { return thing.startIndex }
		public var endIndex:   Index    { return thing.startIndex }
		public var count: Int           { return thing.count }
		public func index(after i: Index) -> Index {
			return thing.index(after: i)
		}
		public func index(before i: Index) -> Index {
			return thing.index(before: i)
		}
		public subscript(i: Index) -> T {
			return T.cast(from: thing[i])
		}
	}
}
