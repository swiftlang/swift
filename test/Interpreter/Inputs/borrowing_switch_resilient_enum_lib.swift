public struct Payload {
    public let a: Int?
    public let b: Int?
    public let c: Int?
    public let d: Int?
    public init(a: Int? = nil, b: Int? = nil, c: Int? = nil, d: Int? = nil) {
        self.a = a; self.b = b; self.c = c; self.d = d
    }
}

public enum MyEnum: ~Copyable {
    case big(Payload)
    case small(String)
}

public extension MyEnum {
    @inlinable var isSmall: Bool {
		switch self {
			case .small: true
			default: false
		}
    }
    @inlinable var smallValue: String? {
		switch self {
			case .small(let t): t
			default: nil
		}
    }
}

@frozen public enum MyFrozenEnum: ~Copyable {
    case big(Payload)
    case small(String)
}

public extension MyFrozenEnum {
    @inlinable var isSmall: Bool {
		switch self {
			case .small: true
			default: false
		}
    }
    @inlinable var smallValue: String? {
		switch self {
			case .small(let t): t
			default: nil
		}
    }
}

public enum MyGenericEnum<T>: ~Copyable {
	case gen(T)
	case small(String)
}

public extension MyGenericEnum {
    @inlinable var isSmall: Bool {
		switch self {
			case .small: true
			default: false
		}
    }
    @inlinable var smallValue: String? {
		switch self {
			case .small(let t): t
			default: nil
		}
    }
}

