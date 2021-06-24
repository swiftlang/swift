// RUN: %target-swift-frontend -emit-ir %s

public protocol PublicContent {
    associatedtype Model
    init(_ model: Model)
}

public protocol PublicConvertible {
    associatedtype Public
    func toPublic() -> Public
}

extension PublicConvertible where Public: PublicContent, Public.Model == Self {
    public func toPublic() -> Public {
        Public(self)
    }
}

extension Array: PublicConvertible where Element: PublicConvertible {
    public func toPublic() -> [Element.Public] {
        map { $0.toPublic() }
    }
}
