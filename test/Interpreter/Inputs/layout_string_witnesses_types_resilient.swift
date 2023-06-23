
public struct SimpleResilient {
    let x: Int
    let y: AnyObject

    public init(x: Int, y: AnyObject) {
        self.x = x
        self.y = y
    }
}

public struct GenericResilient<C, T> {
    let x: C
    let y: T

    public init(x: C, y: T) {
        self.x = x
        self.y = y
    }
}

public enum ResilientMultiPayloadEnum<T> {
    case empty0
    case empty1
    case nonEmpty0(AnyObject)
    case nonEmpty1(T)
}

public func getResilientMultiPayloadEnumEmpty0<T>(_ t: T.Type) -> ResilientMultiPayloadEnum<T> {
    return .empty0
}
