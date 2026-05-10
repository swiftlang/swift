
public class NonSendableClass {}

@available(*, unavailable)
extension NonSendableClass: @unchecked Sendable {}

public struct SendableStruct: Sendable {}

public struct AnotherSendableStruct: Sendable {}

public protocol SendableSubProtocol: Sendable {}

public struct NonSendableViaProtocol {
    public var value: Int = 0
    public init() {}
}

@available(*, unavailable)
extension NonSendableViaProtocol: SendableSubProtocol {}

@_nonSendable
public struct NonSendableViaAttr {
    public var value: Int = 0
    public init() {}
}

public class NonSendableBase {}

@available(*, unavailable)
extension NonSendableBase: @unchecked Sendable {}

public class NonSendableChild: NonSendableBase {}

public protocol AnotherProtocol {}

public class NonSendableViaComposition {}

@available(*, unavailable)
extension NonSendableViaComposition: Sendable & AnotherProtocol {}
