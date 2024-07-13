
public class NonSendableClass {}

@available(*, unavailable)
extension NonSendableClass: @unchecked Sendable {}

public struct SendableStruct: Sendable {}

public struct AnotherSendableStruct: Sendable {}
