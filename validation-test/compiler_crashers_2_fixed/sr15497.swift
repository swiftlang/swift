// RUN: not %target-swift-frontend -typecheck  %s

public struct {
    @UserDefault()
    public static var region: String
}

@propertyWrapper
public struct UserDefault {
    init() {}
    public var wrappedValue: String
}
