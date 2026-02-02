// RUN: not %target-swift-frontend -typecheck  %s

// https://github.com/apple/swift/issues/57802

public struct {
    @UserDefault()
    public static var region: String
}

@propertyWrapper
public struct UserDefault {
    init() {}
    public var wrappedValue: String
}
