// RUN: not %target-swift-frontend -typecheck %s
public struct Use { @Wrap var value: some Doubl = 1.0 }
@propertyWrapper public struct Wrap { public var wrappedValue: Double }

