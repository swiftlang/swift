public protocol P {
    func blah()
}

public struct S : P {
    public func blah() {}
}

extension P {
    @available(macOS 100, *)
    @_weakLinked public func someAPI() -> some P { return S() }
}
