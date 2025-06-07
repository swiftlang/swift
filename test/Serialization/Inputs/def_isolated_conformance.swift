public protocol MyProtocol {
  func f()
}

@MainActor
public class MyClass { }

extension MyClass: @MainActor MyProtocol {
  @MainActor public func f() { }
}

public protocol OtherProtocol {
}

extension MyClass: OtherProtocol { }
