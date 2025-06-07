public protocol P {}

public func callee<T: P>(t: T) -> some P { return t }
