// RUN: %target-typecheck-verify-swift

@_specialize(exported: true, where T == Int) // expected-error{{'exported: true' has no effect in '_specialize' attribute}}
public func myGenericFunc<T>(_ t: T) {}
