// RUN: %target-typecheck-verify-swift
//
// https://github.com/swiftlang/swift/issues/87565
public protocol P {
  func foo()
}
public struct S: P {
  @_spi(Secret)
  public func foo() {}
}
