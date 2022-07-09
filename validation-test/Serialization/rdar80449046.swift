// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s

@propertyWrapper
public struct TestWrapper {
  public var wrappedValue: Int
  public init(wrappedValue: Int) { self.wrappedValue = wrappedValue }
}

@frozen public struct Test {
  @TestWrapper public var x: Int = 42
}

