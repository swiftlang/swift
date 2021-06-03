// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -module-name errors -emit-module -o %t/errors.swiftmodule -experimental-allow-module-with-compiler-errors %s

// Property wrappers are invalid on top level code, check allowing errors
// does not crash

@propertyWrapper
public struct Wrapper<T> {
  public var wrappedValue: T
  public init() {}
}
@Wrapper
public var topLevelVar: Int = 10
