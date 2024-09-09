// RUN: %target-typecheck-verify-swift -swift-version 5 -package-name myPkg

// https://github.com/swiftlang/swift/issues/65640

@propertyWrapper
struct Wrapper {
  init(wrappedValue: Int) {}
}
func test(@Wrapper x: Int) {}
