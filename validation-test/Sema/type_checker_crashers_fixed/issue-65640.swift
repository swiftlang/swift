// RUN: %target-typecheck-verify-swift -swift-version 5 -package-name myPkg

// https://github.com/swiftlang/swift/issues/65640

@propertyWrapper
struct Wrapper {
  // expected-error@-1{{property wrapper type 'Wrapper' does not contain a non-static property named 'wrappedValue'}}
  init(wrappedValue: Int) {}
}
func test(@Wrapper x: Int) {}
