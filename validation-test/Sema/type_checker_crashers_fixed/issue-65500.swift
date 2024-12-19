//  RUN: %target-typecheck-verify-swift

// https://github.com/swiftlang/swift/issues/65500

@propertyWrapper
struct Wrapper { 
  var wrappedValue: Bool { true }
  init(wrappedValue: Bool, er: Void) {}

  var projectedValue: Bool { true }
  init(projectedValue: Bool) {}
}

func test(@Wrapper x: Bool) {} 
test(x: false) 
// expected-error@-1 {{cannot convert value 'x' of type 'Bool' to expected type 'Wrapper', use wrapper instead}}{{9-9=$}}