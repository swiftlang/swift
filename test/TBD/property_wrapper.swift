// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all %s
// RUN: %target-swift-frontend -emit-ir -o/dev/null -parse-as-library -module-name test -validate-tbd-against-ir=all -enable-library-evolution %s

@propertyWrapper
public struct Wrapper<Value> {
  public var wrappedValue: Value

  public init(wrappedValue: Value) {
    self.wrappedValue = wrappedValue
  }
}

public struct UseWrapper {
  @Wrapper public var string = "hello"
}

