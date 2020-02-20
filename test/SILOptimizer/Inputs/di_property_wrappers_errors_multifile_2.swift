@propertyWrapper
 public struct WrapGod<T> {
   private var value: T

    public init(wrappedValue: T) {
     value = wrappedValue
   }

    public var wrappedValue: T {
     get { value }
     set { value = newValue }
   }
 }

final class TestCaseRunner {}

struct ContentView { // expected-note {{'init(runner:)' declared here}}
  @WrapGod var runner: TestCaseRunner
}
