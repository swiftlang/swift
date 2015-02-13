// RUN: %target-parse-verify-swift

class HasFunc {
  func HasFunc() -> HasFunc { // expected-error {{use of undeclared type 'HasFunc'}}
    return HasFunc()
  }
  func SomethingElse() -> SomethingElse? { // expected-error {{use of undeclared type 'SomethingElse'}}
    return nil
  }
}

class HasProp {
  var HasProp: HasProp { // expected-error 2 {{use of undeclared type 'HasProp'}}
    return HasProp()
  }
  var SomethingElse: SomethingElse? { // expected-error 2 {{use of undeclared type 'SomethingElse'}}
    return nil
  }
}

protocol SomeProtocol {}
protocol ReferenceSomeProtocol {
  var SomeProtocol: SomeProtocol { get } // expected-error 2 {{use of undeclared type 'SomeProtocol'}}
}
