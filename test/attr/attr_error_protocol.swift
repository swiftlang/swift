// RUN: %target-parse-verify-swift

@__error_protocol
protocol ErrorType {
  func foo()
  func bar()
  var x: Int { get }
}

@__error_protocol class NotAProtocol {} // expected-error{{@__error_protocol may only be used on 'protocol' declarations}}

@__error_protocol // expected-error{{cannot contain mutating methods}}
protocol HasMutatingProperty {
  var x: Int {
    get
    set // expected-note{{mutating method here}}
  }
}

@__error_protocol // expected-error{{cannot contain mutating methods}}
protocol HasMutatingMethod {
  mutating func foo() // expected-note{{mutating method here}}
}
