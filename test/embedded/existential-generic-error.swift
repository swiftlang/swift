// RUN: %target-swift-emit-ir -parse-as-library -module-name main -verify %s -enable-experimental-feature Embedded -wmo

// REQUIRES: optimized_stdlib
// REQUIRES: swift_feature_Embedded

public protocol MyProtocol: AnyObject {
    func foo<T: BinaryInteger>(ptr: UnsafeMutableRawPointer?, value: T)
}

func test_some(p: some MyProtocol) {
  p.foo(ptr: nil, value: 0) // expected-error {{a protocol type cannot contain a generic method 'foo(ptr:value:)' in embedded Swift}}
}

public func test_any(p: any MyProtocol) {
  test_some(p: p)
  // expected-warning@-1{{cannot use generic global function 'test_some(p:)' on a value of type 'any MyProtocol' in Embedded Swift}}
}

// The same, but where the caller of the generic requirement does get
// specialized: the witness_method must keep the requirement's own generic
// parameters, because IRGen derives the signature of the call from the
// unsubstituted requirement and still passes their metadata and witness tables.
public protocol OtherProtocol: AnyObject {
  func bar<T: BinaryInteger>(value: T)
}

func call_bar<T: BinaryInteger>(value: T, p: any OtherProtocol) {
  p.bar(value: value) // expected-error {{a protocol type cannot contain a generic method 'bar(value:)' in embedded Swift}}
  // expected-warning@-1 {{cannot use generic instance method 'bar(value:)' on a value of type 'any OtherProtocol' in Embedded Swift}}
}

public func test_specialized_caller(p: any OtherProtocol) {
  call_bar(value: 0, p: p) // expected-note {{generic specialization called here}}
}
