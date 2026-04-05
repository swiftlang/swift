// RUN: %target-swift-emit-sil -enable-experimental-feature Lifetimes -enable-experimental-feature AddressableParameters -verify %s
//
// REQUIRES: swift_feature_AddressableParameters
// REQUIRES: swift_feature_Lifetimes

struct Optionable<Wrapped: ~Copyable>: ~Copyable {
  public var span: Span<Wrapped> {
    @_addressableSelf
    @_lifetime(borrow self)
    get {
		fatalError()
    }
  }

  @_addressableSelf
  @_lifetime(borrow self)
  public func getSpan() -> Span<Wrapped> {    
	fatalError()
  }

  public mutating func set(_: consuming Wrapped) { }
}

extension Optionable: Copyable where Wrapped: Copyable {}

func testProperty_mutate(_ o: inout Optionable<[Int]>, _ value: [Int]) -> Int {
  let span = o.span // expected-note{{}}
  o.set(value) // expected-error{{overlapping access}}
  return span.count
}
func testMethod_mutate(_ o: inout Optionable<[Int]>, _ value: [Int]) -> Int {
  let span = o.getSpan() // expected-note{{}}
  o.set(value) // expected-error{{overlapping access}}
  return span.count
}

func testProperty_reassign(_ o: inout Optionable<[Int]>, _ value: Optionable<[Int]>) -> Int {
  let span = o.span // expected-error{{escapes}} expected-note{{}}
  o = value
  return span.count // expected-note{{}}
}
func testMethod_reassign(_ o: inout Optionable<[Int]>, _ value: Optionable<[Int]>) -> Int {
  let span = o.getSpan() // expected-error{{escapes}} expected-note{{}}
  o = value
  return span.count // expected-note{{}}
}
