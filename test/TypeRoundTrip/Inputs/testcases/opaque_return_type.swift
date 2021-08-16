import RoundTrip

protocol P {}
extension Int: P {}

@available(macOS 10.15, *)
func foo() -> some P { return 0 }

@available(macOS 10.15, *)
var prop: some P { return 0 }

@available(macOS 10.15, *)
func bar() -> some Sequence { return [] }

struct G<T> {}

extension G where T == Int {
  @available(macOS 10.15, *)
  var baz: some P { return 0 }
}

public func test() {
  if #available(macOS 10.15, *) {
    roundTripType(type(of:foo))
    roundTripType(type(of:prop))
    roundTripType(type(of:bar))
    roundTripType(type(of:G<Int>().baz))
    roundTripType(type(of:bar()).Element.self)
  } else {
    print("Skipped as there is no support for `some Foo` syntax")
  }
}
