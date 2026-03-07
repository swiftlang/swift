// RUN: %target-typecheck-verify-swift

class A {}
class B: A {}
class C: B {}
class D: B {}
class E: D {}
class F {}

protocol P {}
protocol Q: P {}
protocol R: P {}
protocol S {}

extension E: P {}

extension Int: Q {}
extension String: R {}

func test<T: ~Copyable>(_: consuming T, _: consuming T) ->T { fatalError() }

func testSimple(x: Int, y: String) {
  test(x, y) // expected-error {{conflicting arguments to generic parameter 'T' ('Int' vs. 'String')}}
}

func testClass1(x: C, y: E) -> B {
  let result = test(x, y)
  return result
}

func testClass2(x: E, y: F) {
  _ = test(x, y) // expected-error {{conflicting arguments to generic parameter 'T' ('E' vs. 'F')}}
}

func testMetatype1(x: C.Type, y: E.Type) -> B.Type {
  let result = test(x, y)
  return result
}

func testMetatype2(x: E.Type, y: F.Type) {
  _ = test(x, y) // expected-error {{conflicting arguments to generic parameter 'T' ('E.Type' vs. 'F.Type')}}
}

func testOptional1(x: E, y: C?) -> B? {
  let result = test(x, y)
  return result
}

func testOptional2(x: E?, y: C) -> B? {
  let result = test(x, y)
  return result
}

func testOptional3(x: E?, y: C?) -> B? {
  let result = test(x, y)
  return result
}

func testArray1(x: [E], y: [C]) -> [B] {
  let result = test(x, y)
  return result
}

func testTuple1(x: (E, C), y: (C, E)) -> (B, B) {
  let result = test(x, y)
  return result
}

func testFunction1(x: @escaping (E) -> C, y: @escaping (A) -> E) -> (E) -> B {
  let result = test(x, y)
  return result
}

func testFunction2(x: @escaping () -> Int, y: @escaping () -> String) {
  let _ = test(x, y)  // expected-error {{conflicting arguments to generic parameter 'T' ('() -> Int' vs. '() -> String')}}
}

func testExistential1(x: any Q, y: any R) -> any P {
  let result = test(x, y)
  return result
}

func testExistential2(x: any Q & D, y: any R & C) -> any P & B {
  let result = test(x, y)
  return result
}

func testExistential3(x: any Q & D, y: any R & F) -> any P {
  let result = test(x, y)
  return result
}

func testExistential4(x: any P & C, y: any S & D) -> B {
  let result = test(x, y)
  return result
}

func testExistential5(x: consuming any ~Copyable, y: any P) -> any ~Copyable {
  let result = test(x, y)
  return result
}

func testExistentialMetatype1(x: any Q.Type, y: any R.Type) -> any P.Type {
  let result = test(x, y)
  return result
}
