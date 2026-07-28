// RUN: %target-typecheck-verify-swift -solver-disable-enumerate-supertypes

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

struct Exactly<T: ~Copyable> {}

func test<T: ~Copyable>(_: consuming T, _: consuming T) -> Exactly<T> { fatalError() }

func testSimple(x: Int, y: String) {
  test(x, y) // expected-error {{conflicting arguments to generic parameter 'T' ('Int' vs. 'String')}}
}

func testClass1(x: C, y: E) -> Exactly<B> {
  let result = test(x, y)
  return result
}

func testClass2(x: E, y: F) {
  _ = test(x, y) // expected-error {{conflicting arguments to generic parameter 'T' ('E' vs. 'F')}}
}

func testClass3(x: A, y: AnyObject) -> Exactly<AnyObject> {
  let result = test(x, y)
  return result
}

func testClass4(x: A?, y: AnyObject) -> Exactly<AnyObject?> {
  let result = test(x, y)
  return result
}

func testClass3(x: A, y: AnyObject?) -> Exactly<AnyObject?> {
  let result = test(x, y)
  return result
}

func testMetatype1(x: C.Type, y: E.Type) -> Exactly<B.Type> {
  let result = test(x, y)
  return result
}

func testMetatype2(x: E.Type, y: F.Type) -> Exactly<any Any.Type> {
  let result = test(x, y)
  return result
}

func testMetatype3(x: C.Type, y: C?.Type) -> Exactly<any Any.Type> {
  let result = test(x, y)
  return result
}

func testMetatype4(x: C?.Type, y: E?.Type) -> Exactly<any Any.Type> {
  let result = test(x, y)
  return result
}

func testOptional1(x: E, y: C?) -> Exactly<B?> {
  let result = test(x, y)
  return result
}

func testOptional2(x: E?, y: C) -> Exactly<B?> {
  let result = test(x, y)
  return result
}

func testOptional3(x: E?, y: C?) -> Exactly<B?> {
  let result = test(x, y)
  return result
}

func testArray1(x: [E], y: [C]) -> Exactly<[B]> {
  let result = test(x, y)
  return result
}

func testTuple1(x: (E, C), y: (C, E)) -> Exactly<(B, B)> {
  let result = test(x, y)
  return result
}

func testTuple2(x: (x: E, y: C), y: (C, E)) -> Exactly<(B, B)> {
  let result = test(x, y)
  return result
}

func testTuple3(x: (E, C), y: (x: C, y: E)) -> Exactly<(B, B)> {
  let result = test(x, y)
  return result
}

func testTuple4(x: (x: E, y: C), y: (x: C, y: E)) -> Exactly<(x: B, y: B)> {
  let result = test(x, y)
  return result
}

func testFunction1(x: @escaping (E) -> C, y: @escaping (A) -> E) -> Exactly<(E) -> B> {
  let result = test(x, y)
  return result
}

func testFunction2(x: @escaping () -> Int, y: @escaping () -> String) -> Exactly<() -> Any> {
  let result = test(x, y)
  return result
}

func testFunction3(x: @escaping () throws -> Int, y: @escaping () -> Int) -> Exactly<() throws -> Int> {
  let result = test(x, y)
  return result
}

func testFunction4(x: @escaping () -> Int, y: @escaping () throws -> Int) -> Exactly<() throws -> Int> {
  let result = test(x, y)
  return result
}

func testFunction5(x: @escaping @Sendable () -> Int, y: @escaping () -> Int) -> Exactly<() -> Int> {
  let result = test(x, y)
  return result
}

func testFunction6(x: @escaping () -> Int, y: @escaping @Sendable () -> Int) -> Exactly<() -> Int> {
  let result = test(x, y)
  return result
}

func testExistential1(x: any Q, y: any R) -> Exactly<any P> {
  let result = test(x, y)
  return result
}

func testExistential2(x: any Q & D, y: any R & C) -> Exactly<any P & B> {
  let result = test(x, y)
  return result
}

func testExistential3(x: any Q & D, y: any R & F) -> Exactly<any P> {
  let result = test(x, y)
  return result
}

func testExistential4(x: any P & C, y: any S & D) -> Exactly<B> {
  let result = test(x, y)
  return result
}

func testExistential5(x: consuming any ~Copyable, y: any P) -> Exactly<any ~Copyable> {
  let result = test(x, y)
  return result
}

func testExistentialMetatype1(x: any Q.Type, y: any R.Type) -> Exactly<any P.Type> {
  let result = test(x, y)
  return result
}

func testAnyHashable1(x: Int, y: AnyHashable) -> Exactly<AnyHashable> {
  let result = test(x, y)
  return result
}

func testAnyHashable2(x: AnyObject, y: AnyHashable) {
  let result = test(x, y)  // expected-error {{argument type 'AnyHashable' expected to be an instance of a class or class-constrained type}}
}

func testAnyHashable3(x: Int?, y: AnyHashable) -> Exactly<AnyHashable> {
  let result = test(x, y)
  return result
}

func testAnyHashable4(x: AnyHashable?, y: AnyHashable) -> Exactly<AnyHashable?> {
  let result = test(x, y)
  return result
}

func testClassArchetype1<T: A>(t: T, c: B) -> Exactly<A> {
  let result = test(t, c)
  return result
}

func testClassArchetype2<T: B>(t: T, c: A) -> Exactly<A> {
  let result = test(t, c)
  return result
}


func testClassArchetype3<T: A>(t: T, c: A) -> Exactly<A> {
  let result = test(t, c)
  return result
}

func testClassArchetype4<T: B>(t: T, c: B) -> Exactly<B> {
  let result = test(t, c)
  return result
}

func testClassExistential1(c: B, d: any P & A) -> Exactly<A> {
  let result = test(c, d)
  return result
}

func testClassExistential2(c: A, d: any P & B) -> Exactly<A> {
  let result = test(c, d)
  return result
}

func testClassExistential3(c: any P & A, d: any S & B) -> Exactly<A> {
  let result = test(c, d)
  return result
}

// Some more one-off tests for weird code that examples I came across.
//
// If we decide to generally ban joins that contain Any in structural
// position, we can continue to allow them for array element types,
// which would be enough to maintain source compatibility in these
// cases.

func f1(_: String) -> Int { return 0 }
func f2(_: String) -> Bool { return false }

func f3(_: String) -> Int? { return nil }
func f4(_: String) -> Bool? { return nil }

func testFunctionJoin1() -> [(String) -> Any] {
    let result = [f1, f2]
    return result
}

func testFunctionJoin2(s: String) -> [Any] {
    let result = [f1, f2].map { $0(s) }
    return result
}

func testFunctionJoin3() -> [(String) -> Any?] {
    let result = [f3, f4]
    return result
}

func testFunctionJoin4(s: String) -> Bool {
    return [f3, f4].contains { $0(s) != nil }
}
