// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

/// Used to verify the type of an expression. Use like this:
/// ```
/// var types = SwiftTypePair(typeOf: expr, type2: SwiftType<Int>.self)
/// types.assertTypesAreEqual()
/// ```
struct SwiftType<T> {}
struct SwiftTypePair<T1, T2> {
  init(typeOf: T1, type2: SwiftType<T2>.Type) {}

  mutating func assertTypesAreEqual() where T1 == T2 {}
}

protocol MiscTestsProto {
  associatedtype R : IteratorProtocol, Sequence
  func getR() -> R

  associatedtype Assoc
  subscript() -> Assoc { get }
  var getAssoc: Assoc? { get }
}
do {
  func miscTests(_ arg: any MiscTestsProto) {
    var r = arg.getR()
    do {
      var types = SwiftTypePair(typeOf: r, type2: SwiftType<any Sequence & IteratorProtocol>.self)
      types.assertTypesAreEqual()
    }

    r.makeIterator() // expected-warning {{result of call to 'makeIterator()' is unused}}
    r.next() // expected-warning {{result of call to 'next()' is unused}}
    r.nonexistent() // expected-error {{value of type 'any IteratorProtocol & Sequence' has no member 'nonexistent'}}

    arg[] // expected-warning {{expression of type 'Any' is unused}}
    arg.getAssoc // expected-warning {{expression of type 'Any?' is unused}}
  }
}

///
/// Interactions between opening and parameter packs
///

// Same-shape requirements
protocol HasSameShape {
  func foo<each T, each U>(t: repeat each T, u: repeat each U) -> (repeat (each T, each U))
}

func bar(a: any HasSameShape) -> (Int, String) {
  a.foo(t: 1, u: "hi")
}

// Pack expansions are invariant
struct Pair<X, Y> {}

protocol PackExpansionResult {
  associatedtype A
  func foo<each T>(t: repeat each T) -> (repeat Pair<each T, A>)
}

func packExpansionResult(p: any PackExpansionResult) {
  p.foo(t: 1, "hi")
  // expected-error@-1 {{member 'foo' cannot be used on value of type 'any PackExpansionResult'; consider using a generic constraint instead}}
}

// rdar://135974645 - invalid error: heterogeneous collection literal could only be inferred to '[Any]'
extension StringProtocol {
  func test(target: any StringProtocol, idx: Int) {
    let _ = [target.prefix(idx), target.suffix(idx)] // Ok
  }
}
