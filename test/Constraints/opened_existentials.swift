// RUN: %target-typecheck-verify-swift -enable-experimental-opened-existential-types -enable-parametrized-protocol-types -enable-experimental-opaque-parameters

protocol Q { }

protocol P {
  associatedtype A: Q
}

extension Int: P {
  typealias A = Double
}

extension Array: P where Element: P {
  typealias A = String
}

extension Double: Q { }
extension String: Q { }

func acceptGeneric<T: P>(_: T) -> T.A? { nil }
func acceptCollection<C: Collection>(_ c: C) -> C.Element { c.first! }

// --- Simple opening of existential values
func testSimpleExistentialOpening(p: any P, pq: any P & Q, c: any Collection) {
  let pa = acceptGeneric(p)
  let _: Int = pa // expected-error{{cannot convert value of type 'Q?' to specified type 'Int'}}

  let pqa = acceptGeneric(pq)
  let _: Int = pqa  // expected-error{{cannot convert value of type 'Q?' to specified type 'Int'}}

  let element = acceptCollection(c) 
  let _: Int = element // expected-error{{cannot convert value of type 'Any' to specified type 'Int'}}
}

// --- Requirements on nested types
protocol CollectionOfPs: Collection where Self.Element: P { }

func takeCollectionOfPs<C: Collection>(_: C) -> C.Element.A?    
  where C.Element: P
{
  nil
}

func testCollectionOfPs(cp: any CollectionOfPs) {
  let e = takeCollectionOfPs(cp)
  let _: Int = e // expected-error{{cannot convert value of type 'Q?' to specified type 'Int'}}
}

// --- Multiple opened existentials in the same expression
func takeTwoGenerics<T1: P, T2: P>(_ a: T1, _ b: T2) -> (T1, T2) { (a, b) }

extension P {
  func combineThePs<T: P & Q>(_ other: T) -> (A, T.A)? { nil }
}

func testMultipleOpened(a: any P, b: any P & Q) {
  let r1 = takeTwoGenerics(a, b)
  let _: Int = r1  // expected-error{{cannot convert value of type '(P, P & Q)' to specified type 'Int'}}

  let r2 = a.combineThePs(b)
  let _: Int = r2  // expected-error{{cannot convert value of type '(Q, Q)?' to specified type 'Int'}}  
}

// --- With primary associated types and opaque parameter types
protocol CollectionOf: Collection {
  @_primaryAssociatedType associatedtype Element
}

extension Array: CollectionOf { }
extension Set: CollectionOf { }

func reverseIt<T>(_ c: some CollectionOf<T>) -> some CollectionOf<T> {
  return c.reversed()
}

func useReverseIt(_ c: any CollectionOf) {
  let c = reverseIt(c)
  let _: Int = c // expected-error{{cannot convert value of type 'CollectionOf' to specified type 'Int'}}
}

/// --- Opening existentials when returning opaque types.
extension P {
  func getQ() -> some Q {
    let a: A? = nil
    return a!
  }

  func getCollectionOf() -> some CollectionOf<A> {
    return [] as [A]
  }
}

func testReturningOpaqueTypes(p: any P) {
  let q = p.getQ()
  let _: Int = q  // expected-error{{cannot convert value of type 'Q' to specified type 'Int'}}

  p.getCollectionOf() // expected-error{{member 'getCollectionOf' cannot be used on value of protocol type 'P'; use a generic constraint instead}}
}
