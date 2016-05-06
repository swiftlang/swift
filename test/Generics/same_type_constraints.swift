// RUN: %target-parse-verify-swift

protocol Fooable {
  associatedtype Foo

  var foo: Foo { get }
}

protocol Barrable {
  associatedtype Bar: Fooable
  var bar: Bar { get }
}

struct X {}
struct Y: Fooable {
  typealias Foo = X
  var foo: X { return X() }
}
struct Z: Barrable {
  typealias Bar = Y
  var bar: Y { return Y() }
}

protocol TestSameTypeRequirement {
  func foo<F1: Fooable where F1.Foo == X>(_ f: F1)
}
struct SatisfySameTypeRequirement : TestSameTypeRequirement {
  func foo<F2: Fooable where F2.Foo == X>(_ f: F2) {}
}

protocol TestSameTypeAssocTypeRequirement {
  associatedtype Assoc
  func foo<F1: Fooable where F1.Foo == Assoc>(_ f: F1)
}
struct SatisfySameTypeAssocTypeRequirement : TestSameTypeAssocTypeRequirement {
  typealias Assoc = X
  func foo<F2: Fooable where F2.Foo == X>(_ f: F2) {}
}
struct SatisfySameTypeAssocTypeRequirementDependent<T>
  : TestSameTypeAssocTypeRequirement
{
  typealias Assoc = T
  func foo<F3: Fooable where F3.Foo == T>(_ f: F3) {}
}

// Pulled in from old standard library to keep the following test
// (LazySequenceOf) valid.
public struct GeneratorOf<T> : IteratorProtocol, Sequence {

  /// Construct an instance whose `next()` method calls `nextElement`.
  public init(_ nextElement: () -> T?) {
    self._next = nextElement
  }
  
  /// Construct an instance whose `next()` method pulls its results
  /// from `base`.
  public init<I : IteratorProtocol where I.Element == T>(_ base: I) {
    var base = base
    self._next = { base.next() }
  }
  
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Precondition: `next()` has not been applied to a copy of `self`
  /// since the copy was made, and no preceding call to `self.next()`
  /// has returned `nil`.
  public mutating func next() -> T? {
    return _next()
  }

  /// `GeneratorOf<T>` is also a `Sequence`, so it `generate`\ s
  /// a copy of itself
  public func makeIterator() -> GeneratorOf {
    return self
  }
  let _next: () -> T?
}

// rdar://problem/19009056
public struct LazySequenceOf<S : Sequence, A where S.Iterator.Element == A> : Sequence {
  public func makeIterator() -> GeneratorOf<A> { 
    return GeneratorOf<A>({ return nil })
  }
  public subscript(i : A) -> A { return i }
}

public func iterate<A>(_ f : (A) -> A) -> (x : A) -> LazySequenceOf<Iterate<A>, A>? {
  return { x in nil }
}

public final class Iterate<A> : Sequence {
  typealias IteratorProtocol = IterateGenerator<A>
  public func makeIterator() -> IterateGenerator<A> {
    return IterateGenerator<A>()
  }
}

public final class IterateGenerator<A> : IteratorProtocol {
  public func next() -> A? {
    return nil
  }
}

// rdar://problem/18475138
public protocol Observable : class {
    associatedtype Output
    func addObserver(_ obj : (Output) -> Void)
}

public protocol Bindable : class {
    associatedtype Input
    func foo()
}

class SideEffect<In> : Bindable {
  typealias Input = In
  func foo() { }
}

struct Composed<Left: Bindable, Right: Observable where Left.Input == Right.Output> { }

infix operator <- { associativity right precedence 90 }

func <- <
    Right : Observable
    >(lhs:(Right.Output) -> Void, rhs: Right) -> Composed<SideEffect<Right>, Right>?
{
  return nil
}

// rdar://problem/17855378
struct Pair<T, U> {
    typealias Type_ = (T, U)
}

protocol Seq {
  associatedtype Element

  func zip<OtherSeq: Seq, ResultSeq: Seq where ResultSeq.Element == Pair<Element, OtherSeq.Element>.Type_> (_ otherSeq: OtherSeq) -> ResultSeq
}

// rdar://problem/18435371
extension Dictionary {
    func multiSubscript<S : Sequence where S.Iterator.Element == Key>(_ seq: S) -> [Value?] {
        var result = [Value?]()
        for seqElt in seq {
            result.append(self[seqElt])
        }
        return result
    }
}

// rdar://problem/19245317
protocol P {
	associatedtype T: P // expected-error{{type may not reference itself as a requirement}}
}

struct S<A: P> {
	init<Q: P where Q.T == A>(_ q: Q) {}
}

// rdar://problem/19371678
protocol Food { }
class Grass : Food { }

protocol Animal {
    associatedtype EdibleFood:Food
    func eat(_ f:EdibleFood)
}
class Cow : Animal {
    func eat(_ f: Grass) { }
}

struct SpecificAnimal<F:Food> : Animal {
    typealias EdibleFood=F
    let _eat:(f:F) -> ()

    init<A:Animal where A.EdibleFood == F>(_ selfie:A) {
        _eat = { selfie.eat($0) }
    }
    func eat(_ f:F) {
        _eat(f:f)
    }
}

// rdar://problem/18803556
struct Something<T> {
    var items: [T] = []
}

extension Something {
    init<S : Sequence where S.Iterator.Element == T>(_ s: S) {
        for item in s {
            items.append(item)
        }
    }
}

// rdar://problem/18120419
func TTGenWrap<T, I : IteratorProtocol where I.Element == (T,T)>(_ iterator: I)
{
  var iterator = iterator
  _ = iterator.next()
}

func IntIntGenWrap<I : IteratorProtocol where I.Element == (Int,Int)>(_ iterator: I)
{
  var iterator = iterator
  _ = iterator.next()
}

func GGWrap<I1 : IteratorProtocol, I2 : IteratorProtocol where I1.Element == I2.Element>(_ i1: I1, _ i2: I2)
{
  var i1 = i1
  var i2 = i2
  _ = i1.next()
  _ = i2.next()
}

func testSameTypeTuple(_ a: Array<(Int,Int)>, s: ArraySlice<(Int,Int)>) {
  GGWrap(a.makeIterator(), s.makeIterator())
  TTGenWrap(a.makeIterator())
  IntIntGenWrap(s.makeIterator())
}

// rdar://problem/20256475
protocol FooProtocol {
  associatedtype Element

  func getElement() -> Element
}
protocol Bar {
  associatedtype Foo : FooProtocol

  func getFoo() -> Foo

  mutating func extend<
    C : FooProtocol
    where
    C.Element == Foo.Element
  >(_ elements: C)
}

// rdar://problem/21620908
protocol P1 { }

protocol P2Base { }

protocol P2 : P2Base {
  associatedtype Q : P1

  func getQ() -> Q
}

struct XP1<T : P2Base> : P1 {
  func wibble() { }
}

func sameTypeParameterizedConcrete<C : P2 where C.Q == XP1<C>>(_ c: C) {
  c.getQ().wibble()
}

// rdar://problem/21621421
protocol P3 {
  associatedtype AssocP3 : P1
}

protocol P4 {
  associatedtype AssocP4 : P3
}

struct X1 : P1 { }

struct X3 : P3 {
  typealias AssocP3 = X1
}

func foo<C : P4 where C.AssocP4 == X3>(_ c: C) { }

struct X4 : P4 {
  typealias AssocP4 = X3
}

func testFoo(_ x3: X4) {
  foo(x3)
}

// rdar://problem/21625478
struct X6<T> { }

protocol P6 { }

protocol P7 {
  associatedtype AssocP7
}

protocol P8 {
  associatedtype AssocP8 : P7
  associatedtype AssocOther
}

func testP8<C : P8 where C.AssocOther == X6<C.AssocP8.AssocP7>>(_ c: C) { }

// setGenericSignature() was getting called twice here
struct Ghost<T> {}

protocol Timewarp {
  associatedtype Wormhole
}

struct Teleporter<A, B where A : Timewarp, A.Wormhole == Ghost<B>> {}

struct Beam {}

struct EventHorizon : Timewarp {
  typealias Wormhole = Ghost<Beam>
}

func activate<T>(_ t: T) {}

activate(Teleporter<EventHorizon, Beam>())
