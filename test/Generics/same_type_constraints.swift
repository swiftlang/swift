// RUN: %target-parse-verify-swift

protocol Fooable {
  typealias Foo

  var foo: Foo { get }
}

protocol Barrable {
  typealias Bar: Fooable
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
  func foo<F1: Fooable where F1.Foo == X>(f: F1)
}
struct SatisfySameTypeRequirement : TestSameTypeRequirement {
  func foo<F2: Fooable where F2.Foo == X>(f: F2) {}
}

protocol TestSameTypeAssocTypeRequirement {
  typealias Assoc
  func foo<F1: Fooable where F1.Foo == Assoc>(f: F1)
}
struct SatisfySameTypeAssocTypeRequirement : TestSameTypeAssocTypeRequirement {
  typealias Assoc = X
  func foo<F2: Fooable where F2.Foo == X>(f: F2) {}
}
struct SatisfySameTypeAssocTypeRequirementDependent<T>
  : TestSameTypeAssocTypeRequirement
{
  typealias Assoc = T
  func foo<F3: Fooable where F3.Foo == T>(f: F3) {}
}

// Pulled in from old standard library to keep the following test
// (LazySequenceOf) valid.
public struct GeneratorOf<T> : GeneratorType, SequenceType {

  /// Construct an instance whose `next()` method calls `nextElement`.
  public init(_ nextElement: ()->T?) {
    self._next = nextElement
  }
  
  /// Construct an instance whose `next()` method pulls its results
  /// from `base`.
  public init<G: GeneratorType where G.Element == T>(var _ base: G) {
    self._next = { base.next() }
  }
  
  /// Advance to the next element and return it, or `nil` if no next
  /// element exists.
  ///
  /// Requires: `next()` has not been applied to a copy of `self`
  /// since the copy was made, and no preceding call to `self.next()`
  /// has returned `nil`.
  public mutating func next() -> T? {
    return _next()
  }

  /// `GeneratorOf<T>` is also a `SequenceType`, so it `generate`\ s
  /// a copy of itself
  public func generate() -> GeneratorOf {
    return self
  }
  let _next: ()->T?
}

// rdar://problem/19009056
public struct LazySequenceOf<S : SequenceType, A where S.Generator.Element == A> : SequenceType {
  public func generate() -> GeneratorOf<A> { 
    return GeneratorOf<A>({ return nil })
  }
  public subscript(i : A) -> A { return i }
}

public func iterate<A>(f : A -> A)(x : A) -> LazySequenceOf<Iterate<A>, A>? {
  return nil
}

public final class Iterate<A> : SequenceType {
  typealias GeneratorType = IterateGenerator<A>
  public func generate() -> IterateGenerator<A> {
    return IterateGenerator<A>()
  }
}

public final class IterateGenerator<A> : GeneratorType {
  public func next() -> A? {
    return nil
  }
}

// rdar://problem/18475138
public protocol Observable : class {
    typealias Output
    func addObserver(obj : Output->Void)
}

public protocol Bindable : class {
    typealias Input
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
    >(lhs:Right.Output -> Void, rhs: Right) -> Composed<SideEffect<Right>, Right>?
{
  return nil
}

// rdar://problem/17855378
struct Pair<T, U> {
    typealias Type_ = (T, U)
}

protocol Seq {
  typealias Element

  func zip<OtherSeq: Seq, ResultSeq: Seq where ResultSeq.Element == Pair<Element, OtherSeq.Element>.Type_> (otherSeq: OtherSeq) -> ResultSeq
}

// rdar://problem/18435371
extension Dictionary {
    func multiSubscript<S: SequenceType where S.Generator.Element == Key>(seq: S) -> [Value?] {
        var result = [Value?]()
        for seqElt in seq {
            result.append(self[seqElt])
        }
        return result
    }
}

// rdar://problem/19245317
protocol P {
	typealias T: P
}

struct S<A: P> {
	init<Q: P where Q.T == A>(_ q: Q) {}
}

// rdar://problem/19371678
protocol Food { }
class Grass : Food { }

protocol Animal {
    typealias EdibleFood:Food
    func eat(f:EdibleFood)
}
class Cow : Animal {
    func eat(f: Grass) { }
}

struct SpecificAnimal<F:Food> : Animal {
    typealias EdibleFood=F
    let _eat:(f:F)->()

    init<A:Animal where A.EdibleFood == F>(_ selfie:A) {
        _eat = { selfie.eat($0) }
    }
    func eat(f:F) {
        _eat(f:f)
    }
}

// rdar://problem/18803556
struct Something<T> {
    var items: [T] = []
}

extension Something {
    init<S: SequenceType where S.Generator.Element == T>(_ s: S) {
        for item in s {
            items.append(item)
        }
    }
}

// rdar://problem/18120419
func TTGenWrap<T, G: GeneratorType where G.Element == (T,T)>(var gen: G)
{
  gen.next()
}

func IntIntGenWrap<G: GeneratorType where G.Element == (Int,Int)>(var gen: G)
{
  gen.next()
}

func GGWrap<G1: GeneratorType, G2: GeneratorType where G1.Element == G2.Element>(var g1: G1, var _ g2: G2)
{
  g1.next()
  g2.next()
}

func testSameTypeTuple(a: Array<(Int,Int)>, s: ArraySlice<(Int,Int)>) {
  GGWrap(a.generate(), s.generate())
  TTGenWrap(a.generate())
  IntIntGenWrap(s.generate())
}
