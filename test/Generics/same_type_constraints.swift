// RUN: %swift -parse %s -verify

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
