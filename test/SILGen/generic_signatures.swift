// RUN: %target-swift-emit-silgen %s | %FileCheck %s

protocol P {
  associatedtype Assoc
}

protocol Q {
  associatedtype Assoc1
  associatedtype Assoc2
}

struct G<T> {}
class C {}

func a<T>(x: T) {}
func b<T: P>(x: G<T>, y: T.Assoc) {}
func c<T>(x: T, y: T.Assoc) where T: P {}
func d<T: P, U: P & Q>(x: T, y: U) {}
func e<T, U>(x: T, y: U) where T: P, U: P, U: Q {}
// FIXME: Same-type constraints expose a typechecker bug.
// <rdar://problem/15730168>
func f<T: Q>(x: T) where T.Assoc1 == T.Assoc2 {}
func g<T>(x: T) where T: Q, T.Assoc1 == T.Assoc2 {}
func h<T: P, U>(x: T) where T.Assoc == U {}
func i<T: P>(x: T) where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2 {}
func j<T: C>(_: T) {}
func k<T>(_: T) where T: C {}
func l<T: C>(_: T) where T: P {}
func m<T: P>(_: T) where T.Assoc: C {}

struct Foo<V> {
  func z() {}

  func a<T>(x: T) {}
  func b<T: P>(x: G<T>, y: T.Assoc) {}
  func c<T>(x: T, y: T.Assoc) where T: P {}
  func d<T: P, U: P & Q>(x: T, y: U) {}
  func e<T, U>(x: T, y: U) where T: P, U: P, U: Q {}
  func f<T: Q>(x: T) where T.Assoc1 == T.Assoc2 {}
  func g<T>(x: T) where T: Q, T.Assoc1 == T.Assoc2 {}
  func h<T: P, U>(x: T) where T.Assoc == U {}
  func i<T: P>(x: T) where T.Assoc: Q, T.Assoc.Assoc1 == T.Assoc.Assoc2 {}
  func j<T: C>(_: T) {}
  func k<T>(_: T) where T: C {}
  func l<T: C>(_: T) where T: P {}
  func m<T: P>(_: T) where T.Assoc: C {}
}

// Test that we handle interface type lowering when accessing a dependent
// member of a dependent member that substitutes to a type parameter.
// <rdar://problem/16257259>
protocol Fooable {
  associatedtype Foo
}

protocol Barrable {
  associatedtype Bar: Fooable

  func bar(_: Bar) -> Bar.Foo
}

struct FooBar<T: Fooable>: Barrable {
  typealias Bar = T

  func bar(_ x: T) -> T.Foo { }
}


// Test that associated types can be constrained to concrete types

func concreteJungle<T>(_: T) -> T.Foo where T : Fooable, T.Foo == C {
  return C()
}

func concreteJungle<T>(_: T, t: T.Foo) -> C where T : Fooable, T.Foo == C {
  let c: C = t
  return c
}

func concreteJungle<T>(_: T, f: @escaping (T.Foo) -> C) -> T.Foo where T : Fooable, T.Foo == C {
  let ff: (C) -> T.Foo = f
  return ff(C())
}

protocol Whereable {
  associatedtype Assoc
  associatedtype Bssoc: Whereable
}
extension Whereable {
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures9WhereablePAAE19staticExtensionFunc3arg7ElementSTQz8IteratorSTQz_tSTRzrlFZ : $@convention(method) <Self where Self : Sequence, Self : Whereable> (@in_guaranteed Self.Iterator, @thick Self.Type) -> @out Self.Element
  static func staticExtensionFunc(arg: Self.Iterator) -> Self.Element
    where Self: Sequence {
      fatalError()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures9WhereablePAAE13extensionFuncyy5BssocQz5AssocRtzrlF : $@convention(method) <Self where Self : Whereable, Self.Assoc == Self.Bssoc> (@in_guaranteed Self) -> ()
  func extensionFunc() where Assoc == Bssoc { }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures9WhereablePAAE5AssocQzycAabERQAD_5BssocQZAFRtzrluig : $@convention(method) <Self where Self : Whereable, Self.Assoc : Whereable, Self.Bssoc == Self.Assoc.Bssoc> (@in_guaranteed Self) -> @out Self.Assoc
  subscript() -> Assoc
    where Assoc: Whereable, Bssoc == Assoc.Bssoc {
      fatalError()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures9WhereablePAAE5AssocQzycAabERQ5Bssoc_ADQZAERSrluig : $@convention(method) <Self where Self : Whereable, Self.Assoc : Whereable, Self.Assoc == Self.Bssoc.Assoc> (@in_guaranteed Self) -> @out Self.Assoc
  subscript() -> Assoc
    where Assoc: Whereable, Assoc == Bssoc.Assoc {
      fatalError()
  }
}

struct W1 {}
struct W2 {}

class Class<T> {
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC9classFuncyyAA9WhereableRz5AssocQzRszlFZ : $@convention(method) <T where T : Whereable, T == T.Assoc> (@thick Class<T>.Type) -> ()
  class func classFunc() where T: Whereable, T.Assoc == T { }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC5func1yyAA7FooableRzlF : $@convention(method) <T where T : Fooable> (@guaranteed Class<T>) -> ()
  func func1() where T: Fooable { }
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC5func2yyAA2W1VRszlF : $@convention(method) (@guaranteed Class<W1>) -> ()
  func func2() where T == W1 { }
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC5func2yyAA2W2VRszlF : $@convention(method) (@guaranteed Class<W2>) -> ()
  func func2() where T == W2 { }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC5AssocQzycAA9WhereableRzluig : $@convention(method) <T where T : Whereable> (@guaranteed Class<T>) -> @out T.Assoc
  subscript() -> T.Assoc where T: Whereable {
    fatalError()
  }

  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC06NestedC0CAEyx_Gycfc : $@convention(method) <T where T : Fooable> (@owned Class<T>.NestedClass) -> @owned Class<T>.NestedClass
  class NestedClass where T: Fooable { }
}

extension Class where T: Whereable {
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassCA2A9WhereableRzlE13extensionFuncyyAA7FooableRzrlF : $@convention(method) <T where T : Fooable, T : Whereable> (@guaranteed Class<T>) -> ()
  func extensionFunc() where T: Fooable { }
}
extension Class.NestedClass {
  // CHECK-LABEL: sil hidden [ossa] @$s18generic_signatures5ClassC06NestedC0C3foo3argyx_tAA9WhereableRz3FooAA7FooablePQz5AssocAaHPRtzrlF : $@convention(method) <T where T : Fooable, T : Whereable, T.Assoc == T.Foo> (@in_guaranteed T, @guaranteed Class<T>.NestedClass) -> ()
  func foo(arg: T) where T: Whereable, T.Foo == T.Assoc { }
}
