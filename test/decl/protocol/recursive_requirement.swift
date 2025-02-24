// RUN: %target-typecheck-verify-swift
// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

// -----

protocol Foo {
  associatedtype Bar : Foo
}

struct Ouroboros : Foo {
  typealias Bar = Ouroboros
}

// -----

protocol P {
 associatedtype A : P
}

struct X<T: P> {
}

func f<T : P>(_ z: T) {
 _ = X<T.A>()
}

// -----

protocol PP2 {
  associatedtype A : P2 = Self
}

protocol P2 : PP2 {
  associatedtype A = Self
}

struct X2<T: P2> {
}

struct Y2 : P2 {
  typealias A = Y2
}

func f<T : P2>(_ z: T) {
 _ = X2<T.A>()
}

// -----

protocol P3 {
 associatedtype A: P4 = Self
}

protocol P4 : P3 {}

// CHECK-LABEL: .DeclaredP@
// CHECK-NEXT: Requirement signature: <Self where Self : P4>
protocol DeclaredP : P3, P4 {}

struct Y3 : DeclaredP {
}

struct X3<T:P4> {}

func f2<T:P4>(_ a: T) {
 _ = X3<T.A>()
}

f2(Y3())

// -----

protocol Alpha {
  associatedtype Beta: Gamma
}

protocol Gamma {
  associatedtype Delta: Alpha
}

// CHECK-LABEL: .Epsilon@
// CHECK-NEXT: Generic signature: <T, U where T : Alpha, T == U.[Gamma]Delta, U == T.[Alpha]Beta>
struct Epsilon<T: Alpha,
               U: Gamma>
  where T.Beta == U,
        U.Delta == T {}

// -----

protocol AsExistentialA {
  var delegate : AsExistentialB? { get }
}
protocol AsExistentialB {
  func aMethod(_ object : AsExistentialA)
}

protocol AsExistentialAssocTypeA {
  var delegate : (any AsExistentialAssocTypeB)? { get }
}
protocol AsExistentialAssocTypeB {
  func aMethod(_ object : AsExistentialAssocTypeA)
  associatedtype Bar
}

protocol AsExistentialAssocTypeAgainA {
  var delegate : AsExistentialAssocTypeAgainB? { get }
  associatedtype Bar
}
protocol AsExistentialAssocTypeAgainB {
  func aMethod(_ object : any AsExistentialAssocTypeAgainA)
}

// https://github.com/apple/swift/issues/43164

protocol A {
    associatedtype B1: B
    associatedtype C1: C
    
    mutating func addObserver(_ observer: B1, forProperty: C1)
}

protocol C {
    
}

protocol B {
    associatedtype BA: A
    associatedtype BC: C
    
    func observeChangeOfProperty(_ property: BC, observable: BA)
}
