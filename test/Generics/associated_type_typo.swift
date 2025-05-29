// RUN: %target-typecheck-verify-swift

// RUN: not %target-swift-frontend -typecheck -debug-generic-signatures %s > %t.dump 2>&1 
// RUN: %FileCheck -check-prefix CHECK-GENERIC %s < %t.dump

protocol P1 {
  associatedtype Assoc
}

protocol P2 {
  associatedtype AssocP2 : P1
}

protocol P3 { }
protocol P4 { }

// expected-error@+1{{'T' does not have a member type named 'assoc'; did you mean 'Assoc'?}}
func typoAssoc1<T : P1>(x: T.assoc, _: T) { } 

// expected-error@+2{{'T' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{53-58=Assoc}}
// expected-error@+1{{'U' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{64-69=Assoc}}
func typoAssoc2<T : P1, U : P1>(_: T, _: U) where T.assoc == U.assoc {}

// CHECK-GENERIC-LABEL: .typoAssoc2
// CHECK-GENERIC: Generic signature: <T, U where T : P1, U : P1>

// expected-error@+3{{'T.AssocP2' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{42-47=Assoc}}
// expected-error@+2{{'U.AssocP2' does not have a member type named 'assoc'; did you mean 'Assoc'?}}{{19-24=Assoc}}
func typoAssoc3<T : P2, U : P2>(t: T, u: U)
  where U.AssocP2.assoc : P3,  T.AssocP2.assoc : P4,
        T.AssocP2 == U.AssocP2 {}

// expected-error@+2{{'T.AssocP2' does not have a member type named 'assoc'; did you mean 'Assoc'?}}
// expected-error@+1{{'T' does not have a member type named 'Assocp2'; did you mean 'AssocP2'?}}{{39-46=AssocP2}}
func typoAssoc4<T : P2>(_: T) where T.Assocp2.assoc : P3 {}

// <rdar://problem/19620340>

func typoFunc1<T : P1>(x: TypoType) { // expected-error{{cannot find type 'TypoType' in scope}}
  let _: (T.Assoc) -> () = { let _ = $0 }
}

func typoFunc2<T : P1>(x: TypoType, y: T) { // expected-error{{cannot find type 'TypoType' in scope}}
  let _: (T.Assoc) -> () = { let _ = $0 }
}

func typoFunc3<T : P1>(x: TypoType, y: (T.Assoc) -> ()) { // expected-error{{cannot find type 'TypoType' in scope}}
}

// rdar://problem/29261689
typealias Element_<S: Sequence> = S.Iterator.Element

public protocol _Indexable1 {
  associatedtype Slice // expected-note{{declared here}}
}
public protocol Indexable : _Indexable1 {
  associatedtype Slice : _Indexable1 // expected-warning{{redeclaration of associated type 'Slice'}}
}

protocol Pattern {
  associatedtype Element : Equatable

  // FIXME: This works for all of the wrong reasons, but it is correct that
  // it works.
  // CHECK-GENERIC-LABEL: .matched(atStartOf:)@
  // CHECK-GENERIC-NEXT: Generic signature: <Self, C where Self : Pattern, C : Sequence, C : Indexable, Self.[Pattern]Element == C.[Sequence]Element, C.[Sequence]Element == C.[_Indexable1]Slice.[Sequence]Element, C.[_Indexable1]Slice : Sequence>
  func matched<C: Indexable>(atStartOf c: C)
  where Element_<C> == Element
  , Element_<C.Slice> == Element
}

class C {
  typealias SomeElement = Int
}

func typoSuperclass1<T : C>(_: T) where T.someElement: P3 { }
// expected-error@-1{{'T' does not have a member type named 'someElement'; did you mean 'SomeElement'?}}{{43-54=SomeElement}}

class D {
  typealias AElement = Int // expected-note{{did you mean 'AElement'?}}
  typealias BElement = Int // expected-note{{did you mean 'BElement'?}}
}

func typoSuperclass2<T : D>(_: T, _: T.Element) { } // expected-error{{'Element' is not a member type of type 'T'}}
