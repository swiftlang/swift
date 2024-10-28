// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -target %target-swift-5.9-abi-triple -emit-module-path %t/print_synthesized_extensions.swiftmodule -emit-module-doc -emit-module-doc-path %t/print_synthesized_extensions.swiftdoc %s
// RUN: %target-swift-ide-test -annotate-print -print-module -synthesize-extension -print-interface -no-empty-line-between-members -module-to-print=print_synthesized_extensions -I %t -source-filename=%s -target=%target-swift-5.9-abi-triple > %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK1 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK2 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK3 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK4 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK5 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK6 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK7 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK8 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK9 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK10 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK11 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK12 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK13 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK14 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK15 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK16 < %t.syn.txt
// RUN: %FileCheck %s -check-prefix=CHECK17 < %t.syn.txt

public protocol P1 {
  associatedtype T1
  associatedtype T2
  func f1(t : T1) -> T1
  func f2(t : T2) -> T2
}

public extension P1 where T1 == Int {
  func p1IntFunc(i : Int) -> Int {return 0}
}

public extension P1 where T1 : P3 {
  func p3Func(i : Int) -> Int {return 0}
}

public protocol P2 {
	associatedtype P2T1
}

public extension P2 where P2T1 : P2{
	public func p2member() {}
}

public protocol P3 {}

public extension P1 where T1 : P2 {
  public func ef1(t : T1) {}
  public func ef2(t : T2) {}
}

public extension P1 where T1 == P3, T2 : P3 {
  public func ef3(t : T1) {}
  public func ef4(t : T1) {}
}

public extension P1 where T2 : P3 {
	public func ef5(t : T2) {}
}

public struct S2 {}

public struct S1<T> : P1, P2 {
  public typealias T1 = T
  public typealias T2 = S2
  public typealias P2T1 = T
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}

public struct S3<T> : P1 {
  public typealias T1 = (T, T)
  public typealias T2 = (T, T)
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}

public struct S4<T> : P1  {
  public typealias T1 = Int
  public typealias T2 = Int
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}

public struct S5 : P3 {}

public struct S6<T> : P1  {
  public typealias T1 = S5
  public typealias T2 = S5
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}
public extension S6 {
  public func f3() {}
}
public struct S7 {
  public struct S8 : P1  {
    public typealias T1 = S5
    public typealias T2 = S5
    public func f1(t : T1) -> T1 {
      return t
    }
    public func f2(t : T2) -> T2 {
      return t
    }
  }
}

public extension P1 where T1 == S9<Int> {
  public func S9IntFunc() {}
}

public struct S9<T> : P3 {}

public struct S10 : P1 {
  public typealias T1 = S9<Int>
  public typealias T2 = S9<Int>
  public func f1(t : T1) -> T1 {
    return t
  }
  public func f2(t : T2) -> T2 {
    return t
  }
}

public protocol P4 {}

/// Extension on P4Func1
public extension P4 {
  func P4Func1() {}
}

/// Extension on P4Func2
public extension P4 {
  func P4Func2() {}
}

public struct S11 : P4 {}

public extension S6 {
  public func fromActualExtension() {}
}

public protocol P5 {
  associatedtype T1

  /// This is picked
  func foo1()
}
public extension P5 {

  /// This is not picked
  public func foo1() {}
}

public extension P5 where T1 == Int {
  /// This is picked
  public func foo2() {}
}

public extension P5 {
  /// This is not picked
 public func foo2() {}
}

public extension P5 {

  /// This is not picked
  public func foo3() {}
}

public extension P5 where T1 : Comparable{

  /// This is picked
  public func foo3() {}
}

public extension P5 where T1 : Comparable {

  /// This is picked
  public func foo4() {}
}

public extension P5 where T1 : AnyObject {

  /// This should not crash
  public func foo5() {}
}

public extension P5 {

  /// This is not picked
  public func foo4() {}
}

public struct S12 : P5{
  public typealias T1 = Int
  public func foo1() {}
}

public protocol P6 {
  func foo1()
  func foo2()
}

public extension P6 {
  public func foo1() {}
}

public protocol P7 {
  associatedtype T1
  func f1(t: T1)
}

public extension P7 {
  public func nomergeFunc(t: T1) -> T1 { return t }
  public func f1(t: T1) -> T1 { return t }
}

public struct S13 {}

extension S13 : P5 {
  public typealias T1 = Int
  public func foo1() {}
}

// CHECK1: <synthesized>extension <ref:Struct>S1</ref> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P2</ref> {
// CHECK1-NEXT:     <decl:Func>public func <loc>p2member()</loc></decl>
// CHECK1-NEXT:     <decl:Func>public func <loc>ef1(<decl:Param>t: <ref:GenericTypeParam>T</ref></decl>)</loc></decl>
// CHECK1-NEXT:     <decl:Func>public func <loc>ef2(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S2</ref></decl>)</loc></decl>
// CHECK1-NEXT: }</synthesized>

// CHECK2:  <synthesized>extension <ref:Struct>S1</ref> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P3</ref>  {
// CHECK2-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK2-NEXT: }</synthesized>

// CHECK3:  <synthesized>extension <ref:Struct>S1</ref> where <ref:GenericTypeParam>T</ref> == <ref:Struct>Int</ref>  {
// CHECK3-NEXT:     <decl:Func>public func <loc>p1IntFunc(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK3-NEXT: }</synthesized>

// CHECK4:  <synthesized>extension <ref:Struct>S1</ref> where <ref:GenericTypeParam>T</ref> == <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S9</ref><<ref:Struct>Int</ref>> {
// CHECK4-NEXT:     <decl:Func>public func <loc>S9IntFunc()</loc></decl>
// CHECK4-NEXT: }</synthesized>

// CHECK5:      <decl:Struct>public struct <loc>S10</loc> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P1</ref> {
// CHECK5-NEXT:   <decl:TypeAlias>public typealias <loc>T1</loc> = <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S9</ref><<ref:Struct>Int</ref>></decl>
// CHECK5-NEXT: <decl:TypeAlias>public typealias <loc>T2</loc> = <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S9</ref><<ref:Struct>Int</ref>></decl>
// CHECK5-NEXT: <decl:Func>public func <loc>f1(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S10</ref>.<ref:TypeAlias>T1</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S10</ref>.<ref:TypeAlias>T1</ref></decl>
// CHECK5-NEXT: <decl:Func>public func <loc>f2(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S10</ref>.<ref:TypeAlias>T2</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S10</ref>.<ref:TypeAlias>T2</ref></decl></decl>
// CHECK5-NEXT: <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK5-NEXT: <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S9</ref><<ref:Struct>Int</ref>></decl>)</loc></decl>
// CHECK5-NEXT: <decl:Func>public func <loc>S9IntFunc()</loc></decl>
// CHECK5-NEXT: }</synthesized>

// CHECK6:  <synthesized>/// Extension on P4Func1
// CHECK6-NEXT:  extension <ref:Struct>S11</ref> {
// CHECK6-NEXT:      <decl:Func>public func <loc>P4Func1()</loc></decl>
// CHECK6-NEXT:  }</synthesized>

// CHECK7:  <synthesized>/// Extension on P4Func2
// CHECK7-NEXT:  extension <ref:Struct>S11</ref> {
// CHECK7-NEXT:  <decl:Func>public func <loc>P4Func2()</loc></decl>
// CHECK7-NEXT:  }</synthesized>

// CHECK8:      <decl:Struct>public struct <loc>S4<<decl:GenericTypeParam>T</decl>></loc> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P1</ref> {
// CHECK8-NEXT:   <decl:TypeAlias>public typealias <loc>T1</loc> = <ref:Struct>Int</ref></decl>
// CHECK8-NEXT:   <decl:TypeAlias>public typealias <loc>T2</loc> = <ref:Struct>Int</ref></decl>
// CHECK8-NEXT:   <decl:Func>public func <loc>f1(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S4</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T1</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S4</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T1</ref></decl>
// CHECK8-NEXT:   <decl:Func>public func <loc>f2(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S4</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T2</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S4</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T2</ref></decl></decl>
// CHECK8-NEXT:   <decl:Func>public func <loc>p1IntFunc(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK8-NEXT:   }</synthesized>

// CHECK9:      <decl:Struct>public struct <loc>S6<<decl:GenericTypeParam>T</decl>></loc> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P1</ref> {
// CHECK9-NEXT:    <decl:TypeAlias>public typealias <loc>T1</loc> = <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S5</ref></decl>
// CHECK9-NEXT:    <decl:TypeAlias>public typealias <loc>T2</loc> = <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S5</ref></decl>
// CHECK9-NEXT:    <decl:Func>public func <loc>f1(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S6</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T1</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S6</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T1</ref></decl>

// CHECK9-NEXT:    <decl:Func>public func <loc>f2(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S6</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T2</ref></decl>)</loc> -> <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S6</ref><<ref:GenericTypeParam>T</ref>>.<ref:TypeAlias>T2</ref></decl></decl>
// CHECK9-NEXT:    <decl:Extension><decl:Func>public func <loc>f3()</loc></decl></decl>
// CHECK9-NEXT:    <decl:Extension><decl:Func>public func <loc>fromActualExtension()</loc></decl></decl>
// CHECK9-NEXT:    <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK9-NEXT:    <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S5</ref></decl>)</loc></decl>
// CHECK9-NEXT: }</synthesized>

// CHECK10: <synthesized>extension <ref:Struct>S7</ref>.<ref:Struct>S8</ref> {
// CHECK10-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK10-NEXT:     <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S5</ref></decl>)</loc></decl>
// CHECK10-NEXT: }</synthesized>

// CHECK11:      <decl:Struct>public struct <loc>S12</loc> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P5</ref> {
// CHECK11-NEXT:  <decl:TypeAlias>public typealias <loc>T1</loc> = <ref:Struct>Int</ref></decl>
// CHECK11-NEXT:  <decl:Func>/// This is picked
// CHECK11-NEXT:    public func <loc>foo1()</loc></decl></decl>
// CHECK11-NEXT:  <decl:Func>/// This is picked
// CHECK11-NEXT:    public func <loc>foo2()</loc></decl>
// CHECK11-NEXT:  <decl:Func>/// This is picked
// CHECK11-NEXT:    public func <loc>foo3()</loc></decl>
// CHECK11-NEXT:  <decl:Func>/// This is picked
// CHECK11-NEXT:    public func <loc>foo4()</loc></decl>
// CHECK11-NEXT: }</synthesized>

// CHECK12:       <decl:Protocol>public protocol <loc>P6</loc> {
// CHECK12-NEXT:    <decl:Func(HasDefault)>func <loc>foo1()</loc></decl>
// CHECK12-NEXT:    <decl:Func>func <loc>foo2()</loc></decl>
// CHECK12-NEXT:  }</decl>

// CHECK13:       <decl:Protocol>public protocol <loc>P7</loc> {
// CHECK13-NEXT:   <decl:AssociatedType>associatedtype <loc>T1</loc></decl>
// CHECK13-NEXT:   <decl:Func(HasDefault)>func <loc>f1(<decl:Param>t: <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T1</ref></decl>)</loc></decl>
// CHECK13-NEXT:  }</decl>

// CHECK13:       <decl:Extension>extension <loc><ref:Protocol>P7</ref></loc> {
// CHECK13-NEXT:   <decl:Func>public func <loc>nomergeFunc(<decl:Param>t: <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T1</ref></decl>)</loc> -> <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T1</ref></decl>
// CHECK13-NEXT:   <decl:Func>public func <loc>f1(<decl:Param>t: <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T1</ref></decl>)</loc> -> <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T1</ref></decl>
// CHECK13-NEXT:  }</decl>

// CHECK14: <decl:Struct>public struct <loc>S13</loc> {</decl>
// CHECK14-NEXT: <decl:Func>/// This is picked
// CHECK14-NEXT:     public func <loc>foo2()</loc></decl>
// CHECK14-NEXT: <decl:Func>/// This is picked
// CHECK14-NEXT:     public func <loc>foo3()</loc></decl>
// CHECK14-NEXT: <decl:Func>/// This is picked
// CHECK14-NEXT:     public func <loc>foo4()</loc></decl>
// CHECK14-NEXT: }</synthesized>

// rdar://76868074: Make sure we print the extensions for C.
public class C<T> {}
public class D {}
public class E {}

extension C where T : D {
  public func foo() {}
}

public protocol P8 {
  associatedtype T
}

extension P8 where T : D {
  public func bar() {}
}

extension P8 where T : E {
  public func baz() {}
}

extension C : P8 {}

// CHECK15:      <decl:Class>public class <loc>C<<decl:GenericTypeParam>T</decl>></loc> {
// CHECK15-NEXT: }</decl>

// CHECK15:      <decl:Extension>extension <loc><ref:Class>C</ref></loc> : <ref:module>print_synthesized_extensions</ref>.<ref:Protocol>P8</ref> {
// CHECK15-NEXT: }</decl>

// CHECK15:      <decl:Extension>extension <loc><ref:Class>C</ref></loc> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>D</ref> {
// CHECK15-NEXT:   <decl:Func>public func <loc>foo()</loc></decl></decl>
// CHECK15-NEXT:   <decl:Func>public func <loc>bar()</loc></decl>
// CHECK15-NEXT: }</synthesized>

// CHECK15:      <synthesized>extension <ref:Class>C</ref> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>E</ref> {
// CHECK15-NEXT:   <decl:Func>public func <loc>baz()</loc></decl>
// CHECK15-NEXT: }</synthesized>

// CHECK15:      <decl:Extension>extension <loc><ref:Protocol>P8</ref></loc> where <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>D</ref> {
// CHECK15-NEXT:   <decl:Func>public func <loc>bar()</loc></decl>
// CHECK15-NEXT: }</decl>

// CHECK15:      <decl:Extension>extension <loc><ref:Protocol>P8</ref></loc> where <ref:GenericTypeParam>Self</ref>.<ref:AssociatedType>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>E</ref> {
// CHECK15-NEXT:   <decl:Func>public func <loc>baz()</loc></decl>
// CHECK15-NEXT: }</decl>

// For this class, make sure we mirror the extension P8 where T : D, but not
// the extension P8 where T : E.
public class F<T : D> {}
extension F : P8 {}

// CHECK16:      <decl:Class>public class <loc>F<<decl:GenericTypeParam>T</decl>></loc> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>D</ref> {</decl>
// CHECK16-NEXT:   <decl:Func>public func <loc>bar()</loc></decl>
// CHECK16-NEXT: }</synthesized>

// CHECK16-NOT: <synthesized>extension <ref:Class>F</ref> where <ref:GenericTypeParam>T</ref> : <ref:module>print_synthesized_extensions</ref>.<ref:Class>E</ref> {


// Parameter packs
public protocol P14 {}

extension P14 {
  public func foo<each T: Equatable>(_: repeat each T) {}
}

public struct S14<each T: Equatable> {}

extension S14 : P14 where repeat each T: Hashable {}

// CHECK17:      <synthesized>extension <ref:Struct>S14</ref> {
// CHECK17-NEXT:   <decl:Func>public func <loc>foo<each <ref:GenericTypeParam>T</ref>>(<decl:Param>_: repeat each <ref:GenericTypeParam>T</ref></decl>)</loc> where Pack{repeat each <ref:GenericTypeParam>T</ref>} : <ref:Protocol>Equatable</ref></decl>
// CHECK17-NEXT: }</synthesized>