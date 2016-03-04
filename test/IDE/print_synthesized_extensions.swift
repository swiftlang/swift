// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions.swiftmodule %s
// RUN: %target-swift-ide-test -print-module -annotate-print -synthesize-extension -print-interface -module-to-print=print_synthesized_extensions -I %t -source-filename=%s > %t.syn.txt
// RUN: FileCheck %s -check-prefix=CHECK-SYN1 < %t.syn.txt
// RUN: FileCheck %s -check-prefix=CHECK-SYN2 < %t.syn.txt
// RUN: FileCheck %s -check-prefix=CHECK-SYN3 < %t.syn.txt
// RUN: FileCheck %s -check-prefix=CHECK-SYN4 < %t.syn.txt

public protocol P1{
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

public extension P1 where T1 == P2, T2 : P3 {
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

// CHECK-SYN1: <synthesized>/// Synthesized extension from P1
// CHECK-SYN1: extension S1 where T : P2 {
// CHECK-SYN1:     <decl:Func>public func <loc>ef1(<decl:Param>t: T</decl>)</loc></decl>
// CHECK-SYN1:     <decl:Func>public func <loc>ef2(<decl:Param>t: <ref:Struct>S2</ref></decl>)</loc></decl>
// CHECK-SYN1: }</synthesized>

// CHECK-SYN2: <synthesized>/// Synthesized extension from P2
// CHECK-SYN2: extension S1 where T : P2 {
// CHECK-SYN2:     <decl:Func>public func <loc>p2member()</loc></decl>
// CHECK-SYN2: }</synthesized>

// No applicable extensions for S2
// CHECK-SYN1-NOT: extension S2 where

// No applicable extensions for S3
// CHECK-SYN1-NOT: extension S3 where

// CHECK-SYN3: <synthesized>/// Synthesized extension from P1
// CHECK-SYN3: extension S4 {
// CHECK-SYN3:   <decl:Func>public func <loc>p1IntFunc(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-SYN3: }</synthesized>

// CHECK-SYN4: <synthesized>/// Synthesized extension from P1
// CHECK-SYN4: extension S6 {
// CHECK-SYN4:   <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-SYN4: }</synthesized>
