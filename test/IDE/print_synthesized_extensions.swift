// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions.swiftmodule %s
// RUN: %target-swift-ide-test -print-module -annotate-print -synthesize-extension -print-interface -module-to-print=print_synthesized_extensions -I %t -source-filename=%s > %t.syn.txt
// RUN: FileCheck %s < %t.syn.txt

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

// CHECK: <synthesized>/// Synthesized extension from P2
// CHECK-NEXT: extension <ref:Struct>S1</ref> where T : P2 {
// CHECK-NEXT:     <decl:Func>public func <loc>p2member()</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S1</ref> where T == Int {
// CHECK-NEXT:     <decl:Func>public func <loc>p1IntFunc(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S1</ref> where T : P3 {
// CHECK-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S1</ref> where T : P2 {
// CHECK-NEXT:     <decl:Func>public func <loc>ef1(<decl:Param>t: T</decl>)</loc></decl>
// CHECK-NEXT:     <decl:Func>public func <loc>ef2(<decl:Param>t: <ref:Struct>S2</ref></decl>)</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S1</ref> where T == S9<Int> {
// CHECK-NEXT:     <decl:Func>public func <loc>S9IntFunc()</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S10</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S10</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:Struct>S9</ref><<ref:Struct>Int</ref>></decl>)</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S10</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>S9IntFunc()</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S4</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>p1IntFunc(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S6</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:Struct>S6</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:Struct>S5</ref></decl>)</loc></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S7</ref>.<ref:Struct>S8</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>p3Func(<decl:Param>i: <ref:Struct>Int</ref></decl>)</loc> -> <ref:Struct>Int</ref></decl>
// CHECK-NEXT: }</synthesized>

// CHECK: <synthesized>/// Synthesized extension from P1
// CHECK-NEXT: extension <ref:module>print_synthesized_extensions</ref>.<ref:Struct>S7</ref>.<ref:Struct>S8</ref> {
// CHECK-NEXT:     <decl:Func>public func <loc>ef5(<decl:Param>t: <ref:Struct>S5</ref></decl>)</loc></decl>
// CHECK-NEXT: }</synthesized>
