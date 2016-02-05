// RUN: rm -rf %t && mkdir %t
// RUN: %target-swift-frontend -emit-module-path %t/print_synthesized_extensions.swiftmodule %s
// RUN: %target-swift-ide-test -print-module -synthesize-extension -print-interface -module-to-print=print_synthesized_extensions -I %t -source-filename=%s | FileCheck %s

public protocol P1{
  associatedtype T1
  associatedtype T2
  func f1(t : T1) -> T1
  func f2(t : T2) -> T2
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

// CHECK: /// Synthesized extension from P2
// CHECK: extension S1 where T : P2 {
// CHECK:     public func p2member()
// CHECK: }

// CHECK: /// Synthesized extension from P1
// CHECK: extension S1 where T : P2 {
// CHECK:     public func ef1(t: T)
// CHECK:     public func ef2(t: S2)
// CHECK: }

// CHECK: /// Synthesized extension from P1
// CHECK: extension S1 where T == P2, S2 : P3 {
// CHECK:     public func ef3(t: P2)
// CHECK:     public func ef4(t: P2)
// CHECK: }

// CHECK: /// Synthesized extension from P1
// CHECK: extension S1 where S2 : P3 {
// CHECK:    public func ef5(t: S2)
// CHECK: }
