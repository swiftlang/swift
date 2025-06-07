

public struct A {
	public func fa() {}
}
extension A {
	public func fea1() {}
}
extension A {
	public func fea2() {}
}
class C1 {
	func f1() {
		var abcd : A
    abcd.fa()
		var intarr : [Int]
		intarr.append(1)
	}
}

// RUN: %target-swift-ide-test -print-type-interface -pos=15:6 -source-filename %s | %FileCheck %s -check-prefix=TYPE1
// RUN: %target-swift-ide-test -print-type-interface -usr=_TtV20print_type_interface1A -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE1
// TYPE1:  public struct A {
// TYPE1:    public func fa()
// TYPE1:    public func fea1()
// TYPE1:    public func fea2()
// TYPE1:  }

public protocol P1 { }
public class T1 : P1 { }
public class D<T> { public func foo() {}}

class C2 {
  func f() {
    let D1 = D<T1>()
    let D2 = D<Int>()
    D1.foo()
    D2.foo()
  }
}

// RUN: %target-swift-ide-test -print-type-interface -pos=37:6 -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE2
// RUN: %target-swift-ide-test -print-type-interface -usr=_TtGC20print_type_interface1DCS_2T1_ -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE2
// RUN: %target-swift-ide-test -print-type-interface -pos=38:6 -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE3
// RUN: %target-swift-ide-test -print-type-interface -usr=_TtGC20print_type_interface1DSi_ -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE3

extension D where T : P1 {
  public func conditionalFunc1() {}
  public func conditionalFunc2(t : T) -> T {return t}
}

extension D {
  public func unconditionalFunc1(){}
  public func unconditionalFunc2(t : T) -> T {return t}
}

// TYPE2: public class D<T1> {
// TYPE2:    public func foo()
// TYPE2:    public func conditionalFunc1()
// TYPE2:    public func conditionalFunc2(t: T1) -> T1
// TYPE2:    public func unconditionalFunc1()
// TYPE2:    public func unconditionalFunc2(t: T1) -> T1
// TYPE2: }

// TYPE3: public class D<Int> {
// TYPE3:   public func foo()
// TYPE3:   public func unconditionalFunc1()
// TYPE3:   public func unconditionalFunc2(t: Int) -> Int
// TYPE3: }

// RUN: %target-swift-ide-test -print-type-interface -usr=_TtGSaSi_ -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE4
// TYPE4-DAG: public typealias Index = Int
// TYPE4-DAG: public func min() -> Int?
// TYPE4-DAG: public mutating func insert<C>(contentsOf newElements: C, at i: Int)
// TYPE4-DAG: public mutating func removeFirst(_ k: Int)
// TYPE4-DAG: public func makeIterator() -> IndexingIterator<Array<Int>>
// TYPE4-NOT: public func joined

// RUN: %target-swift-ide-test -print-type-interface -usr=_TtGSaSS_ -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE5
// TYPE5-DAG: public func prefix(_ maxLength: Int) -> ArraySlice<String>
// TYPE5-DAG: public func suffix(_ maxLength: Int) -> ArraySlice<String>
// TYPE5-DAG: public func split(separator: String, maxSplits: Int = Int.max, omittingEmptySubsequences: Bool = true) -> [ArraySlice<String>]
// TYPE5-DAG: public func formIndex(_ i: inout Int, offsetBy distance: Int)
// TYPE5-DAG: public func distance(from start: Int, to end: Int) -> Int
// TYPE5-DAG: public func joined(separator: String = "") -> String

extension Array {
  public struct Inner {}
}

public protocol P2 {}

extension Array.Inner where Element: P2 {
  public func innerFoo() {}
}

extension Int: P2 {}

// Print interface for Array<Int>.Inner
// RUN: %target-swift-ide-test -print-type-interface -usr='$sSa20print_type_interfaceE5InnerVySi_GD' -module-name print_type_interface -source-filename %s | %FileCheck %s -check-prefix=TYPE6

// TYPE6-LABEL: public struct Inner {
// TYPE6:   public func innerFoo()
// TYPE6: }
