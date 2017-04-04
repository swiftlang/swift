// XFAIL: linux
// RUN: %sourcekitd-test -req=interface-gen -usr _TtGSaSi_ %s -- %s | %FileCheck -check-prefix=CHECK1 %s
// RUN: %sourcekitd-test -req=interface-gen -usr _TtGSaSS_ %s -- %s | %FileCheck -check-prefix=CHECK2 %s
// RUN: %sourcekitd-test -req=interface-gen -usr _TtV14gen_swift_type1A %s -- %s | %FileCheck -check-prefix=CHECK3 %s
// RUN: %sourcekitd-test -req=interface-gen -usr _TtGSaV14gen_swift_type1A_ %s -- %s | %FileCheck -check-prefix=CHECK4 %s
// RUN: %sourcekitd-test -req=interface-gen -usr _TtGC14gen_swift_type1DCS_2T1_ %s -- %s | %FileCheck -check-prefix=CHECK5 %s
// RUN: %sourcekitd-test -req=interface-gen -usr _TtGC14gen_swift_type1DSi_ %s -- %s | %FileCheck -check-prefix=CHECK6 %s

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

struct S1 {
  func f1(a : [A]) {
    _ = a.count
  }
}
// CHECK1: public struct Array<Int> : RandomAccessCollection, MutableCollection {

// CHECK2: public struct Array<String> : RandomAccessCollection, MutableCollection {

// CHECK3: public struct A
// CHECK3: public func fa()
// CHECK3: public func fea1()
// CHECK3: public func fea2()

// CHECK4: public struct Array<A> : RandomAccessCollection, MutableCollection {

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

extension D where T : P1 {
  public func conditionalFunc1() {}
  public func conditionalFunc2(t : T) -> T {return t}
}

extension D {
  public func unconditionalFunc1(){}
  public func unconditionalFunc2(t : T) -> T {return t}
}

// CHECK5: public class D<T1> {
// CHECK5: public func foo()
// CHECK5: public func conditionalFunc1()
// CHECK5: public func conditionalFunc2(t: T1) -> T1
// CHECK5: public func unconditionalFunc1()
// CHECK5: public func unconditionalFunc2(t: T1) -> T1

// CHECK6: public class D<Int> {
// CHECK6: public func foo()
// CHECK6: public func unconditionalFunc1()
// CHECK6: public func unconditionalFunc2(t: Int) -> Int
