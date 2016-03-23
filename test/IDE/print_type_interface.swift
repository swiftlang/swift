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

// RUN: %target-swift-ide-test -print-type-interface -pos=13:6 -source-filename %s | FileCheck %s -check-prefix=TYPE1
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

// RUN: %target-swift-ide-test -print-type-interface -pos=34:6 -source-filename %s | FileCheck %s -check-prefix=TYPE2
// RUN: %target-swift-ide-test -print-type-interface -pos=35:6 -source-filename %s | FileCheck %s -check-prefix=TYPE3

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
