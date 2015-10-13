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
// TYPE1:       public struct A {
// TYPE1-NEXT:    public func fa()
// TYPE1-NEXT:    public func fea1()
// TYPE1-NEXT:    public func fea2()
// TYPE1-NEXT:  }
