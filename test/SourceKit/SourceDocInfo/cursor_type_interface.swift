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
// RUN: %sourcekitd-test -req=cursor -pos=13:6 %s -- %s | FileCheck -check-prefix=CHECK-TYPE1 %s
// CHECK-TYPE1: 			public struct A {
// CHECK-TYPE1-NEXT:    public func fa()
// CHECK-TYPE1-NEXT:    public func fea1()
// CHECK-TYPE1-NEXT:    public func fea2()
// CHECK-TYPE1-NEXT:	}
