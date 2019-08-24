// RUN: %target-typecheck-verify-swift -D FOO -D BAR

class A {}

#if FOO
typealias A1 = A
#endif
var a: A = A()
var a1: A1 = A1() // should not result in an error

#if FOO
class C {}
#endif

var c = C() // should not result in an error

class D {
#if FOO
	var x: Int
#endif

	init() {
#if !BAR
		x = "BAR"; // should not result in an error
#else
		x = 1
#endif
	}

#if !BAR
    func overload(a: Int) {}
    func overload(b: String) {} // should not result in an error
#endif
}

var d = D()

#if !FOO 
func f1() -> Bool {
	return true
}
#else 
func f1() -> Int {
#if BAR
	return 1
#else
	return "1" // should not result in an error
#endif
}
#endif

var i: Int = f1()

protocol P1 { 
#if FOO
  func fFOO() -> Int
#endif

#if !BAR
  func fNotBAR() -> Int
#else
  func fBAR() -> Int
#endif
}

class P : P1 {
	func fFOO() -> Int { return 0; }
	func fBAR() -> Int { return 0; }
}

func constants1() -> Int {
#if true
	return 1
#else
	return "1" // should not result in an error
#endif
}

func constants2() -> Int {
#if false
	return "1" // should not result in an error
#elseif ((false || false))
	return "1" // should not result in an error
#else
	return 1
#endif
}
