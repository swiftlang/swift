// RUN: %target-typecheck-verify-swift

public func takeIntConst(_ a: _const Int) {}
public func takeStringConst(_ a: _const String) {}
public func takeDoubleConst(_ a: _const Double) {}

func main(_ i: Int, _ d: Double, _ s: String) {
	takeIntConst(2)
	takeDoubleConst(3.3)
	takeStringConst("")
	
	takeIntConst(i) // expected-error {{expect a compile-time constant literal}}
	takeDoubleConst(d) // expected-error {{expect a compile-time constant literal}}
	takeStringConst("\(d)") // expected-error {{expect a compile-time constant literal}}
	takeStringConst(s) // expected-error {{expect a compile-time constant literal}}
}

public struct Utils {
	public func takeIntConst(_ a: _const Int) {}
	public func takeStringConst(_ a: _const String) {}
	public func takeDoubleConst(_ a: _const Double) {}
}

func main_member(_ u: Utils, _ i: Int, _ d: Double, _ s: String) {
	u.takeIntConst(2)
	u.takeDoubleConst(3.3)
	u.takeStringConst("")

	u.takeIntConst(i) // expected-error {{expect a compile-time constant literal}}
	u.takeDoubleConst(d) // expected-error {{expect a compile-time constant literal}}
	u.takeStringConst("\(d)") // expected-error {{expect a compile-time constant literal}}
	u.takeStringConst(s) // expected-error {{expect a compile-time constant literal}}
}

protocol ConstFan {
	static _const var v: String { get }  // expected-note {{protocol requires property 'v' with type 'String'; do you want to add a stub?}}
}

class ConstFanClass1: ConstFan { // expected-error {{type 'ConstFanClass1' does not conform to protocol 'ConstFan'}}
	static var v: String = "" // expected-note {{candidate operates as non-const, not const as required}}
}

class ConstFanClassCorrect: ConstFan {
	static _const var v: String = ""
}
