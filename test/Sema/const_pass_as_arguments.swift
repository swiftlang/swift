// RUN: %target-typecheck-verify-swift

public func takeIntConst(_ a: _const Int) {}
public func takeStringConst(_ a: _const String) {}
public func takeDoubleConst(_ a: _const Double) {}
public func takeArrayConst(_ a: _const [String]) {}
public func takeDictConst(_ a: _const [Int: String]) {}

func main(_ i: Int, _ d: Double, _ s: String, arr: [String], dict: [Int: String]) {
	takeIntConst(2)
	takeDoubleConst(3.3)
	takeStringConst("")
	takeArrayConst([""])
	takeDictConst([1: "", 2: "text"])
	
	takeIntConst(i) // expected-error {{expect a compile-time constant literal}}
	takeDoubleConst(d) // expected-error {{expect a compile-time constant literal}}
	takeStringConst("\(d)") // expected-error {{expect a compile-time constant literal}}
	takeStringConst(s) // expected-error {{expect a compile-time constant literal}}
	takeArrayConst(arr) // expected-error {{expect a compile-time constant literal}}
	takeArrayConst([s]) // expected-error {{expect a compile-time constant literal}}
	takeArrayConst(["", s]) // expected-error {{expect a compile-time constant literal}}
	takeDictConst([1: "", 2: s]) // expected-error {{expect a compile-time constant literal}}
	takeDictConst([1: "", i: "text"]) // expected-error {{expect a compile-time constant literal}}
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
	static _const var v: String { get }  // expected-note {{protocol requires property 'v' with type 'String'}}
}

class ConstFanClass1: ConstFan { // expected-error {{type 'ConstFanClass1' does not conform to protocol 'ConstFan'}} expected-note {{add stubs for conformance}}
	static let v: String = "" // expected-note {{candidate operates as non-const, not const as required}}
}

class ConstFanClassCorrect: ConstFan {
	static _const let v: String = ""
}

class ConstFanClassWrong1: ConstFan {

	static _const let v: String // expected-error {{_const let should be initialized with a literal value}}
	// expected-error@-1 {{'static let' declaration requires an initializer expression or an explicitly stated getter}}
	// expected-note@-2 {{add an initializer to silence this error}}
}

class ConstFanClassWrong2: ConstFan {
	static _const let v: String = "\(v)" // expected-error {{_const let should be initialized with a literal value}}
}

class ConstFanClassWrong3: ConstFan {
	static _const var v: String = "" // expected-error {{let is required for a _const variable declaration}}
}

class ConstFanClassWrong4: ConstFan {
	static func giveMeString() -> String { return "" }
	static _const let v: String = giveMeString() // expected-error {{_const let should be initialized with a literal value}}
}

_const let globalConst = 3

class ConstFanClassWrong5 {
	func foo() -> Int {
		_const let localConst = 3
		return globalConst + localConst
	}
}
