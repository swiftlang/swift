// RUN: %target-parse-verify-swift

struct Blob {}

func withBlob(block: (Blob) -> ()) {}

protocol Binding {}
extension Int: Binding {}
extension Double: Binding {}
extension String: Binding {}
extension Blob: Binding {}

struct Stmt {
  @discardableResult
  func bind(_ values: Binding?...) -> Stmt {
    return self
  }

  @discardableResult
  func bind(_ values: [Binding?]) -> Stmt {
    return self
  }

  @discardableResult
  func bind(_ values: [String: Binding?]) -> Stmt {
    return self
  }
}

let stmt = Stmt()
withBlob { stmt.bind(1, 2.0, "3", $0) }
withBlob { stmt.bind([1, 2.0, "3", $0]) }
withBlob { stmt.bind(["1": 1, "2": 2.0, "3": "3", "4": $0]) }

// <rdar://problem/19840785>
// We shouldn't crash on the call to 'a.dispatch' below.
class A {
	func dispatch(_ f : () -> Void) {
		f()
	}
}

class C {
	var prop = 0
	var a = A()

	func act() {
		a.dispatch({() -> Void in
                  self.prop // expected-warning {{expression of type 'Int' is unused}}
                })
	}
}
