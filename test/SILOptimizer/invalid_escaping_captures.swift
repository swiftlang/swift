// RUN: %target-swift-frontend -emit-sil %s -verify
// RUN: %target-swift-frontend -emit-sil %s -verify -enable-ownership-stripping-after-serialization

func takesEscaping(_: @escaping () -> ()) {}

func takesNonEscaping(_ fn: () -> ()) { fn() }

func badClosureCaptureInOut1(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  takesEscaping { // expected-error {{escaping closure captures 'inout' parameter 'x'}}
    x += 1 // expected-note {{captured here}}
  }
}

func badClosureCaptureInOut2(x: inout Int, b: Bool) { // expected-note 2{{parameter 'x' is declared 'inout'}}
  takesEscaping(b ? { // expected-error {{escaping closure captures 'inout' parameter 'x'}}
                  x += 1 // expected-note {{captured here}}
                } : { // expected-error {{escaping closure captures 'inout' parameter 'x'}}
                  x -= 1 // expected-note {{captured here}}
                })
}

func badClosureCaptureNoEscape1(y: () -> ()) { // expected-note {{parameter 'y' is implicitly non-escaping}}
  takesEscaping { // expected-error {{escaping closure captures non-escaping parameter 'y'}}
    y() // expected-note {{captured here}}
  }
}

func badClosureCaptureNoEscape2(y: () -> (), b: Bool) { // expected-note 2{{parameter 'y' is implicitly non-escaping}}
  takesEscaping(b ? { // expected-error {{escaping closure captures non-escaping parameter 'y'}}
                  y() // expected-note {{captured here}}
                } : { // expected-error {{escaping closure captures non-escaping parameter 'y'}}
                  y() // expected-note {{captured here}}
                })
}

func badClosureCaptureNoEscape3(y: () -> ()) {
  let yy = (y, y)

  takesEscaping { // expected-error {{escaping closure captures non-escaping value}}
    yy.0() // expected-note {{captured here}}
  }
}

func badClosureCaptureNoEscape4(y: () -> (), z: () -> (), b: Bool) {
  let x = b ? y : z

  takesEscaping { // expected-error {{escaping closure captures non-escaping value}}
    x() // expected-note {{captured here}}
  }
}

func badLocalFunctionCaptureInOut1(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  func local() {
    x += 1 // expected-note {{captured here}}
  }

  takesEscaping(local) // expected-error {{escaping local function captures 'inout' parameter 'x'}}
}

func badLocalFunctionCaptureInOut2(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  func local() {
    x += 1 // expected-note {{captured here}}
  }

  takesEscaping { // expected-error {{escaping closure captures 'inout' parameter 'x'}}
    local() // expected-note {{captured indirectly by this call}}
  }
}

func badLocalFunctionCaptureInOut3(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  func local1() {
    x += 1 // expected-note {{captured here}}
  }

  func local2() {
    local1() // expected-note {{captured indirectly by this call}}
  }

  takesEscaping(local2) // expected-error {{escaping local function captures 'inout' parameter 'x'}}
}

func badLocalFunctionCaptureNoEscape1(y: () -> ()) { // expected-note {{parameter 'y' is implicitly non-escaping}}
  func local() {
    y() // expected-note {{captured here}}
  }

  takesEscaping(local) // expected-error {{escaping local function captures non-escaping parameter 'y'}}
}

func badLocalFunctionCaptureNoEscape2(y: () -> ()) { // expected-note {{parameter 'y' is implicitly non-escaping}}
  func local() {
    y() // expected-note {{captured here}}
  }

  takesEscaping { // expected-error {{escaping closure captures non-escaping parameter 'y'}}
    local() // expected-note {{captured indirectly by this call}}
  }
}

func badLocalFunctionCaptureNoEscape3(y: () -> ()) { // expected-note {{parameter 'y' is implicitly non-escaping}}
  func local1() {
    y() // expected-note {{captured here}}
  }

  func local2() {
    local1() // expected-note {{captured indirectly by this call}}
  }

  takesEscaping(local2) // expected-error {{escaping local function captures non-escaping parameter 'y'}}
}

func badLocalFunctionCaptureNoEscape4(y: () -> ()) { // expected-note {{parameter 'y' is implicitly non-escaping}}
  func local1() {
    takesNonEscaping(y) // expected-note {{captured here}}
  }

  func local2() {
    local1() // expected-note {{captured indirectly by this call}}
  }

  takesEscaping(local2) // expected-error {{escaping local function captures non-escaping parameter 'y'}}
}

// Capturing 'self' produces a different diagnostic.
struct SelfCapture {
  var a: Int
  mutating func badLocalFunctionCaptureInOut() {
    // FIXME: The 'captured here' location chosen here is not ideal, because
    // the original closure is formed in a closure that is nested inside the
    // local function. That's a funny edge case that trips up the heuristics.
    func _foo() {
      a += 1
      takesEscaping { // expected-error {{escaping closure captures mutating 'self' parameter}}
        _foo() // expected-note {{captured here}}
      }
    }
  }
}

// Make sure reabstraction thunks don't cause problems.
func takesEscapingGeneric<T>(_: @escaping () -> T) {}

func testGenericClosureReabstraction(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  takesEscapingGeneric { () -> Int in // expected-error {{escaping closure captures 'inout' parameter 'x'}}
    x += 1 // expected-note {{captured here}}
    return 0
  }
}

func testGenericLocalFunctionReabstraction(x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  func local() -> Int {
    x += 1 // expected-note {{captured here}}
    return 0
  }
  takesEscapingGeneric(local) // expected-error {{escaping local function captures 'inout' parameter 'x'}}
}

// Make sure that withoutActuallyEscaping counts as a safe use.
func goodUseOfNoEscapeClosure(fn: () -> (), fn2: () -> ()) {
  withoutActuallyEscaping(fn) { _fn in
    takesEscaping(_fn)
  }
}

// Some random regression tests
infix operator ~>
protocol Target {}

func ~> <Target, Arg0, Result>(x: inout Target, f: @escaping (_: inout Target, _: Arg0) -> Result) -> (Arg0) -> Result {
  // expected-note@-1 {{parameter 'x' is declared 'inout'}}
  return { f(&x, $0) } // expected-note {{captured here}}
  // expected-error@-1 {{escaping closure captures 'inout' parameter 'x'}}
}

func ~> (x: inout Int, f: @escaping (_: inout Int, _: Target) -> Target) -> (Target) -> Target {
  // expected-note@-1 {{parameter 'x' is declared 'inout'}}
  return { f(&x, $0) } // expected-note {{captured here}}
  // expected-error@-1 {{escaping closure captures 'inout' parameter 'x'}}
}

func addHandler(_: @escaping () -> ()) {}

public struct SelfEscapeFromInit {
  public init() {
    addHandler { self.handler() }
    // expected-error@-1 {{escaping closure captures mutating 'self' parameter}}
    // expected-note@-2 {{captured here}}
  }

  public mutating func handler() {}
}
