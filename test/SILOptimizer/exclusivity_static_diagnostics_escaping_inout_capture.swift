// RUN: %target-swift-frontend -emit-sil -verify -Onone %s -o /dev/null
// RUN: %target-swift-frontend -emit-sil -verify -O %s -o /dev/null

// An escaping closure that captures an inout parameter is rejected by DiagnoseInvalidEscapingCaptures. The capture is
// still an @inout_aliasable address in SIL, like the capture of a promoted box in a closure that is only called.
// DiagnoseStaticExclusivity only follows the closures of promoted boxes, so it doesn't report an overlapping access
// for the closure's access to the parameter during the call that modifies it, a second diagnostic on invalid code.

var saved: (() -> ())?

func modifyAndSave(_ x: inout Int, _ body: @escaping () -> ()) {
  saved = body
}

func test(_ x: inout Int) { // expected-note {{parameter 'x' is declared 'inout'}}
  modifyAndSave(&x) { // expected-error {{escaping closure captures 'inout' parameter 'x'}}
    x += 1 // expected-note {{captured here}}
  }
}
