// RUN: %target-swift-frontend -typecheck -verify %s

// This used to crash in SILGen (rightly so).
func sr3210_crash() {
  defer {
    print("\(b)") // expected-error {{cannot capture 'b' before it is declared}}
  }

  return

  let b = 2 // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}} expected-note {{'b' declared here}}
}

func sr3210() {
  defer {
    print("\(b)") // expected-error {{cannot capture 'b' before it is declared}}
  }

  let b = 2 // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}} expected-note {{'b' declared here}}
}
