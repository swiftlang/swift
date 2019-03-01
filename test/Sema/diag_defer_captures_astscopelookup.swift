// REQUIRES: enable-astscope-lookup
// RUN: %target-typecheck-verify-swift

// This used to crash in SILGen (rightly so).
func sr3210_crash() {
  defer {
    print("\(b)") // expected-error {{use of unresolved identifier 'b'}}
  }

  return

  let b = 2 // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
}

func sr3210() {
  defer {
    print("\(b)") // expected-error {{use of unresolved identifier 'b'}}
  }

  let b = 2 // expected-warning {{initialization of immutable value 'b' was never used; consider replacing with assignment to '_' or removing it}}
}
