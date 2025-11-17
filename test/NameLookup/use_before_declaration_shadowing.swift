// RUN: %target-typecheck-verify-swift
// RUN: %target-typecheck-verify-swift -parse-as-library

func testLocal() {
  // The first `y` here is considered the inner result.
  do {
    let y = ""
    do {
      let _: String = y
      let y = 0
      _ = y
    }
  }
  do {
    let y = ""
    do {
      _ = {
        let _: String = y
      }
      let y = 0
      _ = y
    }
  }
  do {
    let y = ""
    _ = {
      _ = {
        let _: String = y
      }
      let y = 0
      _ = y
    }
  }
  do {
    let y = ""
    func bar() {
      _ = {
        let _: String = y
      }
      let y = 0
      _ = y
    }
  }
}

let topLevelString = ""

func testTopLevel() {
  // Here 'topLevelString' is now an outer result.
  do {
    let _: String = topLevelString
    let topLevelString = 0
    _ = topLevelString
  }
  do {
    _ = {
      let _: String = topLevelString // expected-error {{use of local variable 'topLevelString' before its declaration}}
    }
    let topLevelString = 0 // expected-note {{'topLevelString' declared here}}
  }
  _ = {
    _ = {
      let _: String = topLevelString // expected-error {{use of local variable 'topLevelString' before its declaration}}
    }
    let topLevelString = 0 // expected-note {{'topLevelString' declared here}}
  }
  func bar() {
    _ = {
      let _: String = topLevelString // expected-error {{use of local variable 'topLevelString' before its declaration}}
    }
    let topLevelString = 0 // expected-note {{'topLevelString' declared here}}
  }
}
