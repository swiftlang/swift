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
      let _: String = topLevelString
    }
    let topLevelString = 0
    _ = topLevelString
  }
  _ = {
    _ = {
      let _: String = topLevelString
    }
    let topLevelString = 0
    _ = topLevelString
  }
  func bar() {
    _ = {
      let _: String = topLevelString
    }
    let topLevelString = 0
    _ = topLevelString
  }
}

struct TestLocalPropertyShadowing {
  var str: String

  func foo() {
    { _ = str }()
    let str = str
    _ = str
  }
  func bar() {
    let str = { str }
    _ = str
  }
  func baz() {
    let str = str
    _ = str
  }
}
