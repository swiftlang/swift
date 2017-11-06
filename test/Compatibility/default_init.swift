// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify -swift-version 4

// Default initialization of variables -- totally broken Swift 4 behavior.

class NotInitializableOptionalClass {
  var opt: Optional<Int>
}

struct NotInitializableOptionalStruct { // expected-note {{'init(opt:)' declared here}}
  var opt: Optional<Int>
}

func testBadDefaultInit() {
  _ = NotInitializableOptionalStruct() // expected-error {{missing argument for parameter 'opt' in call}}
  _ = NotInitializableOptionalClass()
}
