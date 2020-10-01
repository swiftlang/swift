// RUN: %target-swift-frontend -typecheck -parse-as-library %s -verify -swift-version 4

// Default initialization of variables -- totally broken Swift 4 behavior.

class NotInitializableOptionalClass {
  var opt: Optional<Int>
  var empty: Void
}

struct NotInitializableOptionalStruct {
  var opt: Optional<Int>
  var empty: Void
}

func testBadDefaultInit() {
  _ = NotInitializableOptionalStruct()
  _ = NotInitializableOptionalClass()
}
