// RUN: %target-typecheck-verify-swift

// We don't warn if a deprecated declaration is referenced from
// within another deprecated declaration.

struct DummyType {}

@available(*, deprecated, renamed: "&-")
func -(x: DummyType, y: DummyType) {}

@available(*, deprecated)
func testDeprecatedReferencingDeprecated1(x: DummyType, y: DummyType) {
  x - y // no-warning
}

@available(*, deprecated)
var testDeprecatedReferencingDeprecated2: () {
  let x = DummyType()
  let y = DummyType()
  x - y // no-warning
}

@available(*, deprecated)
var testDeprecatedReferencingDeprecated3: () = DummyType() - DummyType()

struct HasDeprecatedMembers {
  @available(*, deprecated)
  func testDeprecatedReferencingDeprecated1(x: DummyType, y: DummyType) {
    x - y // no-warning
  }

  @available(*, deprecated)
  var testDeprecatedReferencingDeprecated2: () {
    let x = DummyType()
    let y = DummyType()
    x - y // no-warning
  }

  @available(*, deprecated)
  var testDeprecatedReferencingDeprecated3: () = DummyType() - DummyType() // no-warning
}
