// RUN: %target-typecheck-verify-swift -parse-as-library

struct DummyType {}

@available(*, deprecated, renamed: "&-")
func -(x: DummyType, y: DummyType) {}

// We don't warn if a deprecated declaration is referenced from
// within another deprecated declaration.

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

// FIXME: This doesn't work because the file is parsed in script mode.
@available(*, deprecated)
var testDeprecatedReferencingDeprecated3: () = DummyType() - DummyType() // no-warning

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