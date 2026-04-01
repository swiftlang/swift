// RUN: %target-swift-emit-silgen %s -verify

enum E : Error {
  case a(Int), b(Int)
}

func bar() throws {}

// Make sure we can correctly emit this without crashing.
func foo() throws {
  do {
    try bar()
  } catch E.a, E.b {
  }
}
