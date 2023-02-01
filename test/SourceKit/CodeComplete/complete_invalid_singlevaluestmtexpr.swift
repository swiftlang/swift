
@resultBuilder
struct Builder {
  static func buildBlock<T>(_ components: T...) -> T {
    fatalError()
  }
}

@Builder
func bar() {
  foo { x in
    switch x {
    case let .success(y):
      0
    }
  }
}
// Make sure we don't crash when attempting to solve the fallback.
// RUN: %sourcekitd-test -req=complete -pos=13:15 %s -- %s
