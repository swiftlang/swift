// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/71040

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ components: T...) -> T {
    components.first!
  }
}

struct S<T> {
  init(_ fn: () -> T) {}
}

func foo<T>(@Builder _ fn: () -> T) {}

@Builder
func bar() {
  foo {
    S<Int> {
      let x = if .random() { 0 } else { 1 }
      return x
    }
  }
}
