// RUN: %empty-directory(%t)
// RUN: not %target-swift-frontend -typecheck %s -index-store-path %t

let k = ""

// Make sure we don't crash here (rdar://120012473).
func foo() {
  _ = {
    let x = switch 1 {
    case k:
      return false
    }
    return true
  }

  for x in [0] where ({
    let x = switch 1 {
    case k:
      return false
    }
    return true
  }()) {}
}

@resultBuilder
struct Builder {
  static func buildBlock<T>(_ components: T...) -> T {
    fatalError()
  }
}

@Builder
func bar() -> Bool {
  let fn = {
    let x = switch 1 {
    case k:
      return false
    }
    return true
  }
  fn()
}

func baz(x: () -> Bool = {
  let x = switch 1 {
  case k:
    return false
  }
  return true
}) {}
