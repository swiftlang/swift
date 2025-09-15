// RUN: %target-swift-frontend -emit-sil %s

// These cases are similar to https://github.com/swiftlang/swift/issues/80657,
// but we can avoid hitting the same issue for non-enum members.

struct S {
  let y = 0
  func foo(_ x: Int) {
    let _ = { [self] in
      switch x {
      case y: break
      default: break
      }
    }
  }
}

class C {
  let y = 0
  func foo(_ x: Int) {
    let _ = { [self] in
      switch x {
      case y: break
      default: break
      }
    }
  }
}

enum E {
  case e

  func bar() -> Int {0}

  func foo() {
    _ = { [self] in
      switch 0 {
      case bar():
        break
      default:
        break
      }
    }
  }
}
