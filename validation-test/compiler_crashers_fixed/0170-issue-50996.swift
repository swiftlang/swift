// RUN: not %target-swift-frontend -typecheck %s

// https://github.com/apple/swift/issues/50996

func receive() {}
func test() {
  receive { (dat: Container<>) in
    dat
  }
}
