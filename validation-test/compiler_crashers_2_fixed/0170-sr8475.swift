// RUN: not %target-swift-frontend -typecheck %s

func receive() {}
func test() {
  receive { (dat: Container<>) in
    dat
  }
}
