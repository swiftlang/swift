// RUN: %target-swift-frontend -typecheck %s

func __shared() {}

func __owned() {}

func foo() {
  __shared()
  __owned()
}
