// RUN: %target-swift-frontend -typecheck -verify %s

func CVarArgs_withBool() {
  func varArgFunc(_ x: Bool, _ args: CVarArg...) { }
  let x = false
  varArgFunc(x, x)
}
