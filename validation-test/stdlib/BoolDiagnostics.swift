// RUN: %target-typecheck-verify-swift

func CVarArgs_withBool() {
  func varArgFunc(_ x: Bool, _ args: CVarArg...) { }
  let x = false
  varArgFunc(x, x)
}
