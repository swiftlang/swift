// RUN: %target-typecheck-verify-swift

func simple_ret(_ s: String, i: Int) -> String {
  return "A string \"\(s)\" and an int \(i)"
}

func in_context(_ s: String, i: Int) -> String {
  let h = "\(s) = \(i)"
  return h
}

func string_literals(_ s: String, i: Int) -> String {
  return "outer \(s)\(i) close"
}

