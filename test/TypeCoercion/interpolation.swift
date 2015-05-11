// RUN: %target-parse-verify-swift

func simple_ret(s: String, i: Int) -> String {
  return "A string \"\(s)\" and an int \(i)"
}

func in_context(s: String, i: Int) -> String {
  let h = "\(s) = \(i)"
  return h
}

func string_literals(s: String, i: Int) -> String {
  return "outer \(s)\(i) close"
}

