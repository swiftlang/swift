// RUN: not %target-swift-frontend %s -typecheck

func foo() -> [String] {
  let dict: [String: String] = [:]
  return dict.filter({ (_
