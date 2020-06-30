// RUN: %target-typecheck-verify-swift %s

public struct S<T : Codable> : Codable {
  var s: [T]!
}
