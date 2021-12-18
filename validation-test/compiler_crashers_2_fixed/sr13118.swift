// RUN: %target-typecheck-verify-swift

public struct S<T : Codable> : Codable {
  var s: [T]!
}
