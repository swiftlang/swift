// RUN: %target-typecheck-verify-swift

// https://github.com/apple/swift/issues/55564

public struct S<T : Codable> : Codable {
  var s: [T]!
}
