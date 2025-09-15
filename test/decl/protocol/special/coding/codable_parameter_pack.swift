// RUN: %target-typecheck-verify-swift -target %target-swift-5.9-abi-triple

// We should accept this:

public struct HasPack<each T>: Codable {
  var x: String?
}
