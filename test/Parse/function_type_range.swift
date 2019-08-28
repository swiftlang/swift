// Ensure that source range for the type does not go past the end of the buffer.
// RUN: %target-swift-frontend -typecheck %s -dump-ast
typealias Alias =


() -> ()