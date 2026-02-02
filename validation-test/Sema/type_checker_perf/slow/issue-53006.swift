// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// https://github.com/swiftlang/swift/issues/53006

func extractBits(from word: UInt64, offset: Int, numBits: Int) -> UInt {
    return UInt((word >> offset) & ((1 << numBits) - 1) & ((1 << numBits) - 1) & ((1 << numBits) - 1))
    // expected-error@-1 {{reasonable time}}
}
