// RUN: %target-typecheck-verify-swift -solver-scope-threshold=1000

// From rdar://113385631

extension Array where Element == UInt8 {
    public func readI32(_ byteOffset: inout Int64) -> UInt32 {
        precondition(byteOffset + 4 <= self.endIndex)
        defer { byteOffset += 4 }

        // expected-error@+1 {{reasonable time}}
        return (UInt32(self[byteOffset + 3]) << 24) +
            (UInt32(self[byteOffset + 2]) << 16) +
            (UInt32(self[byteOffset + 1]) << 8) +
            UInt32(self[byteOffset])
    }
}
