// RUN: %target-typecheck-verify-swift -disable-experimental-associated-type-inference
// RUN: %target-typecheck-verify-swift -enable-experimental-associated-type-inference

// The 'for' loop has to come first, to force Sequence.makeIterator().
for x in S() { _ = x }

struct S: RandomAccessCollection {
  public var startIndex: Int { 0 }
  public var endIndex: Int { 0 }
  public subscript(position: Int) -> Int { 0 }
}

