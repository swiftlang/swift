// RUN: %target-swift-frontend -parse-as-library -O %s -emit-ir | %FileCheck %s

// No global metadata, witness tables, or reflection field descriptor, etc.
// CHECK-NOT: @"$s{{.* =}}"

// No conformance records
// CHECK-NOT: protocol_conformances

// No functions
// CHECK-NOT: define

struct MicroSequence : Sequence, IteratorProtocol {
  func next() -> Int? {
    return nil
  }
}

