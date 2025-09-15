// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//// Ensure we cannot add the conformance in a different source file.

// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   %t/Visitor.swift %t/GardenKit.swift

//--- GardenKit.swift

public struct Garden<Plant: ~Copyable>: ~Copyable {
  public let plant: Plant
  public init(_ p: consuming Plant) { plant = p }
}

//--- Visitor.swift

// expected-error@+1 {{conformance to 'Copyable' must occur in the same source file as generic struct 'Garden'}}
extension Garden: Copyable where Plant: Copyable {}
