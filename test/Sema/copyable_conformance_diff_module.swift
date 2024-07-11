// RUN: %empty-directory(%t)
// RUN: split-file %s %t

//// Ensure we cannot add the conformance in a different module.

// RUN: %target-build-swift \
// RUN:   -swift-version 5 \
// RUN:   -emit-module \
// RUN:   -emit-module-path %t/GardenKit.swiftmodule \
// RUN:   -emit-module-interface-path %t/GardenKit.swiftinterface \
// RUN:   -enable-library-evolution \
// RUN:   -module-name=GardenKit \
// RUN:   %t/GardenKit.swift

// RUN: %target-swift-frontend \
// RUN:   -typecheck -verify \
// RUN:   -I %t \
// RUN:   %t/Visitor.swift

//--- GardenKit.swift

public struct Garden<Plant: ~Copyable>: ~Copyable {
  public let plant: Plant
  public init(_ p: consuming Plant) { plant = p }
}

//--- Visitor.swift

import GardenKit

// expected-error@+1 {{conformance to 'Copyable' must occur in the same source file as generic struct 'Garden'}}
extension Garden: @retroactive Copyable where Plant: Copyable {}
