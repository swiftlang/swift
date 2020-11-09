// Test both orderings, and single-file vs WMO

// RUN: %target-swift-frontend -emit-ir -primary-file %s %S/Inputs/rdar67842221-other.swift -module-name main
// RUN: %target-swift-frontend -emit-ir %s -primary-file %S/Inputs/rdar67842221-other.swift -module-name main

// RUN: %target-swift-frontend -emit-ir -primary-file %S/Inputs/rdar67842221-other.swift %s -module-name main
// RUN: %target-swift-frontend -emit-ir %S/Inputs/rdar67842221-other.swift -primary-file %s -module-name main

// RUN: %target-swift-frontend -emit-ir %S/Inputs/rdar67842221-other.swift %s -module-name main
// RUN: %target-swift-frontend -emit-ir %S/Inputs/rdar67842221-other.swift %s -module-name main

// The closure defines a local class; we want to make sure there is no cycle
// between computing the semantic members of the local class (which requires
// sorting) and computing the type of the closure value
public func force() {
  _ = closureValue
}
