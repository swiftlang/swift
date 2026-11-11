// RUN: %empty-directory(%t)
// RUN: split-file %s %t
// RUN: %target-swift-frontend -emit-module %t/A.swift -module-name A -o %t/A.swiftmodule
// RUN: %target-swift-frontend -emit-module %t/B.swift -module-name B -I %t -o %t/B.swiftmodule
// RUN: %target-swift-frontend -typecheck -verify -I %t %t/main.swift

// An operator in one module shadows an operator with the same signature in a
// module that it imports, so only one of them can satisfy the requirement.

//--- A.swift

infix operator <=>

public protocol Q {
  static func <=>(lhs: Self, rhs: Self) -> Bool
}

public func <=><T: Q>(lhs: T, rhs: T) -> Bool { true }

//--- B.swift

@_exported import A

// Shadows A.<=>, so the conformance below isn't ambiguous.
public func <=><T: Q>(lhs: T, rhs: T) -> Bool { false }

//--- main.swift

import B

struct ConformsToQ: Q {}
