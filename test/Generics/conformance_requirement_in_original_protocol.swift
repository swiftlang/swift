// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures -requirement-machine-protocol-signatures=verify 2>&1 | %FileCheck %s

public protocol P1 {
  associatedtype X
}

public protocol P2 {}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q1a@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.Y.Z, Self.Y : Q1b>

public protocol Q1a {
  associatedtype Y : Q1b where Self == Self.Y.Z
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q1b@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : P1, Self.Z : Q1a, Self.Z.X : P2>

public protocol Q1b {
  associatedtype Z : Q1a, P1 where Self.Z.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q2a@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : P1, Self.Z : Q2b, Self.Z.X : P2>

public protocol Q2a {
  associatedtype Z : Q2b, P1 where Self.Z.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q2b@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.Y.Z, Self.Y : Q2a>

public protocol Q2b {
  associatedtype Y : Q2a where Self == Self.Y.Z
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q3a@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.Y.Z, Self.X : P2, Self.Y : Q3b>

public protocol Q3a : P1 {
  associatedtype Y : Q3b where Self == Self.Y.Z, Self.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q3b@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : Q3a>

public protocol Q3b {
  associatedtype Z : Q3a
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q4a@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : Q4b>

public protocol Q4a {
  associatedtype Z : Q4b
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q4b@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.Y.Z, Self.X : P2, Self.Y : Q4a>

public protocol Q4b : P1 {
  associatedtype Y : Q4a where Self == Self.Y.Z, Self.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q5a@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.Y.Z, Self.X : P2, Self.Y : Q5b>

public protocol Q5a {
  associatedtype Y : Q5b where Self == Self.Y.Z, Self.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q5b@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : P1, Self.Z : Q5a>

public protocol Q5b {
  associatedtype Z : Q5a, P1
}

///

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q6a@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : P1, Self.Z : Q6b>

public protocol Q6a {
  associatedtype Z : Q6b, P1
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q6b@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.Y.Z, Self.X : P2, Self.Y : Q6a>

public protocol Q6b {
  associatedtype Y : Q6a where Self == Self.Y.Z, Self.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q7a@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.Y.Z, Self.Y : Q7b>

public protocol Q7a : P1 {
  associatedtype Y : Q7b where Self == Self.Y.Z
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q7b@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : Q7a, Self.Z.X : P2>

public protocol Q7b {
  associatedtype Z : Q7a where Self.Z.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q8a@
// CHECK-LABEL: Requirement signature: <Self where Self.Z : Q8b, Self.Z.X : P2>

public protocol Q8a {
  associatedtype Z : Q8b where Self.Z.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q8b@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.Y.Z, Self.Y : Q8a>

public protocol Q8b : P1 {
  associatedtype Y : Q8a where Self == Self.Y.Z
}