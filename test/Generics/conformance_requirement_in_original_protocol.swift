// RUN: %target-swift-frontend -typecheck %s -debug-generic-signatures 2>&1 | %FileCheck %s

public protocol P1 {
  associatedtype X
}

public protocol P2 {}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q1a@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[Q1a]Y.[Q1b]Z, Self.[Q1a]Y : Q1b>

public protocol Q1a {
  associatedtype Y : Q1b where Self == Self.Y.Z
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q1b@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q1b]Z : P1, Self.[Q1b]Z : Q1a, Self.[Q1b]Z.[P1]X : P2>

public protocol Q1b {
  associatedtype Z : Q1a, P1 where Self.Z.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q2a@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q2a]Z : P1, Self.[Q2a]Z : Q2b, Self.[Q2a]Z.[P1]X : P2>

public protocol Q2a {
  associatedtype Z : Q2b, P1 where Self.Z.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q2b@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[Q2b]Y.[Q2a]Z, Self.[Q2b]Y : Q2a>

public protocol Q2b {
  associatedtype Y : Q2a where Self == Self.Y.Z
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q3a@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.[Q3a]Y.[Q3b]Z, Self.[P1]X : P2, Self.[Q3a]Y : Q3b>

public protocol Q3a : P1 {
  associatedtype Y : Q3b where Self == Self.Y.Z, Self.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q3b@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q3b]Z : Q3a>

public protocol Q3b {
  associatedtype Z : Q3a
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q4a@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q4a]Z : Q4b>

public protocol Q4a {
  associatedtype Z : Q4b
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q4b@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.[Q4b]Y.[Q4a]Z, Self.[P1]X : P2, Self.[Q4b]Y : Q4a>

public protocol Q4b : P1 {
  associatedtype Y : Q4a where Self == Self.Y.Z, Self.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q5a@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[Q5a]Y.[Q5b]Z, Self.[P1]X : P2, Self.[Q5a]Y : Q5b>

public protocol Q5a {
  associatedtype Y : Q5b where Self == Self.Y.Z, Self.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q5b@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q5b]Z : P1, Self.[Q5b]Z : Q5a>

public protocol Q5b {
  associatedtype Z : Q5a, P1
}

///

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q6a@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q6a]Z : P1, Self.[Q6a]Z : Q6b>

public protocol Q6a {
  associatedtype Z : Q6b, P1
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q6b@
// CHECK-LABEL: Requirement signature: <Self where Self == Self.[Q6b]Y.[Q6a]Z, Self.[P1]X : P2, Self.[Q6b]Y : Q6a>

public protocol Q6b {
  associatedtype Y : Q6a where Self == Self.Y.Z, Self.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q7a@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.[Q7a]Y.[Q7b]Z, Self.[Q7a]Y : Q7b>

public protocol Q7a : P1 {
  associatedtype Y : Q7b where Self == Self.Y.Z
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q7b@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q7b]Z : Q7a, Self.[Q7b]Z.[P1]X : P2>

public protocol Q7b {
  associatedtype Z : Q7a where Self.Z.X : P2
}

////

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q8a@
// CHECK-LABEL: Requirement signature: <Self where Self.[Q8a]Z : Q8b, Self.[Q8a]Z.[P1]X : P2>

public protocol Q8a {
  associatedtype Z : Q8b where Self.Z.X : P2
}

// CHECK-LABEL: conformance_requirement_in_original_protocol.(file).Q8b@
// CHECK-LABEL: Requirement signature: <Self where Self : P1, Self == Self.[Q8b]Y.[Q8a]Z, Self.[Q8b]Y : Q8a>

public protocol Q8b : P1 {
  associatedtype Y : Q8a where Self == Self.Y.Z
}
