// RUN: %target-typecheck-verify-swift -enable-experimental-feature Reparenting

// REQUIRES: swift_feature_Reparenting

protocol P {}

@reparentable protocol Reparentable {}
protocol S: Reparentable {}

struct Nachos<T> {}
extension Nachos: S where T: P {}

// Expect no error here the conditional conformance to S
// implying conformance to Reparentable and needing to
// write that in a separate extension of Nachos.
