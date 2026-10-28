// RUN: %target-swift-frontend -emit-ir %s -module-name main | %FileCheck %s

// Reflection records must name Swift protocols with a symbolic reference to
// the protocol descriptor. The textual mangling of a private protocol
// contains a private discriminator, which reflection cannot reconstruct from
// the protocol descriptor, so a textual name could never be matched.

// Field descriptors.

// CHECK-DAG: @"$s4main8PrivateP{{.*}}_pMF" = internal constant {{.*}} @"symbolic _____ 4main8PrivateP{{.*}}P"
private protocol PrivateP {}

// Associated type descriptors.

// CHECK-DAG: @"$s4main9Conformer{{.*}}PrivateAssocP{{.*}}MA" = internal constant {{.*}} @"symbolic _____ 4main13PrivateAssocP{{.*}}P"
private protocol PrivateAssocP { associatedtype A }
private struct Conformer: PrivateAssocP { typealias A = Int }
