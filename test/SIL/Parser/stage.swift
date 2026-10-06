// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -emit-module %s -O -parse-stdlib -parse-as-library -emit-module -o %t/stage.swiftmodule
// RUN: %target-sil-opt %t/stage.swiftmodule -sil-disable-ast-dump -o %t/stage.sil
// RUN: %target-sil-opt %t/stage.sil -o - | %FileCheck %s

// FIXME: We create all SIL modules in the 'raw' stage regardless of the input
// kind. If the primary input is a serialized module, we should assume the
// canonical stage.
//
// The per-function half of that FIXME is done: a serialized function records
// its own stage, so the body below comes back Canonical even though the module
// floor is still Raw. That divergence is what '[stage=canonical]' reports.
//
// CHECK: sil_stage raw
//
// '[canonical]' is provenance, meaning the body was diagnosed elsewhere.
// '[stage=canonical]' is the phase, meaning this function is ahead of the
// floor. They are independent, and both must survive the round trip.
// CHECK: sil [serialized] [canonical] [stage=canonical] [ossa] @$s5stage21functionToReserializeyyF : $@convention(thin) () -> () {
@inlinable
public func functionToReserialize() {}
