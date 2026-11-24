// RUN: %empty-directory(%t)
//
// `-sil-debug-serialization` returns from runSILDiagnosticPasses before the
// floor commits to Canonical, and substitutes a cut-down pipeline for the
// mandatory checkers. AccessEnforcementSelection never runs, so the emitted
// `.swiftmodule` holds raw SIL: `begin_access [unknown]`.
//
// RUN: %target-swift-frontend -emit-module -module-name Lib -o %t/Lib.swiftmodule %s -sil-debug-serialization
// RUN: %target-sil-opt %t/Lib.swiftmodule -module-name Lib -sil-verify-all | %FileCheck %s
//
// Reading that module back used to abort with "access must have known
// enforcement outside raw stage", because the deserializer inferred Canonical
// from the container kind. The stage is recorded now, so the body comes back at
// the stage it was written at.
//
// CHECK: sil_stage raw
// CHECK: begin_access {{.*}}[unknown]

public var counter: Int = 0

@inlinable
public func bump(_ x: inout Int) {
  x += 1
  counter += 1
}
