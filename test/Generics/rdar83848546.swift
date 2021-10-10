// RUN: %target-typecheck-verify-swift -debug-generic-signatures -requirement-machine-protocol-signatures=on 2>&1 | %FileCheck %s

// CHECK: 83848546.(file).Reporter@
// CHECK-NEXT: Requirement signature: <Self where Self.SubReporterType : SubReporter>
protocol Reporter {
  associatedtype SubReporterType: SubReporter
  func makeSubReporter() -> SubReporterType
}

// CHECK: 83848546.(file).SubReporter@
// CHECK-NEXT: Requirement signature: <Self where Self.SubReporterType : SubReporter>
protocol SubReporter {
  associatedtype SubReporterType: SubReporter
  func makeSubReporter() -> SubReporterType
}

// CHECK: 83848546.(file).CausesCompilerCrash@
// CHECK-NEXT: Requirement signature: <Self where Self.ReporterType : Reporter, Self.SubReporterType == Self.ReporterType.SubReporterType, Self.ReporterType.SubReporterType == Self.SubReporterType.SubReporterType>
protocol CausesCompilerCrash {
  associatedtype ReporterType: Reporter
  associatedtype SubReporterType
    where ReporterType.SubReporterType == SubReporterType,
          SubReporterType.SubReporterType == SubReporterType
}

// CHECK: 83848546.(file).DoesNotCrash@
// CHECK-NEXT: Requirement signature: <Self where Self.ReporterType : Reporter, Self.ReporterType.SubReporterType == Self.ReporterType.SubReporterType.SubReporterType>
protocol DoesNotCrash {
  associatedtype ReporterType: Reporter
    where ReporterType.SubReporterType == ReporterType.SubReporterType.SubReporterType
}
