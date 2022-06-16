// RUN: %target-typecheck-verify-swift -debug-generic-signatures 2>&1 | %FileCheck %s

// CHECK: 83848546.(file).Reporter@
// CHECK-NEXT: Requirement signature: <Self where Self.[Reporter]SubReporterType : SubReporter>
protocol Reporter {
  associatedtype SubReporterType: SubReporter
  func makeSubReporter() -> SubReporterType
}

// CHECK: 83848546.(file).SubReporter@
// CHECK-NEXT: Requirement signature: <Self where Self.[SubReporter]SubReporterType : SubReporter>
protocol SubReporter {
  associatedtype SubReporterType: SubReporter
  func makeSubReporter() -> SubReporterType
}

// CHECK: 83848546.(file).CausesCompilerCrash@
// CHECK-NEXT: Requirement signature: <Self where Self.[CausesCompilerCrash]ReporterType : Reporter, Self.[CausesCompilerCrash]SubReporterType == Self.[CausesCompilerCrash]ReporterType.[Reporter]SubReporterType, Self.[CausesCompilerCrash]ReporterType.[Reporter]SubReporterType == Self.[CausesCompilerCrash]SubReporterType.[SubReporter]SubReporterType>
protocol CausesCompilerCrash {
  associatedtype ReporterType: Reporter
  associatedtype SubReporterType
    where ReporterType.SubReporterType == SubReporterType,
          SubReporterType.SubReporterType == SubReporterType
}

// CHECK: 83848546.(file).DoesNotCrash@
// CHECK-NEXT: Requirement signature: <Self where Self.[DoesNotCrash]ReporterType : Reporter, Self.[DoesNotCrash]ReporterType.[Reporter]SubReporterType == Self.[DoesNotCrash]ReporterType.[Reporter]SubReporterType.[SubReporter]SubReporterType>
protocol DoesNotCrash {
  associatedtype ReporterType: Reporter
    where ReporterType.SubReporterType == ReporterType.SubReporterType.SubReporterType
}
