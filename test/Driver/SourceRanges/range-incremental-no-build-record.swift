// Check behavior with no build record.
//
// =============================================================================
// First, build without range dependencies with no new options.
// =============================================================================


// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation


// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-incremental-no-build-record/* %t
// RUN: cd %t && %swiftc_driver -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental  >& %t/output0

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-NO-BUILD-REC %s < %t/output0
// CHECK-NO-BUILD-REC: Incremental compilation could not read build record.


// RUN: ls %t | %FileCheck -check-prefix=CHECK-NO-RANGE-OUTPUTS %s
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource
// CHECK-NO-RANGE-OUTPUTS: .swiftdeps
// CHECK-NO-RANGE-OUTPUTS-NOT: .swiftranges
// CHECK-NO-RANGE-OUTPUTS-NOT: .compiledsource

// RUN: %FileCheck -check-prefix=CHECK-HAS-BATCHES %s < %t/output0

// CHECK-HAS-BATCHES: Batchable: {compile:

// RUN: %t/main | tee run0 | grep Any > /dev/null && rm %t/main

// =============================================================================
// Same, except force the driver to compute both strategies via -driver-compare-incremental-schemes
// =============================================================================


// Copy in the inputs.
// The lack of a build record or swiftdeps files should disable incremental compilation


// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-incremental-no-build-record/* %t
// RUN: cd %t && %swiftc_driver -driver-compare-incremental-schemes -output-file-map %t/output.json -incremental -enable-batch-mode ./main.swift ./fileA.swift ./fileB.swift -module-name main -j2 -driver-show-job-lifecycle -driver-show-incremental  >& %t/output1

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-NO-BUILD-REC %s < %t/output1

// RUN: %FileCheck -match-full-lines -check-prefix=CHECK-COMPARE-DISABLED-NO-BUILD-RECORD %s < %t/output1
// CHECK-COMPARE-DISABLED-NO-BUILD-RECORD: *** Incremental build disabled because could not read build record, cannot compare ***

// RUN: %t/main | tee run1 | grep Any > /dev/null && rm %t/main
