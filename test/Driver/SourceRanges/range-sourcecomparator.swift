// Tests the diff algorithm (the SourceComparator) used for range-based
// incremental compilation.
//
// If you try to add inputs, be sure to update output.json and add a swiftranges
// input.

// Copy in the inputs.


// Ensure that the extra outputs are not generated when they should not be:
// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/range-sourcecomparator/* %t

// The lack of a build record or swiftdeps files should disable incremental compilation
// So, do one run just to build the build record

// RUN: cd %t && %swiftc_driver -output-file-map %t/output.json -incremental -c ./in1.swift ./in2.swift ./in3.swift ./in4.swift ./in5.swift ./in6.swift -module-name main -incremental -enable-source-range-dependencies  -driver-skip-execution -driver-compare-incremental-schemes >& output

// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in1.swiftdeps
// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in2.swiftdeps
// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in3.swiftdeps
// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in4.swiftdeps
// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in5.swiftdeps
// RUN: cp %S/Inputs/range-sourcecomparator/dummy.swiftdeps %t/in6.swiftdeps

// RUN: cd %t && %swiftc_driver -output-file-map %t/output.json -incremental -c ./in1.swift ./in2.swift ./in3.swift ./in4.swift ./in5.swift ./in6.swift -module-name main -incremental -enable-source-range-dependencies -driver-dump-compiled-source-diffs -driver-skip-execution -driver-compare-incremental-schemes >& output1


// RUN: %FileCheck  %s <%t/output1 --match-full-lines
// CHECK: *** all changed ranges in 'in1.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:1--2:15)
// CHECK-NEXT: *** all changed ranges in 'in1.swift' (w.r.t to-be-compiled) ***
// CHECK-NEXT: - [2:1--2:1)
// CHECK-NEXT: *** nonlocal changed ranges in 'in1.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:1--2:15)
// CHECK-EMPTY:
// CHECK-NEXT: *** all changed ranges in 'in2.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [1:3--1:4)
// CHECK-NEXT: *** all changed ranges in 'in2.swift' (w.r.t to-be-compiled) ***
// CHECK-NEXT: - [1:3--1:3)
// CHECK-NEXT: *** nonlocal changed ranges in 'in2.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [1:3--1:4)
// CHECK-EMPTY:
// CHECK-NEXT: *** all changed ranges in 'in3.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:1--2:1)
// CHECK-NEXT: *** all changed ranges in 'in3.swift' (w.r.t to-be-compiled) ***
// CHECK-NEXT: - [2:1--4:1)
// CHECK-NEXT: *** nonlocal changed ranges in 'in3.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:1--2:1)
// CHECK-EMPTY:
// CHECK-NEXT: *** all changed ranges in 'in4.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [1:5--1:16)
// CHECK-NEXT: *** all changed ranges in 'in4.swift' (w.r.t to-be-compiled) ***
// CHECK-NEXT: - [1:5--1:18)
// CHECK-NEXT: *** nonlocal changed ranges in 'in4.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [1:5--1:16)
// CHECK-EMPTY:
// CHECK-NEXT: *** all changed ranges in 'in5.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:6--2:7)
// CHECK-NEXT: - [4:6--5:6)
// CHECK-NEXT: *** all changed ranges in 'in5.swift' (w.r.t to-be-compiled) ***
// CHECK-NEXT: - [2:6--2:7)
// CHECK-NEXT: - [4:6--4:7)
// CHECK-NEXT: *** nonlocal changed ranges in 'in5.swift' (w.r.t previously-compiled) ***
// CHECK-NEXT: - [2:6--2:7)
// CHECK-NEXT: - [4:6--5:6)
