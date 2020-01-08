/// main ==> depends-on-main | bad ==> depends-on-bad
/// coarse & fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-interface-hash/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST: Handled main.swift
// CHECK-COARSE-FIRST: Handled bad.swift
// CHECK-COARSE-FIRST: Handled depends-on-main.swift
// CHECK-COARSE-FIRST: Handled depends-on-bad.swift

// Reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/fail-interface-hash/*.swiftdeps %t

// RUN: touch -t 201401240006 %t/bad.swift %t/main.swift
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-SECOND %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-SECOND: Handled main.swift
// CHECK-COARSE-SECOND-NOT: Handled depends
// CHECK-COARSE-SECOND: Handled bad.swift
// CHECK-COARSE-SECOND-NOT: Handled depends

// CHECK-COARSE-RECORD-DAG: "./bad.swift": !dirty [
// CHECK-COARSE-RECORD-DAG: "./main.swift": [
// CHECK-COARSE-RECORD-DAG: "./depends-on-main.swift": !dirty [
// CHECK-COARSE-RECORD-DAG: "./depends-on-bad.swift": [




// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-interface-hash/output.json %t
// RUN: echo 'let bad = 1' >%t/bad.swift
// RUN: echo 'let DB = bad' >%t/depends-on-bad.swift
// RUN: echo 'let main_ = 2' >%t/main.swift
// RUN: echo 'let DM = main_' >%t/depends-on-main.swift
// RUN: touch -t 201401240005 %t/*

// Create swiftdeps with "before" interface hashes

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c  -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-PRE %s

// CHECK-FINE-PRE-DAG: -primary-file ./main.swift
// CHECK-FINE-PRE-DAG: -primary-file ./depends-on-main.swift
// CHECK-FINE-PRE-DAG: -primary-file ./bad.swift
// CHECK-FINE-PRE-DAG: -primary-file ./depends-on-bad.swift

// Change the interface hashes
// RUN: sed 's/fingerprint:.*/fingerprint: CHANGED/' <%t/main.swiftdeps >%t/x; mv %t/{x,main.swiftdeps}
// RUN: sed 's/fingerprint:.*/fingerprint: CHANGED/' <%t/depends-on-main.swiftdeps >%t/x; mv %t/{x,depends-on-main.swiftdeps}
// RUN: sed 's/fingerprint:.*/fingerprint: CHANGED/' <%t/bad.swiftdeps >%t/x; mv %t/{x,bad.swiftdeps}
// RUN: sed 's/fingerprint:.*/fingerprint: CHANGED/' <%t/depends-on-bad.swiftdeps >%t/x; mv %t/{x,depends-on-bad.swiftdeps}

// RUN: echo 'let _ = TheUnknown(); let bad = 1' >%t/bad.swift
// RUN: touch -t 201401240006 %t/bad.swift %t/main.swift
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-SECOND: -primary-file ./main.swift
// CHECK-FINE-SECOND-NOT: -primary-file ./depends
// CHECK-FINE-SECOND: -primary-file ./bad.swift
// CHECK-FINE-SECOND-NOT: -primary-file ./depends

// CHECK-FINE-RECORD-DAG: "./bad.swift": !dirty [
// CHECK-FINE-RECORD-DAG: "./main.swift": [
// CHECK-FINE-RECORD-DAG: "./depends-on-main.swift": !dirty [
// CHECK-FINE-RECORD-DAG: "./depends-on-bad.swift": [
