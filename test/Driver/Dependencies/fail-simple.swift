/// bad ==> main | bad --> other
/// Coarse, then fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST: Handled main.swift
// CHECK-COARSE-FIRST: Handled bad.swift
// CHECK-COARSE-FIRST: Handled other.swift

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-SECOND %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-SECOND: Handled bad.swift
// CHECK-COARSE-SECOND-NOT: Handled main.swift
// CHECK-COARSE-SECOND-NOT: Handled other.swift

// CHECK-COARSE-RECORD-DAG: "./bad.swift": !dirty [
// CHECK-COARSE-RECORD-DAG: "./main.swift": !dirty [
// CHECK-COARSE-RECORD-DAG: "./other.swift": !private [



// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-simple/output.json %t
// RUN: echo 'func a() -> Int {4}'>%t/bad.swift
// RUN: echo 'struct S {let L = a()}; func a(b: Int = 3) {}'>%t/main.swift
// RUN: echo 'func F() {_ = a()}'>%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// CHECK-FINE-FIRST: Disabling incremental

// RUN: echo 'var X = theUnknown(); func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-SECOND: Queuing{{.*}}compile:{{.*}} bad.swift

// CHECK-FINE-RECORD-DAG: "./bad.swift": !dirty [
// CHECK-FINE-RECORD-DAG: "./main.swift": !dirty [
// CHECK-FINE-RECORD-DAG: "./other.swift": !private [
