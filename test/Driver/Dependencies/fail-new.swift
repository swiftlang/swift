/// bad ==> main | bad --> other
/// Coarse, then fine


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE %s
// CHECK-COARSE-NOT: warning
// CHECK-COARSE: Handled main.swift
// CHECK-COARSE: Handled bad.swift
// CHECK-COARSE-NOT: Handled other.swift

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-BAD-ONLY %s

// CHECK-COARSE-BAD-ONLY-NOT: warning
// CHECK-COARSE-BAD-ONLY-NOT: Handled
// CHECK-COARSE-BAD-ONLY: Handled bad.swift
// CHECK-COARSE-BAD-ONLY-NOT: Handled

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-OKAY %s
// CHECK-COARSE-OKAY: Handled main.swift
// CHECK-COARSE-OKAY: Handled bad.swift
// CHECK-COARSE-OKAY: Handled other.swift
// CHECK-COARSE-OKAY-NOT: Handled

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: rm %t/bad.swiftdeps
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE %s

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-OKAY-2 %s

// CHECK-COARSE-OKAY-2-DAG: Handled bad.swift
// CHECK-COARSE-OKAY-2-DAG: Handled other.swift
// CHECK-COARSE-OKAY-2-DAG: Handled main.swift

// RUN: touch -t 201401240006 %t/main.swift
// RUN: rm %t/main.swiftdeps
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE %s

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-OKAY %s

// RUN: touch -t 201401240006 %t/other.swift
// RUN: rm %t/other.swiftdeps
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE %s



// Simulate update-dependencies.py vs update-dependencies-bad.py by changing bad.swift to something legal and v/v

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-simple/output.json %t
// RUN: echo 'var X = theUnknown; func a() -> Int {4}'>%t/bad.swift
// RUN: echo 'struct S {let L = a()}; func a(b: Int = 3) {}'>%t/main.swift
// RUN: echo 'func F() {_ = a()}'>%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE %s
// CHECK-FINE: Disabling incremental

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-BAD-ONLY %s

// CHECK-FINE-BAD-ONLY-NOT: Queuing{{.*}}compile:
// CHECK-FINE-BAD-ONLY: Queuing{{.*}}compile:{{.*}} bad.swift
// CHECK-FINE-BAD-ONLY-NOT: Queuing{{.*}}compile:

// Create swiftdeps files
// RUN: echo 'func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 >/dev/null

// RUN: touch %t/bad.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-OKAY %s
// CHECK-FINE-OKAY-DAG: Queuing{{.*}}compile:{{.*}} bad.swift
// CHECK-FINE-OKAY-DAG: Queuing{{.*}}compile:{{.*}} main.swift
// CHECK-FINE-OKAY-DAG: Queuing{{.*}}compile:{{.*}} other.swift

// RUN: echo 'var X = theUnknown; func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240006 %t/bad.swift
// RUN: rm %t/bad.swiftdeps
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE %s

// RUN: echo 'func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-OKAY-2 %s

// CHECK-FINE-OKAY-2-DAG: Queuing{{.*}}compile:{{.*}} bad.swift
// CHECK-FINE-OKAY-2-DAG: Queuing{{.*}}compile:{{.*}} other.swift
// CHECK-FINE-OKAY-2-DAG: Queuing{{.*}}compile:{{.*}} main.swift

// RUN: echo 'var X = theUnknown; func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: touch -t 201401240006 %t/main.swift
// RUN: rm %t/main.swiftdeps
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE %s

// RUN: echo 'func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-OKAY %s

// RUN: echo 'var X = theUnknown(); func a() -> Int {4}'>%t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: touch -t 201401240006 %t/other.swift
// RUN: rm %t/other.swiftdeps
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE %s
