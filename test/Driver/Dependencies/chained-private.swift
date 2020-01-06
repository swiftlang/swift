/// other --> main ==> yet-another
/// Coarse & fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/chained-private/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST: Handled main.swift
// CHECK-COARSE-FIRST: Handled other.swift
// CHECK-COARSE-FIRST: Handled yet-another.swift

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | tee out| %FileCheck -check-prefix=CHECK-COARSE-SECOND %s

// CHECK-COARSE-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v >%t/outputToCheck 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-THIRD %s < %t/outputToCheck

// Driver now schedules jobs in the order the inputs were given, but
// either order is fine.
// CHECK-COARSE-THIRD-DAG: Handled main.swift
// CHECK-COARSE-THIRD-DAG: Handled other.swift

// RUN: %FileCheck -check-prefix=CHECK-COARSE-THIRD-EXCLUSION %s < %t/outputToCheck

// CHECK-COARSE-THIRD-EXCLUSION-NOT: Handled yet-another.swift


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/chained-private/output.json %t
// RUN: echo 'struct z {}; private func foo() {a()}' >%t/main.swift
// RUN: echo 'func a() {} ' >%t/other.swift
// RUN: echo 'struct zz {let a = z()}' >%t/yet-another.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// CHECK-FINE-FIRST: Disabling incremental build

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | tee out| %FileCheck -check-prefix=CHECK-FINE-SECOND %s

// CHECK-FINE-SECOND-NOT: compile:

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v >%t/outputToCheck 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-THIRD %s < %t/outputToCheck

// Driver now schedules jobs in the order the inputs were given, but
// either order is fine.
// CHECK-FINE-THIRD-DAG: compile: {{.*}} main.swift
// CHECK-FINE-THIRD-DAG: compile: {{.*}} other.swift

// RUN: %FileCheck -check-prefix=CHECK-FINE-THIRD-EXCLUSION %s < %t/outputToCheck

// CHECK-FINE-THIRD-EXCLUSION-NOT: compile: {{.*}} yet-another.swift

