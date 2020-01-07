// other ==> main ==> yet-another
// Once with coarse-grained dependencies, once with fine.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/chained-additional-kinds/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST: Handled main.swift
// CHECK-COARSE-FIRST: Handled other.swift
// CHECK-COARSE-FIRST: Handled yet-another.swift

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-SECOND %s

// CHECK-COARSE-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-THIRD %s

// CHECK-COARSE-THIRD-DAG: Handled other.swift
// CHECK-COARSE-THIRD-DAG: Handled main.swift
// CHECK-COARSE-THIRD-DAG: Handled yet-another.swift

// RUN: touch -t 201401240007 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./main.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-THIRD %s



// other ==> main ==> yet-another

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/chained-additional-kinds/output.json %t
// RUN: echo 'struct S { let q = a() }; class C { func z() -> Int {3} }' >%t/main.swift
// RUN: echo 'func a() {}' >%t/other.swift
// RUN: echo ' var qqq = C().z()' >%t/yet-another.swift
// RUN: touch -t 201401240005 %t/{added,main,other,yet-another}.swift

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// CHECK-FINE-FIRST: Disabling incremental build: could not read build record

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s

// CHECK-FINE-SECOND-NOT: Queuing

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s

// CHECK-FINE-THIRD-DAG: Queuing {{.*}} {compile: other.o <= other.swift}
// CHECK-FINE-THIRD-DAG: Queuing {{.*}} {compile: main.o <= main.swift}
// CHECK-FINE-THIRD-DAG: Queuing {{.*}} {compile: yet-another.o <= yet-another.swift}

// RUN: touch -t 201401240007 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./main.swift ./yet-another.swift -module-name main -j1 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s
