// other ==> main ==> yet-another
// Coarse, fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/chained/* %t
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

// RUN: touch -t 201401240008 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./yet-another.swift ./other.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-THIRD %s

// RUN: touch -t 201401240009 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./yet-another.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-THIRD %s




// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/chained/output.json %t
// RUN: echo 'var V = a; struct z {}'>%t/main.swift
// RUN: echo 'let a = 34'>%t/other.swift
// RUN: echo 'let V2 = z()'>%t/yet-another.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// CHECK-FINE-FIRST: Disabling incremental

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s

// CHECK-FINE-SECOND-NOT: Queuing{{.*}}compile:

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s

// CHECK-FINE-THIRD-DAG: Queuing{{.*}}compile:{{.*}} other.swift
// CHECK-FINE-THIRD-DAG: Queuing{{.*}}compile:{{.*}} main.swift
// CHECK-FINE-THIRD-DAG: Queuing{{.*}}compile:{{.*}} yet-another.swift

// RUN: touch -t 201401240007 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./main.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s

// RUN: touch -t 201401240008 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./yet-another.swift ./other.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s

// RUN: touch -t 201401240009 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./other.swift ./yet-another.swift ./main.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s
