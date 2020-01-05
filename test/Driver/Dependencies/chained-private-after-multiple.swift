/// other --> main ==> yet-another
/// other ==>+ main ==> yet-another
/// coarse and fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/chained-private-after-multiple/* %t
// RUN: touch -t 201401240005 %t/*.swift

// Generate the build record...
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v

// ...then reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/chained-private-after-multiple/*.swiftdeps %t
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./yet-another.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-SECOND %s

// CHECK-COARSE-SECOND-DAG: Handled other.swift
// CHECK-COARSE-SECOND-DAG: Handled main.swift
// CHECK-COARSE-SECOND: Handled yet-another.swift




// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/chained-private-after-multiple/output.json %t
// RUN: echo 'func foo() {_ = a()}; struct b {}' >%t/main.swift
// RUN: echo 'func a() {}' >%t/other.swift
// RUN: echo 'var  vv = b()' >%t/yet-another.swift
// RUN: touch -t 201401240005 %t/*.swift

// Generate the build record...
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// CHECK-FINE-FIRST-NOT: Queuing{{.*}}compile:

// RUN: echo 'struct S { let aa = a}; struct b {}' >%t/main.swift
// RUN: touch -t 201401240005 %t/main.swift
// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./yet-another.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s

// CHECK-FINE-SECOND-DAG: Queuing{{.*}}compile:{{.*}}other.swift
// CHECK-FINE-SECOND-DAG: Queuing{{.*}}compile:{{.*}}main.swift
// CHECK-FINE-SECOND: Queuing{{.*}}compile:{{.*}}yet-another.swift
