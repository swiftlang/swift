/// main <==> other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/mutual-with-swiftdeps/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s
// CHECK-COARSE-FIRST: Handled main.swift
// CHECK-COARSE-FIRST: Handled other.swift
// CHECK-COARSE-FIRST: Disabling incremental build: could not read build record

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-SECOND %s
// CHECK-COARSE-SECOND-NOT: Queuing

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-THIRD %s
// CHECK-COARSE-THIRD: Queuing (initial): {compile: other.o <= other.swift}
// CHECK-COARSE-THIRD: Queuing because of the initial set: {compile: main.o <= main.swift}
// CHECK-COARSE-THIRD-NEXT: other.swift provides top-level name 'a'



// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/mutual-with-swiftdeps/output.json %t
// RUN: echo 'var b = a' >%t/main.swift
// RUN: echo 'let a = 23; struct S {let X = b}' >%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s
// CHECK-FINE-FIRST: Disabling incremental build: could not read build record

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FINE-SECOND %s
// CHECK-FINE-SECOND-NOT: Queuing{{.*}}compile:

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v -driver-show-incremental 2>&1 | %FileCheck -check-prefix=CHECK-FINE-THIRD %s
// CHECK-FINE-THIRD: Queuing (initial): {compile: other.o <= other.swift}
// CHECK-FINE-THIRD: Queuing because of the initial set: {compile: main.o <= main.swift}
// CHECK-FINE-THIRD-NEXT: interface of top-level name 'a' in other.swift -> interface of source file main.swiftdeps

