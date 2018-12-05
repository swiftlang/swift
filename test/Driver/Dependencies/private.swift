// a ==> b --> c ==> d | e ==> c

// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/private/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-INITIAL %s

// CHECK-INITIAL-NOT: warning
// CHECK-INITIAL: Handled a.swift
// CHECK-INITIAL: Handled b.swift
// CHECK-INITIAL: Handled c.swift
// CHECK-INITIAL: Handled d.swift
// CHECK-INITIAL: Handled e.swift

// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=CHECK-A-NEG %s < %t/a.txt

// CHECK-A: Handled a.swift
// CHECK-A-DAG: Handled b.swift
// CHECK-A-DAG: Handled c.swift
// CHECK-A-NEG-NOT: Handled d.swift
// CHECK-A-NEG-NOT: Handled e.swift

// RUN: touch -t 201401240006 %t/b.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/b.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-B %s < %t/b.txt
// RUN: %FileCheck -check-prefix=CHECK-B-NEG %s < %t/b.txt

// CHECK-B-NEG-NOT: Handled a.swift
// CHECK-B: Handled b.swift
// CHECK-B: Handled c.swift
// CHECK-B-NEG-NOT: Handled d.swift
// CHECK-B-NEG-NOT: Handled e.swift

// RUN: touch -t 201401240006 %t/c.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/c.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-C %s < %t/c.txt
// RUN: %FileCheck -check-prefix=CHECK-C-NEG %s < %t/c.txt

// CHECK-C-NEG-NOT: Handled a.swift
// CHECK-C-NEG-NOT: Handled b.swift
// CHECK-C: Handled c.swift
// CHECK-C: Handled d.swift
// CHECK-C-NEG-NOT: Handled e.swift

// RUN: touch -t 201401240006 %t/d.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/d.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-D %s < %t/d.txt
// RUN: %FileCheck -check-prefix=CHECK-D-NEG %s < %t/d.txt

// CHECK-D-NEG-NOT: Handled a.swift
// CHECK-D-NEG-NOT: Handled b.swift
// CHECK-D-NEG-NOT: Handled c.swift
// CHECK-D: Handled d.swift
// CHECK-D-NEG-NOT: Handled e.swift

// RUN: touch -t 201401240006 %t/e.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/e.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-E %s < %t/e.txt
// RUN: %FileCheck -check-prefix=CHECK-E-NEG %s < %t/e.txt

// CHECK-E-NEG-NOT: Handled a.swift
// CHECK-E-NEG-NOT: Handled b.swift
// CHECK-E: Handled e.swift
// CHECK-E-DAG: Handled c.swift
// CHECK-E-DAG: Handled d.swift

// RUN: touch -t 201401240007 %t/a.swift %t/e.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift -module-name main -j1 -v > %t/ae.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-AE %s < %t/ae.txt
// RUN: %FileCheck -check-prefix=CHECK-AE-NEG %s < %t/ae.txt

// CHECK-AE: Handled a.swift
// CHECK-AE: Handled e.swift
// CHECK-AE-DAG: Handled b.swift
// CHECK-AE-DAG: Handled c.swift
// CHECK-AE-DAG: Handled d.swift
// CHECK-AE-NEG: Handled
