/// bad ==> main | bad --> other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s
// CHECK-NOT: warning
// CHECK: Handled main.swift
// CHECK: Handled bad.swift
// CHECK-NOT: Handled other.swift

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-BAD-ONLY %s

// CHECK-BAD-ONLY-NOT: warning
// CHECK-BAD-ONLY-NOT: Handled
// CHECK-BAD-ONLY: Handled bad.swift
// CHECK-BAD-ONLY-NOT: Handled

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-OKAY %s
// CHECK-OKAY: Handled main.swift
// CHECK-OKAY: Handled bad.swift
// CHECK-OKAY: Handled other.swift
// CHECK-OKAY-NOT: Handled

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: rm %t/bad.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-OKAY-2 %s

// CHECK-OKAY-2-DAG: Handled bad.swift
// CHECK-OKAY-2-DAG: Handled other.swift
// CHECK-OKAY-2-DAG: Handled main.swift

// RUN: touch -t 201401240006 %t/main.swift
// RUN: rm %t/main.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s

// RUN: touch -t 201401240005 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-OKAY %s
// RUN: touch -t 201401240006 %t/other.swift
// RUN: rm %t/other.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck %s
