/// other --> main ==> yet-another

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/chained-private/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift
// CHECK-FIRST: Handled yet-another.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/other.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | cat > %t/outputToCheck

// RUN: %FileCheck -check-prefix=CHECK-THIRD %s < %t/outputToCheck

// Driver now schedules jobs in the order the inputs were given, but
// either order is fine.
// CHECK-THIRD-DAG: Handled main.swift
// CHECK-THIRD-DAG: Handled other.swift

// RUN: %FileCheck -check-prefix=CHECK-THIRD-EXCLUSION %s < %t/outputToCheck

// CHECK-THIRD-EXCLUSION-NOT: Handled yet-another.swift
