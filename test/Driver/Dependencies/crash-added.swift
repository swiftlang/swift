/// crash ==> main | crash --> other

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-INITIAL %s

// CHECK-INITIAL-NOT: warning
// CHECK-INITIAL: Handled main.swift
// CHECK-INITIAL: Handled other.swift

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./crash.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps

// CHECK-ADDED-NOT: Handled
// CHECK-ADDED: Handled crash.swift
// CHECK-ADDED-NOT: Handled

// CHECK-RECORD-ADDED-DAG: "./crash.swift": !dirty [
// CHECK-RECORD-ADDED-DAG: "./main.swift": [
// CHECK-RECORD-ADDED-DAG: "./other.swift": [


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-INITIAL %s

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./crash.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps
