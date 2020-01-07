/// crash ==> main | crash --> other
/// coarse, fine

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-INITIAL %s

// CHECK-COARSE-INITIAL-NOT: warning
// CHECK-COARSE-INITIAL: Handled main.swift
// CHECK-COARSE-INITIAL: Handled other.swift

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./crash.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-ADDED-NOT: Handled
// CHECK-COARSE-ADDED: Handled crash.swift
// CHECK-COARSE-ADDED-NOT: Handled

// CHECK-COARSE-RECORD-ADDED-DAG: "./crash.swift": !dirty [
// CHECK-COARSE-RECORD-ADDED-DAG: "./main.swift": [
// CHECK-COARSE-RECORD-ADDED-DAG: "./other.swift": [


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-INITIAL %s

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./crash.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps



// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/output.json %t
// RUN: echo 'func a() -> Int {34}' >%t/crash.swift
// RUN: echo 'var V = a()' >%t/main.swift
// RUN: echo 'func F() {_ = a()}' >%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-INITIAL %s

// CHECK-FINE-INITIAL-NOT: warning
// CHECK-FINE-INITIAL: Handled main.swift
// CHECK-FINE-INITIAL: Handled other.swift

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./crash.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-ADDED-NOT: Handled
// CHECK-FINE-ADDED: Handled crash.swift
// CHECK-FINE-ADDED-NOT: Handled

// CHECK-FINE-RECORD-ADDED-DAG: "./crash.swift": !dirty [
// CHECK-FINE-RECORD-ADDED-DAG: "./main.swift": [
// CHECK-FINE-RECORD-ADDED-DAG: "./other.swift": [


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/crash-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-INITIAL %s

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./crash.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps
