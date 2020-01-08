/// bad ==> main | bad --> other
/// Coarse, then fine


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-INITIAL %s

// CHECK-COARSE-INITIAL-NOT: warning
// CHECK-COARSE-INITIAL: Handled main.swift
// CHECK-COARSE-INITIAL: Handled other.swift

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-ADDED-NOT: Handled
// CHECK-COARSE-ADDED: Handled bad.swift
// CHECK-COARSE-ADDED-NOT: Handled

// CHECK-COARSE-RECORD-ADDED-DAG: "./bad.swift": !dirty [
// CHECK-COARSE-RECORD-ADDED-DAG: "./main.swift": [
// CHECK-COARSE-RECORD-ADDED-DAG: "./other.swift": [

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-simple/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-INITIAL %s

// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps




// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-simple/output.json %t
// RUN: echo '#error "fail"; func a() -> Int {4}'>%t/bad.swift
// RUN: echo 'struct S {let L = a()}; func a(b: Int = 3) {}'>%t/main.swift
// RUN: echo 'func F() {_ = a()}'>%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-INITIAL %s

// CHECK-FINE-INITIAL: Disabling incremental

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-ADDED-NOT: Queuing{{.*}}compile:{{.*}}
// CHECK-FINE-ADDED: Queuing{{.*}}compile:{{.*}} bad.swift
// CHECK-FINE-ADDED-NOT: Queuing{{.*}}compile:{{.*}}

// CHECK-FINE-RECORD-ADDED-DAG: "./bad.swift": !dirty [
// CHECK-FINE-RECORD-ADDED-DAG: "./main.swift": [
// CHECK-FINE-RECORD-ADDED-DAG: "./other.swift": [


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-simple/output.json %t
// RUN: echo '#error "fail"; func a() -> Int {4}'>%t/bad.swift
// RUN: echo 'struct S {let L = a()}; func a(b: Int = 3) {}'>%t/main.swift
// RUN: echo 'func F() {_ = a()}'>%t/other.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-INITIAL %s

// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental  -c  -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-ADDED %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-ADDED %s < %t/main~buildrecord.swiftdeps
