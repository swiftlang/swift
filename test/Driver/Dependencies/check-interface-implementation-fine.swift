/// The fine-grained dependency graph has implicit dependencies from interfaces to implementations.
/// These are not presently tested because depends nodes are marked as depending in the interface,
/// as of 1/9/20. But this test will check fail if those links are not followed.

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/check-interface-implementation-fine/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -disable-direct-intramodule-dependencies -driver-always-rebuild-dependents ./a.swift ./c.swift  ./bad.swift -module-name main -j1 -v  2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled a.swift
// CHECK-FIRST: Handled c.swift
// CHECK-FIRST: Handled bad.swift

// CHECK-RECORD-CLEAN-DAG: "./a.swift": [
// CHECK-RECORD-CLEAN-DAG: "./bad.swift": [
// CHECK-RECORD-CLEAN-DAG: "./c.swift": [


// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t &&  not %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies-bad.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -disable-direct-intramodule-dependencies -driver-always-rebuild-dependents ./a.swift ./bad.swift ./c.swift  -module-name main -j1 -v  -driver-show-incremental > %t/a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-A: Handled a.swift
// CHECK-A: Handled bad.swift
// NEGATIVE-A-NOT: Handled c.swift

// CHECK-RECORD-A-DAG: "./a.swift": [
// CHECK-RECORD-A-DAG: "./bad.swift": !private [
// CHECK-RECORD-A-DAG: "./c.swift": !private [

// RUN: cd %t &&   %swiftc_driver -c -driver-use-frontend-path "%{python.unquoted};%S/Inputs/update-dependencies.py;%swift-dependency-tool" -output-file-map %t/output.json -incremental -disable-direct-intramodule-dependencies -driver-always-rebuild-dependents ./a.swift ./bad.swift ./c.swift  -module-name main -j1 -v  -driver-show-incremental 2>&1 | %FileCheck -check-prefix CHECK-BC %s

// CHECK-BC-NOT: Handled a.swift
// CHECK-BC-DAG: Handled bad.swift
// CHECK-BC-DAG: Handled c.swift
