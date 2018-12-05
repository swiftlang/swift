/// main ==> depends-on-main | bad ==> depends-on-bad

// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-with-bad-deps/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled bad.swift
// CHECK-FIRST: Handled depends-on-main.swift
// CHECK-FIRST: Handled depends-on-bad.swift

// Reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/fail-with-bad-deps/*.swiftdeps %t

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-NONE %s
// CHECK-NONE-NOT: Handled

// Reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/fail-with-bad-deps/*.swiftdeps %t

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-BUILD-ALL %s

// CHECK-BUILD-ALL-NOT: warning
// CHECK-BUILD-ALL: Handled bad.swift
// CHECK-BUILD-ALL-DAG: Handled main.swift
// CHECK-BUILD-ALL-DAG: Handled depends-on-main.swift
// CHECK-BUILD-ALL-DAG: Handled depends-on-bad.swift

// Reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/fail-with-bad-deps/*.swiftdeps %t

// RUN: touch -t 201401240007 %t/bad.swift %t/main.swift
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./depends-on-main.swift ./depends-on-bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-WITH-FAIL %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD %s < %t/main~buildrecord.swiftdeps

// CHECK-WITH-FAIL: Handled main.swift
// CHECK-WITH-FAIL-NOT: Handled depends
// CHECK-WITH-FAIL: Handled bad.swift
// CHECK-WITH-FAIL-NOT: Handled depends

// CHECK-RECORD-DAG: "./bad.swift": !private [
// CHECK-RECORD-DAG: "./main.swift": [
// CHECK-RECORD-DAG: "./depends-on-main.swift": !dirty [
// CHECK-RECORD-DAG: "./depends-on-bad.swift": [
