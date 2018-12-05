/// a ==> bad ==> c ==> d | b --> bad --> e ==> f

// UNSUPPORTED: OS=windows-msvc

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-chained/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s
// RUN: %FileCheck -check-prefix=CHECK-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled a.swift
// CHECK-FIRST: Handled b.swift
// CHECK-FIRST: Handled c.swift
// CHECK-FIRST: Handled d.swift
// CHECK-FIRST: Handled e.swift
// CHECK-FIRST: Handled f.swift
// CHECK-FIRST: Handled bad.swift

// CHECK-RECORD-CLEAN-DAG: "./a.swift": [
// CHECK-RECORD-CLEAN-DAG: "./b.swift": [
// CHECK-RECORD-CLEAN-DAG: "./c.swift": [
// CHECK-RECORD-CLEAN-DAG: "./d.swift": [
// CHECK-RECORD-CLEAN-DAG: "./e.swift": [
// CHECK-RECORD-CLEAN-DAG: "./f.swift": [
// CHECK-RECORD-CLEAN-DAG: "./bad.swift": [


// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-A: Handled a.swift
// CHECK-A: Handled bad.swift
// NEGATIVE-A-NOT: Handled b.swift
// NEGATIVE-A-NOT: Handled c.swift
// NEGATIVE-A-NOT: Handled d.swift
// NEGATIVE-A-NOT: Handled e.swift
// NEGATIVE-A-NOT: Handled f.swift

// CHECK-RECORD-A-DAG: "./a.swift": [
// CHECK-RECORD-A-DAG: "./b.swift": [
// CHECK-RECORD-A-DAG: "./c.swift": !dirty [
// CHECK-RECORD-A-DAG: "./d.swift": !dirty [
// CHECK-RECORD-A-DAG: "./e.swift": !private [
// CHECK-RECORD-A-DAG: "./f.swift": [
// CHECK-RECORD-A-DAG: "./bad.swift": !dirty [

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/a2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A2 %s < %t/a2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-A2 %s < %t/a2.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-A2-DAG: Handled c.swift
// CHECK-A2-DAG: Handled d.swift
// CHECK-A2-DAG: Handled e.swift
// CHECK-A2-DAG: Handled bad.swift
// NEGATIVE-A2-NOT: Handled a.swift
// NEGATIVE-A2-NOT: Handled b.swift
// NEGATIVE-A2-NOT: Handled f.swift


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-chained/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// RUN: touch -t 201401240006 %t/b.swift
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/b.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-B %s < %t/b.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-B %s < %t/b.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-B %s < %t/main~buildrecord.swiftdeps

// CHECK-B: Handled b.swift
// CHECK-B: Handled bad.swift
// NEGATIVE-B-NOT: Handled a.swift
// NEGATIVE-B-NOT: Handled c.swift
// NEGATIVE-B-NOT: Handled d.swift
// NEGATIVE-B-NOT: Handled e.swift
// NEGATIVE-B-NOT: Handled f.swift

// CHECK-RECORD-B-DAG: "./a.swift": [
// CHECK-RECORD-B-DAG: "./b.swift": [
// CHECK-RECORD-B-DAG: "./c.swift": [
// CHECK-RECORD-B-DAG: "./d.swift": [
// CHECK-RECORD-B-DAG: "./e.swift": [
// CHECK-RECORD-B-DAG: "./f.swift": [
// CHECK-RECORD-B-DAG: "./bad.swift": !private [

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/b2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-B2 %s < %t/b2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-B2 %s < %t/b2.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-B2-DAG: Handled bad.swift
// NEGATIVE-B2-NOT: Handled a.swift
// NEGATIVE-B2-NOT: Handled b.swift
// NEGATIVE-B2-NOT: Handled c.swift
// NEGATIVE-B2-NOT: Handled d.swift
// NEGATIVE-B2-NOT: Handled e.swift
// NEGATIVE-B2-NOT: Handled f.swift


// RUN: rm -rf %t && cp -r %S/Inputs/fail-chained/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FIRST %s

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/bad.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-BAD %s < %t/bad.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-BAD %s < %t/bad.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-BAD: Handled bad.swift
// NEGATIVE-BAD-NOT: Handled a.swift
// NEGATIVE-BAD-NOT: Handled b.swift
// NEGATIVE-BAD-NOT: Handled c.swift
// NEGATIVE-BAD-NOT: Handled d.swift
// NEGATIVE-BAD-NOT: Handled e.swift
// NEGATIVE-BAD-NOT: Handled f.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/bad2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A2 %s < %t/bad2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-A2 %s < %t/bad2.txt
// RUN: %FileCheck -check-prefix=CHECK-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps
