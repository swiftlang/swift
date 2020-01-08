/// a ==> bad ==> c ==> d | b --> bad --> e ==> f

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-chained/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-FIRST-NOT: warning
// CHECK-COARSE-FIRST: Handled a.swift
// CHECK-COARSE-FIRST: Handled b.swift
// CHECK-COARSE-FIRST: Handled c.swift
// CHECK-COARSE-FIRST: Handled d.swift
// CHECK-COARSE-FIRST: Handled e.swift
// CHECK-COARSE-FIRST: Handled f.swift
// CHECK-COARSE-FIRST: Handled bad.swift

// CHECK-COARSE-RECORD-CLEAN-DAG: "./a.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./b.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./c.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./d.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./e.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./f.swift": [
// CHECK-COARSE-RECORD-CLEAN-DAG: "./bad.swift": [


// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./bad.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift -module-name main -j1 -v > %t/a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-A: Handled a.swift
// CHECK-COARSE-A: Handled bad.swift
// NEGATIVE-COARSE-A-NOT: Handled b.swift
// NEGATIVE-COARSE-A-NOT: Handled c.swift
// NEGATIVE-COARSE-A-NOT: Handled d.swift
// NEGATIVE-COARSE-A-NOT: Handled e.swift
// NEGATIVE-COARSE-A-NOT: Handled f.swift

// CHECK-COARSE-RECORD-A-DAG: "./a.swift": [
// CHECK-COARSE-RECORD-A-DAG: "./b.swift": [
// CHECK-COARSE-RECORD-A-DAG: "./c.swift": !dirty [
// CHECK-COARSE-RECORD-A-DAG: "./d.swift": !dirty [
// CHECK-COARSE-RECORD-A-DAG: "./e.swift": !private [
// CHECK-COARSE-RECORD-A-DAG: "./f.swift": [
// CHECK-COARSE-RECORD-A-DAG: "./bad.swift": !dirty [

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/a2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-A2 %s < %t/a2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-A2 %s < %t/a2.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-A2-DAG: Handled c.swift
// CHECK-COARSE-A2-DAG: Handled d.swift
// CHECK-COARSE-A2-DAG: Handled e.swift
// CHECK-COARSE-A2-DAG: Handled bad.swift
// NEGATIVE-COARSE-A2-NOT: Handled a.swift
// NEGATIVE-COARSE-A2-NOT: Handled b.swift
// NEGATIVE-COARSE-A2-NOT: Handled f.swift


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-chained/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// RUN: touch -t 201401240006 %t/b.swift
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/b.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-B %s < %t/b.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-B %s < %t/b.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-B %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-B: Handled b.swift
// CHECK-COARSE-B: Handled bad.swift
// NEGATIVE-COARSE-B-NOT: Handled a.swift
// NEGATIVE-COARSE-B-NOT: Handled c.swift
// NEGATIVE-COARSE-B-NOT: Handled d.swift
// NEGATIVE-COARSE-B-NOT: Handled e.swift
// NEGATIVE-COARSE-B-NOT: Handled f.swift

// CHECK-COARSE-RECORD-B-DAG: "./a.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./b.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./c.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./d.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./e.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./f.swift": [
// CHECK-COARSE-RECORD-B-DAG: "./bad.swift": !private [

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/b2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-B2 %s < %t/b2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-B2 %s < %t/b2.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-B2-DAG: Handled bad.swift
// NEGATIVE-COARSE-B2-NOT: Handled a.swift
// NEGATIVE-COARSE-B2-NOT: Handled b.swift
// NEGATIVE-COARSE-B2-NOT: Handled c.swift
// NEGATIVE-COARSE-B2-NOT: Handled d.swift
// NEGATIVE-COARSE-B2-NOT: Handled e.swift
// NEGATIVE-COARSE-B2-NOT: Handled f.swift

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/fail-chained/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-COARSE-FIRST %s

// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && not %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies-bad.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift -module-name main -j1 -v > %t/bad.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-BAD %s < %t/bad.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-BAD %s < %t/bad.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-COARSE-BAD: Handled bad.swift
// NEGATIVE-COARSE-BAD-NOT: Handled a.swift
// NEGATIVE-COARSE-BAD-NOT: Handled b.swift
// NEGATIVE-COARSE-BAD-NOT: Handled c.swift
// NEGATIVE-COARSE-BAD-NOT: Handled d.swift
// NEGATIVE-COARSE-BAD-NOT: Handled e.swift
// NEGATIVE-COARSE-BAD-NOT: Handled f.swift

// RUN: cd %t && %swiftc_driver -disable-fine-grained-dependencies -c -driver-use-frontend-path "%{python};%S/Inputs/update-dependencies.py" -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/bad2.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-COARSE-A2 %s < %t/bad2.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-COARSE-A2 %s < %t/bad2.txt
// RUN: %FileCheck -check-prefix=CHECK-COARSE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps


//===================================================================================================================================

// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-chained/output.json %t
// RUN: echo 'let a = 3' > %t/a.swift
// RUN: echo 'let b = 4' > %t/b.swift
// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: echo 'let c = bad' > %t/c.swift
// RUN: echo 'let d = c' > %t/d.swift
// RUN: echo 'let e = 4; func EE() { _ = bad }' > %t/e.swift
// RUN: echo 'let f = e' > %t/f.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -driver-show-incremental -c -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-FIRST: Disabling incremental

// CHECK-FINE-RECORD-CLEAN-DAG: "./a.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./b.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./c.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./d.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./e.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./f.swift": [
// CHECK-FINE-RECORD-CLEAN-DAG: "./bad.swift": [


// RUN: echo 'let X = theUnknown; let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./bad.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift -module-name main -j1 -v -driver-show-job-lifecycle > %t/aF.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-A %s < %t/aF.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-FINE-A %s < %t/aF.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-A: -primary-file ./a.swift
// CHECK-FINE-A: -primary-file ./bad.swift
// NEGATIVE-FINE-A-NOT: -primary-file ./b.swift
// NEGATIVE-FINE-A-NOT: -primary-file ./c.swift
// NEGATIVE-FINE-A-NOT: -primary-file ./d.swift
// NEGATIVE-FINE-A-NOT: -primary-file ./e.swift
// NEGATIVE-FINE-A-NOT: -primary-file ./f.swift

// CHECK-FINE-RECORD-A-DAG: "./a.swift": [
// CHECK-FINE-RECORD-A-DAG: "./b.swift": [
// CHECK-FINE-RECORD-A-DAG: "./c.swift": !dirty [
// CHECK-FINE-RECORD-A-DAG: "./d.swift": !dirty [
// CHECK-FINE-RECORD-A-DAG: "./e.swift": !private [
// CHECK-FINE-RECORD-A-DAG: "./f.swift": [
// CHECK-FINE-RECORD-A-DAG: "./bad.swift": !dirty [

// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/a2F.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-A2 %s < %t/a2F.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-FINE-A2 %s < %t/a2F.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-A2-DAG: Queuing{{.*}}compile: {{.*}} c.swift
// CHECK-FINE-A2-DAG: Queuing{{.*}}compile: {{.*}} d.swift
// CHECK-FINE-A2-DAG: Queuing{{.*}}compile: {{.*}} e.swift
// CHECK-FINE-A2-DAG: Queuing{{.*}}compile: {{.*}} bad.swift
// NEGATIVE-FINE-A2-NOT: Queuing{{.*}}compile: {{.*}} a.swift
// NEGATIVE-FINE-A2-NOT: Queuing{{.*}}compile: {{.*}} b.swift
// NEGATIVE-FINE-A2-NOT: Queuing{{.*}}compile: {{.*}} f.swift


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-chained/output.json %t
// RUN: echo 'let a = 3' > %t/a.swift
// RUN: echo 'let b = 4' > %t/b.swift
// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: echo 'let c = bad' > %t/c.swift
// RUN: echo 'let d = c' > %t/d.swift
// RUN: echo 'let e = 4; func EE() { _ = bad }' > %t/e.swift
// RUN: echo 'let f = e' > %t/f.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// RUN: echo 'let X = theUnknown; let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: touch -t 201401240006 %t/b.swift
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/bF.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-B %s < %t/bF.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-FINE-B %s < %t/bF.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-B %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-B: Queuing{{.*}}compile: {{.*}} b.swift
// CHECK-FINE-B: Queuing{{.*}}compile: {{.*}} bad.swift
// NEGATIVE-FINE-B-NOT: Queuing{{.*}}compile: {{.*}} a.swift
// NEGATIVE-FINE-B-NOT: Queuing{{.*}}compile: {{.*}} c.swift
// NEGATIVE-FINE-B-NOT: Queuing{{.*}}compile: {{.*}} d.swift
// NEGATIVE-FINE-B-NOT: Queuing{{.*}}compile: {{.*}} e.swift
// NEGATIVE-FINE-B-NOT: Queuing{{.*}}compile: {{.*}} f.swift

// CHECK-FINE-RECORD-B-DAG: "./a.swift": [
// CHECK-FINE-RECORD-B-DAG: "./b.swift": [
// CHECK-FINE-RECORD-B-DAG: "./c.swift": [
// CHECK-FINE-RECORD-B-DAG: "./d.swift": [
// CHECK-FINE-RECORD-B-DAG: "./e.swift": [
// CHECK-FINE-RECORD-B-DAG: "./f.swift": [
// CHECK-FINE-RECORD-B-DAG: "./bad.swift": !dirty [

// RUN: sed 's/dirty/private/' <%t/main~buildrecord.swiftdeps >%t/zort; cp %t/zort %t/main~buildrecord.swiftdeps

// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v -driver-show-job-lifecycle > %t/b2F.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-B2 %s < %t/b2F.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps

// (no jobs scheduled till *after* bad finishes:
// CHECK-FINE-B2: Queuing (initial): {compile: bad.o <= bad.swift}
// CHECK-FINE-B2-NEXT: Adding standard job to task queue: {compile: bad.o <= bad.swift}
// CHECK-FINE-B2-NEXT: Added to TaskQueue: {compile: bad.o <= bad.swift}
// CHECK-FINE-B2-NEXT: Job finished: {compile: bad.o <= bad.swift}
// CHECK-FINE-B2: Queuing because of dependencies discovered later: {compile: c.o <= c.swift}


// RUN: %empty-directory(%t)
// RUN: cp %S/Inputs/fail-chained/output.json %t
// RUN: echo 'let a = 3' > %t/a.swift
// RUN: echo 'let b = 4' > %t/b.swift
// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: echo 'let c = bad' > %t/c.swift
// RUN: echo 'let d = c' > %t/d.swift
// RUN: echo 'let e = 4; func EE() { _ = bad }' > %t/e.swift
// RUN: echo 'let f = e' > %t/f.swift
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-FINE-FIRST %s

// RUN: echo 'let X = theUnknown; let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240006 %t/bad.swift
// RUN: cd %t && not %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./bad.swift ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift -module-name main -j1 -v > %t/badF.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-BAD %s < %t/badF.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-FINE-BAD %s < %t/badF.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-A %s < %t/main~buildrecord.swiftdeps

// CHECK-FINE-BAD: -primary-file ./bad.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./a.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./b.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./c.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./d.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./e.swift
// NEGATIVE-FINE-BAD-NOT: -primary-file ./f.swift

// RUN: echo 'let bad = 6; struct S {let T = a; func foo() {_ = b}}' > %t/bad.swift
// RUN: touch -t 201401240005 %t/bad.swift
// RUN: cd %t && %swiftc_driver -enable-fine-grained-dependencies -c -driver-show-incremental -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./bad.swift -module-name main -j1 -v > %t/bad2F.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-FINE-A2 %s < %t/bad2F.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-FINE-A2 %s < %t/bad2F.txt
// RUN: %FileCheck -check-prefix=CHECK-FINE-RECORD-CLEAN %s < %t/main~buildrecord.swiftdeps
