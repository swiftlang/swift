// RUN: %empty-directory(%t)
// RUN: touch %t/a.swift %t/b.swift %t/c.swift

// RUN: (cd %t && %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -emit-module ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json 2>&1 | %FileCheck %s)

// CHECK-NOT: Handled
// CHECK: Handled a.swift
// CHECK-NEXT: Supplementary "./a.swift":
// CHECK-NEXT: Supplementary swiftdoc: "./a.swiftdoc"
// CHECK-NEXT: Supplementary swiftmodule: "./a.swiftmodule"
// CHECK-NEXT: Handled b.swift
// CHECK-NEXT: Supplementary "./b.swift":
// CHECK-NEXT: Supplementary swiftdoc: "./b.swiftdoc"
// CHECK-NEXT: Supplementary swiftmodule: "./b.swiftmodule"
// CHECK-NEXT: Handled c.swift
// CHECK-NEXT: Supplementary "./c.swift":
// CHECK-NEXT: Supplementary swiftdoc: "./c.swiftdoc"
// CHECK-NEXT: Supplementary swiftmodule: "./c.swiftmodule"
// CHECK-NEXT: Handled modules
// CHECK-NOT: Handled



// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c %t/a.swift %t/b.swift %t/c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -force-single-frontend-invocation 2>&1 | %FileCheck -check-prefix=CHECK-WMO %s

// CHECK-WMO-NOT: Handled
// CHECK-WMO: Handled all
// CHECK-WMO: Supplementary "{{.*}}/a.swift":
// CHECK-WMO: Supplementary object: "main.o"
// CHECK-WMO-NOT: output
// CHECK-WMO-NOT: Handled


// RUN: %empty-directory(%t/bin)
// RUN: ln -s %S/Inputs/filelists/fake-ld.py %t/bin/ld

// RUN: (cd %t && %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 2>&1 | %FileCheck -check-prefix=CHECK-WMO-THREADED %s)
// RUN: (cd %t && %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 -embed-bitcode 2>&1 | %FileCheck -check-prefix=CHECK-WMO-THREADED %s)
// RUN: %empty-directory(%t/tmp)
// RUN: (cd %t && env TMPDIR="%t/tmp/" %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 -save-temps 2>&1 | %FileCheck -check-prefix=CHECK-WMO-THREADED %s)
// RUN: ls %t/tmp/sources-* %t/tmp/outputs-*

// CHECK-WMO-THREADED-NOT: Handled
// CHECK-WMO-THREADED: Handled all
// CHECK-WMO-THREADED-NEXT: Supplementary "{{.*}}/a.swift":
// CHECK-WMO-THREADED-NEXT: Supplementary {{object|llvm-bc}}: "{{.*}}/a.{{o|bc}}"
// CHECK-WMO-THREADED-NEXT: Supplementary "{{.*}}/b.swift":
// CHECK-WMO-THREADED-NEXT: Supplementary {{object|llvm-bc}}: "{{.*}}/b.{{o|bc}}"
// CHECK-WMO-THREADED-NEXT: Supplementary "{{.*}}/c.swift":
// CHECK-WMO-THREADED-NEXT: Supplementary {{object|llvm-bc}}: "{{.*}}/c.{{o|bc}}"
// CHECK-WMO-THREADED-NEXT: ...with output!
// CHECK-WMO-THREADED-NOT: Handled

// RUN: mkdir -p %t/tmp-fail/
// RUN: (cd %t && not env TMPDIR="%t/tmp-fail/" %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/fail.py -c ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1)
// RUN: not ls %t/tmp-fail/sources-*
// RUN: not ls %t/tmp-fail/outputs-*

// RUN: mkdir -p %t/tmp-crash/
// RUN: (cd %t && not env TMPDIR="%t/tmp-crash/" %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/crash.py -c ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1)
// RUN: ls %t/tmp-crash/sources-* %t/tmp-crash/outputs-*


// RUN: (cd %t && env PATH="%t/bin/:$PATH" %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -emit-library ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json 2>&1 | %FileCheck -check-prefix=CHECK-LINK %s)
// RUN: (cd %t && env PATH="%t/bin/:$PATH" %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -emit-library ./a.swift ./b.swift ./c.swift -module-name main -target x86_64-apple-macosx10.9 -driver-filelist-threshold=0 -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 2>&1 | %FileCheck -check-prefix=CHECK-LINK %s)

// CHECK-LINK: Handled link

