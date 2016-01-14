// RUN: rm -rf %t && mkdir %t
// RUN: touch %t/a.swift %t/b.swift %t/c.swift

// RUN: (cd %t && env PATH=%S/Inputs/filelists/:$PATH %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -emit-module ./a.swift ./b.swift ./c.swift -module-name main -driver-use-filelists -output-file-map=%S/Inputs/filelists/output.json 2>&1 | FileCheck %s)

// CHECK-NOT: Handled
// CHECK: Handled a.swift
// CHECK: Handled b.swift
// CHECK: Handled c.swift
// CHECK: Handled modules
// CHECK-NOT: Handled

// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -emit-module %t/a.swift %t/b.swift %t/c.swift -module-name main -driver-use-filelists -force-single-frontend-invocation 2>&1 | FileCheck -check-prefix=CHECK-WMO %s

// CHECK-NOT: Handled
// CHECK-WMO: Handled all
// CHECK-NOT: Handled

// RUN: (cd %t && env PATH=%S/Inputs/filelists/:$PATH %swiftc_driver_plain -emit-library ./a.swift ./b.swift ./c.swift -module-name main -driver-use-filelists -output-file-map=%S/Inputs/filelists/output.json 2>&1 | FileCheck -check-prefix=CHECK-LINK %s)
// RUN: (cd %t && env PATH=%S/Inputs/filelists/:$PATH %swiftc_driver_plain -emit-library ./a.swift ./b.swift ./c.swift -module-name main -driver-use-filelists -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 2>&1 | FileCheck -check-prefix=CHECK-LINK %s)

// CHECK-LINK: Handled link

