// RUN: rm -rf %t && mkdir %t
// RUN: touch %t/a.swift %t/b.swift %t/c.swift

// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c %t/a.swift %t/b.swift %t/c.swift -module-name main -driver-use-filelists 2>&1 | FileCheck %s

// CHECK-DAG: Handled a.swift
// CHECK-DAG: Handled b.swift
// CHECK-DAG: Handled c.swift

// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/filelists/check-filelist-abc.py -c %t/a.swift %t/b.swift %t/c.swift -module-name main -driver-use-filelists -force-single-frontend-invocation 2>&1 | FileCheck -check-prefix=CHECK-WMO %s

// CHECK-WMO: Handled all

// RUN: (cd %t && env PATH=%S/Inputs/filelists/:$PATH %swiftc_driver_plain -emit-library ./a.swift ./b.swift ./c.swift -module-name main -driver-use-filelists -output-file-map=%S/Inputs/filelists/output.json 2>&1 | FileCheck -check-prefix=CHECK-LINK %s)
// RUN: (cd %t && env PATH=%S/Inputs/filelists/:$PATH %swiftc_driver_plain -emit-library ./a.swift ./b.swift ./c.swift -module-name main -driver-use-filelists -output-file-map=%S/Inputs/filelists/output.json -force-single-frontend-invocation -num-threads 1 2>&1 | FileCheck -check-prefix=CHECK-LINK %s)

// CHECK-LINK: Handled link
