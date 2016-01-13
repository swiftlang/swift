// RUN: rm -rf %t && mkdir %t
// RUN: touch %t/a.swift %t/b.swift %t/c.swift

// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/check-filelist-abc.py -c %t/a.swift %t/b.swift %t/c.swift -module-name main -driver-use-filelists 2>&1 | FileCheck %s

// CHECK-DAG: Handled a.swift
// CHECK-DAG: Handled b.swift
// CHECK-DAG: Handled c.swift

// RUN: %swiftc_driver_plain -driver-use-frontend-path %S/Inputs/check-filelist-abc.py -c %t/a.swift %t/b.swift %t/c.swift -module-name main -driver-use-filelists -force-single-frontend-invocation 2>&1 | FileCheck -check-prefix=CHECK-WMO %s

// CHECK-WMO: Handled all
