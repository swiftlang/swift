/// a ==> depends-on-a-ext, depends-on-a-foo | a-ext ==> depends-on-a-ext

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/nominal-members/* %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./a-ext.swift ./depends-on-a-foo.swift ./depends-on-a-ext.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-INITIAL %s

// CHECK-INITIAL-NOT: warning
// CHECK-INITIAL: Handled a.swift
// CHECK-INITIAL: Handled a-ext.swift
// CHECK-INITIAL: Handled depends-on-a-foo.swift
// CHECK-INITIAL: Handled depends-on-a-ext.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./a-ext.swift ./depends-on-a-foo.swift ./depends-on-a-ext.swift  -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-CLEAN %s

// CHECK-CLEAN-NOT: Handled

// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./a-ext.swift ./depends-on-a-foo.swift ./depends-on-a-ext.swift  -module-name main -j1 -v > %t/touched-a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-TOUCHED-A %s < %t/touched-a.txt
// RUN: %FileCheck -check-prefix=NEGATIVE-TOUCHED-A %s < %t/touched-a.txt

// CHECK-TOUCHED-A: Handled a.swift
// CHECK-TOUCHED-A-DAG: Handled depends-on-a-foo.swift
// CHECK-TOUCHED-A-DAG: Handled depends-on-a-ext.swift
// NEGATIVE-TOUCHED-A-NOT: Handled a-ext.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./a-ext.swift ./depends-on-a-foo.swift ./depends-on-a-ext.swift  -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-CLEAN %s


// RUN: touch -t 201401240007 %t/a-ext.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./a-ext.swift ./depends-on-a-foo.swift ./depends-on-a-ext.swift  -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-TOUCHED-EXT %s

// CHECK-TOUCHED-EXT-NOT: Handled
// CHECK-TOUCHED-EXT: Handled a-ext.swift
// CHECK-TOUCHED-EXT-NOT: Handled
// CHECK-TOUCHED-EXT: Handled depends-on-a-ext.swift
// CHECK-TOUCHED-EXT-NOT: Handled
