/// a --> b ==> c | a ==> d |    e ==> b |       f ==> g
/// a --> b ==> c | a ==> d +==> e +==> b, e --> f ==> g

// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/private-after/* %t
// RUN: touch -t 201401240005 %t/*.swift

// Generate the build record...
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./g.swift -module-name main -j1 -v

// ...then reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/private-after/*.swiftdeps %t

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./g.swift -module-name main -j1 -v 2>&1 | %FileCheck -check-prefix=CHECK-INITIAL %s

// CHECK-INITIAL-NOT: warning
// CHECK-INITIAL-NOT: Handled

// RUN: touch -t 201401240006 %t/a.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./g.swift -module-name main -j1 -v > %t/a.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-A %s < %t/a.txt
// RUN: %FileCheck -check-prefix=CHECK-A-NEG %s < %t/a.txt

// CHECK-A: Handled a.swift
// CHECK-A-DAG: Handled b.swift
// CHECK-A-DAG: Handled d.swift
// CHECK-A: Handled e.swift
// CHECK-A-DAG: Handled c.swift
// CHECK-A-DAG: Handled f.swift
// CHECK-A-NEG-NOT: Handled g.swift


// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/private-after/* %t
// RUN: touch -t 201401240005 %t/*.swift

// Generate the build record...
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./g.swift -module-name main -j1 -v

// ...then reset the .swiftdeps files.
// RUN: cp -r %S/Inputs/private-after/*.swiftdeps %t

// RUN: touch -t 201401240006 %t/f.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental -driver-always-rebuild-dependents ./a.swift ./b.swift ./c.swift ./d.swift ./e.swift ./f.swift ./g.swift -module-name main -j1 -v > %t/f.txt 2>&1
// RUN: %FileCheck -check-prefix=CHECK-F %s < %t/f.txt
// RUN: %FileCheck -check-prefix=CHECK-F-NEG %s < %t/f.txt

// CHECK-F: Handled f.swift
// CHECK-F: Handled g.swift
// CHECK-F-NEG-NOT: Handled a.swift
// CHECK-F-NEG-NOT: Handled b.swift
// CHECK-F-NEG-NOT: Handled c.swift
// CHECK-F-NEG-NOT: Handled d.swift
// CHECK-F-NEG-NOT: Handled e.swift
