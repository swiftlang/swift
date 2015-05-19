/// other --> main ==> yet-another
/// other ==>+ main ==> yet-another

// RUN: rm -rf %t && cp -r %S/Inputs/chained-private-after/ %t
// RUN: touch -t 201401240005 %t/*.swift
// RUN: touch -t 201401240006 %t/*.o

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift ./yet-another.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST-NOT: warning
// CHECK-FIRST-NOT: Handled

// RUN: rm %t/other.o
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./yet-another.swift ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND: Handled other.swift
// CHECK-SECOND: Handled main.swift
// CHECK-SECOND: Handled yet-another.swift
