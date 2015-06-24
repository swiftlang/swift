// main | other

// RUN: rm -rf %t && cp -r %S/Inputs/independent/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s
// RUN: ls %t/main~buildrecord.swiftdeps

// CHECK-FIRST-NOT: warning
// CHECK-FIRST: Handled main.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled

// RUN: touch -t 201401240006 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// RUN: touch -t 201401240007 %t/main.swift
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s


// RUN: rm -rf %t && cp -r %S/Inputs/independent/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST-MULTI %s

// CHECK-FIRST-MULTI: Handled main.swift
// CHECK-FIRST-MULTI: Handled other.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// RUN: touch -t 201401240006 %t/*
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST-MULTI %s


// RUN: rm -rf %t && cp -r %S/Inputs/independent/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SINGLE %s
// CHECK-SINGLE: Handled main.swift

// RUN: ls %t/main~buildrecord.swiftdeps

