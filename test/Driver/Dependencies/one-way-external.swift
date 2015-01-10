/// other ==> main
/// "./main1-external" ==> main
/// "./main2-external" ==> main
/// "./other1-external" ==> other
/// "./other2-external" ==> other

// RUN: rm -rf %t && cp -r %S/Inputs/one-way-external/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FIRST %s

// CHECK-FIRST: Handled main.swift
// CHECK-FIRST: Handled other.swift

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-SECOND %s

// CHECK-SECOND-NOT: Handled


// RUN: touch -t 201401240005 %t/*
// RUN: touch -t 201401240006 %t/*.o
// RUN: touch -t 201401240004 %t/*-external
// RUN: touch %t/other1-external
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s

// CHECK-THIRD-DAG: Handled other.swift
// CHECK-THIRD-DAG: Handled main.swift

// RUN: touch -t 201401240005 %t/*
// RUN: touch -t 201401240006 %t/*.o
// RUN: touch -t 201401240004 %t/*-external
// RUN: touch %t/other2-external
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-THIRD %s


// RUN: touch -t 201401240005 %t/*
// RUN: touch -t 201401240006 %t/*.o
// RUN: touch -t 201401240004 %t/*-external
// RUN: touch %t/main1-external
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FOURTH %s

// CHECK-FOURTH-NOT: Handled other.swift
// CHECK-FOURTH: Handled main.swift
// CHECK-FOURTH-NOT: Handled other.swift

// RUN: touch -t 201401240005 %t/*
// RUN: touch -t 201401240006 %t/*.o
// RUN: touch -t 201401240004 %t/*-external
// RUN: touch %t/main2-external
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-FOURTH %s
