/// bad ==> main | bad --> other

// RUN: rm -rf %t && cp -r %S/Inputs/fail-simple/ %t
// RUN: touch -t 201401240005 %t/*

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck %s
// CHECK: Handled main.swift
// CHECK: Handled bad.swift
// CHECK-NOT: Handled other.swift

// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck %s

// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-OKAY %s
// CHECK-OKAY: Handled main.swift
// CHECK-OKAY: Handled bad.swift
// CHECK-OKAY: Handled other.swift

// RUN: rm %t/bad.o
// RUN: rm %t/bad.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck %s

// RUN: rm %t/*.o
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-OKAY %s
// RUN: rm %t/main.o
// RUN: rm %t/main.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck %s

// RUN: rm %t/*.o
// RUN: cd %t && %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck -check-prefix=CHECK-OKAY %s
// RUN: rm %t/other.o
// RUN: rm %t/other.swiftdeps
// RUN: cd %t && not %swiftc_driver -c -driver-use-frontend-path %S/Inputs/update-dependencies-bad.py -output-file-map %t/output.json -incremental ./main.swift ./bad.swift ./other.swift -module-name main -j1 -v 2>&1 | FileCheck %s
