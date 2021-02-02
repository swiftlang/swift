// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/module-fingerprint/* %t

//
// Set up a clean incremental build of all three modules
//

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -enable-experimental-cross-module-incremental-build -module-name C -I %t -output-file-map %t/C.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -enable-experimental-cross-module-incremental-build -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -enable-experimental-cross-module-incremental-build -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print C -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-C
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print B -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-B
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print A -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-A

// CHECK-CLEAN-C: fingerprint=6e60fd224d614a59568a348e0ac9b55a
// CHECK-CLEAN-B: fingerprint=bfa14052e1df9253b7bf7e0eb7cfc505
// CHECK-CLEAN-A: fingerprint=a939d89f07c766a4607c5fe1a1715cf5

//
// Now change C and ensure that B rebuilds but A does not
//

// RUN: cd %t && echo "public func other() {}" >> C.swift

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -enable-experimental-cross-module-incremental-build -module-name C -I %t -output-file-map %t/C.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: touch %t/C.swiftmodule
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -enable-experimental-cross-module-incremental-build -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -enable-experimental-cross-module-incremental-build -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print C -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-C
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print B -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-B
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print A -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-A

// CHECK-INCREMENTAL-C: fingerprint=3e68d59b74032e18401ec978cdb11cf3
// CHECK-INCREMENTAL-B: fingerprint=bfa14052e1df9253b7bf7e0eb7cfc505
// CHECK-INCREMENTAL-A: fingerprint=a939d89f07c766a4607c5fe1a1715cf5
