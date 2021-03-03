// RUN: %empty-directory(%t)
// RUN: cp -r %S/Inputs/module-fingerprint/* %t

//
// Set up a clean incremental build of all three modules
//

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I %t -output-file-map %t/C.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print C -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-C
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print B -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-B
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print A -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-CLEAN-A

// CHECK-CLEAN-C: fingerprint=7957733a8ee9bc44f726a516108786eb
// CHECK-CLEAN-B: fingerprint=17bb71bdc7972a93446d524f47044156
// CHECK-CLEAN-A: fingerprint=048332944f6e149f2e8bed309d47283d

//
// Now change C and ensure that B rebuilds but A does not
//

// RUN: cd %t && echo "public func other() {}" >> C.swift

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I %t -output-file-map %t/C.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: touch %t/C.swiftmodule
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print C -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-C
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print B -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-B
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print A -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL-A

// CHECK-INCREMENTAL-C: fingerprint=263f083edcaaf08536f657d10082dacc
// CHECK-INCREMENTAL-B: fingerprint=17bb71bdc7972a93446d524f47044156
// CHECK-INCREMENTAL-A: fingerprint=048332944f6e149f2e8bed309d47283d

//
// Now change a top-level type of C and ensure that C's fingerprint does not change
//

// RUN: cp %S/Inputs/module-fingerprint/C2.swift %t/C.swift

// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/C.swiftmodule -module-name C -I %t -output-file-map %t/C.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle C.swift
// RUN: touch %t/C.swiftmodule
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/B.swiftmodule -module-name B -I %t -output-file-map %t/B.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle B.swift
// RUN: cd %t && %target-swiftc_driver -c -incremental -emit-dependencies -emit-module -emit-module-path %t/A.swiftmodule -module-name A -I %t -output-file-map %t/A.json -working-directory %t -driver-show-incremental -driver-show-job-lifecycle A.swift

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print C -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL2-C
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print B -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL2-B
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print A -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-INCREMENTAL2-A

// CHECK-INCREMENTAL2-C: fingerprint=263f083edcaaf08536f657d10082dacc
// CHECK-INCREMENTAL2-B: fingerprint=17bb71bdc7972a93446d524f47044156
// CHECK-INCREMENTAL2-A: fingerprint=048332944f6e149f2e8bed309d47283d
