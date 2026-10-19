
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)

// This test verifies that paths in the .swiftsourceinfo file can be mapped during compilation
// and subsequently un-mapped during module metadata printing.
// There is a 2x2 matrix of configurations:
// 1. Serialization obfuscation via `-serialized-path-obfuscate` and `-file-prefix-map`, to remap
//    the original source file path.
// 2. Deserialization reading the raw mapped path vs un-mapping via `-sourceinfo-prefix-map`

// --- 1. Test -serialized-path-obfuscate ---
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-module-source-info-path %t/Foo.swiftsourceinfo %s -parse-as-library -serialized-path-obfuscate %s=/CHANGED_FOO -module-name Foo -prefix-map-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-SOURCEINFO-MAPPED
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo -source-filename=x -I %t -sourceinfo-prefix-map /CHANGED_FOO=/UNMAPPED_FOO | %FileCheck %s --check-prefix=CHECK-SOURCEINFO-UNMAPPED

// --- 2. Test -file-prefix-map ---
// RUN: %target-swift-frontend -emit-module -o %t/Foo2.swiftmodule -emit-module-source-info-path %t/Foo2.swiftsourceinfo %s -parse-as-library -file-prefix-map %s=/CHANGED_FOO_FILE_MAP -module-name Foo2 -prefix-map-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo2 -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-FILEMAP-MAPPED
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo2 -source-filename=x -I %t -sourceinfo-prefix-map /CHANGED_FOO_FILE_MAP=/UNMAPPED_FOO_FILE_MAP | %FileCheck %s --check-prefix=CHECK-FILEMAP-UNMAPPED

public class A {}

// CHECK-SOURCEINFO-MAPPED: filepath=/CHANGED_FOO;
// CHECK-SOURCEINFO-MAPPED: mtime=19{{(69|70)}}-

// CHECK-SOURCEINFO-UNMAPPED: filepath=/UNMAPPED_FOO;
// CHECK-SOURCEINFO-UNMAPPED: mtime=19{{(69|70)}}-

// CHECK-FILEMAP-MAPPED: filepath=/CHANGED_FOO_FILE_MAP;
// CHECK-FILEMAP-MAPPED: mtime=19{{(69|70)}}-

// CHECK-FILEMAP-UNMAPPED: filepath=/UNMAPPED_FOO_FILE_MAP;
// CHECK-FILEMAP-UNMAPPED: mtime=19{{(69|70)}}-
