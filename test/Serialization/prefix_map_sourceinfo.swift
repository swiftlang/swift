// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)

// This test verifies that paths in the .swiftsourceinfo file can be mapped during compilation
// and subsequently un-mapped during module metadata printing.
// There is a 2x2 matrix of configurations:
// 1. Serialization obfuscation via `-serialized-path-obfuscate` and `-file-prefix-map`, to remap
//    the original source file path.
// 2. Deserialization reading the raw mapped path vs un-mapping via `-sourceinfo-prefix-map`

// --- 1. Test -serialized-path-obfuscate ---
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-module-source-info-path %t/Foo.swiftsourceinfo %s -parse-as-library -serialized-path-obfuscate %s=/CHANGED_FOO -serialized-path-obfuscate /original-sourceinfo=./virtual -module-name Foo -prefix-map-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-SOURCEINFO-MAPPED
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo -source-filename=x -I %t -sourceinfo-prefix-map /CHANGED_FOO=/UNMAPPED_FOO | %FileCheck %s --check-prefix=CHECK-SOURCEINFO-UNMAPPED
// RUN: %llvm-bcanalyzer -dump -show-binary-blobs %t/Foo.swiftsourceinfo | %FileCheck %s --enable-yaml-compatibility --check-prefix=CHECK-PATHS -DFILE=/CHANGED_FOO --implicit-check-not SOURCE_DIR --implicit-check-not /original-sourceinfo

// --- 2. Test -file-prefix-map ---
// RUN: %target-swift-frontend -emit-module -o %t/Foo2.swiftmodule -emit-module-source-info-path %t/Foo2.swiftsourceinfo %s -parse-as-library -file-prefix-map %s=/CHANGED_FOO_FILE_MAP -file-prefix-map /original-sourceinfo=./virtual -module-name Foo2 -prefix-map-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo2 -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-FILEMAP-MAPPED
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo2 -source-filename=x -I %t -sourceinfo-prefix-map /CHANGED_FOO_FILE_MAP=/UNMAPPED_FOO_FILE_MAP | %FileCheck %s --check-prefix=CHECK-FILEMAP-UNMAPPED
// RUN: %llvm-bcanalyzer -dump -show-binary-blobs %t/Foo2.swiftsourceinfo | %FileCheck %s --enable-yaml-compatibility --check-prefix=CHECK-PATHS -DFILE=/CHANGED_FOO_FILE_MAP --implicit-check-not SOURCE_DIR --implicit-check-not /original-sourceinfo

// Prefix mapping must cover declaration and documentation locations, not just
// the source-file list printed by -print-module-metadata. Inspect the complete
// string table and ensure that no original matching paths remain.
// CHECK-PATHS: <TEXT_DATA{{.*}}blob data = '[[FILE]]\x00
// CHECK-PATHS-SAME: ./virtual/documented.swift\x00
// CHECK-PATHS-SAME: ./virtual/declaration.swift\x00

// Mapping remains opt-in, even when -file-prefix-map is supplied.
// Binary blob dumps escape Windows backslashes; sanitize those escaped paths.
// RUN: %target-swift-frontend -emit-module -o %t/Unmapped.swiftmodule -emit-module-source-info-path %t/Unmapped.swiftsourceinfo %s -parse-as-library -file-prefix-map %s=/CHANGED_FOO_FILE_MAP -file-prefix-map /original-sourceinfo=./virtual -module-name Unmapped
// RUN: %llvm-bcanalyzer -dump -show-binary-blobs %t/Unmapped.swiftsourceinfo | %FileCheck %s --enable-yaml-compatibility --check-prefix=CHECK-ORIGINAL --implicit-check-not ./virtual --implicit-check-not /CHANGED_FOO_FILE_MAP
// CHECK-ORIGINAL: <TEXT_DATA{{.*}}blob data = 'SOURCE_DIR{{[/\\]+}}test{{[/\\]+}}Serialization{{[/\\]+}}prefix_map_sourceinfo.swift\x00
// CHECK-ORIGINAL-SAME: /original-sourceinfo/documented.swift\x00
// CHECK-ORIGINAL-SAME: /original-sourceinfo/declaration.swift\x00

// Identical sources in different build roots, with different modification
// times, must produce byte-identical sourceinfo when their prefixes are mapped.
// RUN: %empty-directory(%t/first)
// RUN: %empty-directory(%t/second)
// RUN: cp %s %t/first/input.swift
// RUN: cp %s %t/second/input.swift
// RUN: %{python} -c "import os; os.utime(r'%t/first/input.swift', (1000000000, 1000000000)); os.utime(r'%t/second/input.swift', (2000000000, 2000000000))"
// RUN: %target-swift-frontend -emit-module -o %t/first/Independent.swiftmodule -emit-module-source-info-path %t/first/Independent.swiftsourceinfo %t/first/input.swift -parse-as-library -file-prefix-map %t/first=. -file-prefix-map /original-sourceinfo=./virtual -module-name Independent -prefix-map-sourceinfo
// RUN: %target-swift-frontend -emit-module -o %t/second/Independent.swiftmodule -emit-module-source-info-path %t/second/Independent.swiftsourceinfo %t/second/input.swift -parse-as-library -file-prefix-map %t/second=. -file-prefix-map /original-sourceinfo=./virtual -module-name Independent -prefix-map-sourceinfo
// RUN: cmp %t/first/Independent.swiftsourceinfo %t/second/Independent.swiftsourceinfo

public class A {}

#sourceLocation(file: "/original-sourceinfo/documented.swift", line: 100)
/// Documentation whose source location must also be mapped.
public class B {}

#sourceLocation(file: "/original-sourceinfo/declaration.swift", line: 200)
public class C {}
#sourceLocation()

// CHECK-SOURCEINFO-MAPPED: filepath=/CHANGED_FOO;
// CHECK-SOURCEINFO-MAPPED: mtime=19{{(69|70)}}-

// CHECK-SOURCEINFO-UNMAPPED: filepath=/UNMAPPED_FOO;
// CHECK-SOURCEINFO-UNMAPPED: mtime=19{{(69|70)}}-

// CHECK-FILEMAP-MAPPED: filepath=/CHANGED_FOO_FILE_MAP;
// CHECK-FILEMAP-MAPPED: mtime=19{{(69|70)}}-

// CHECK-FILEMAP-UNMAPPED: filepath=/UNMAPPED_FOO_FILE_MAP;
// CHECK-FILEMAP-UNMAPPED: mtime=19{{(69|70)}}-
