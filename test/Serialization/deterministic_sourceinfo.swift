
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-module-source-info-path %t/Foo.swiftsourceinfo %s -parse-as-library -serialized-path-obfuscate %s=/CHANGED_FOO -module-name Foo -deterministic-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-SOURCEINFO

// RUN: %target-swift-frontend -emit-module -o %t/Foo2.swiftmodule -emit-module-source-info-path %t/Foo2.swiftsourceinfo %s -parse-as-library -file-prefix-map %s=/CHANGED_FOO_FILE_MAP -module-name Foo2 -deterministic-sourceinfo
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print=Foo2 -source-filename=x -I %t | %FileCheck %s --check-prefix=CHECK-FILEMAP

public class A {}

// CHECK-SOURCEINFO: filepath=/CHANGED_FOO;
// CHECK-SOURCEINFO: mtime=19{{(69|70)}}-

// CHECK-FILEMAP: filepath=/CHANGED_FOO_FILE_MAP;
// CHECK-FILEMAP: mtime=19{{(69|70)}}-
