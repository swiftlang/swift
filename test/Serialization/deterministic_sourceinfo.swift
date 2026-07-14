
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule -emit-module-source-info-path %t/Foo.swiftsourceinfo %s -parse-as-library -serialized-path-obfuscate %s=/CHANGED_FOO -module-name Foo -serialize-debugging-options -deterministic-sourceinfo
// RUN: strings %t/Foo.swiftsourceinfo | %FileCheck %s --check-prefix=CHECK-SOURCEINFO

// RUN: %target-swift-frontend -emit-module -o %t/Foo2.swiftmodule -emit-module-source-info-path %t/Foo2.swiftsourceinfo %s -parse-as-library -file-prefix-map %s=/CHANGED_FOO_FILE_MAP -module-name Foo2 -serialize-debugging-options -deterministic-sourceinfo
// RUN: strings %t/Foo2.swiftsourceinfo | %FileCheck %s --check-prefix=CHECK-FILEMAP

public class A {}

// CHECK-SOURCEINFO: /CHANGED_FOO
// CHECK-FILEMAP: /CHANGED_FOO_FILE_MAP
