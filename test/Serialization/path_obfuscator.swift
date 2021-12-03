// REQUIRES: VENDOR=apple
// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t.module-cache)
// RUN: %target-swift-frontend -emit-module -o %t/Foo.swiftmodule %s -parse-as-library -serialized-path-obfuscate /FOO=/CHANGED_FOO -serialized-path-obfuscate /BAR=/CHANGED_BAR -I /FOO/contents -I /BAR/contents -module-name Foo -serialize-debugging-options
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -enable-swiftsourceinfo -I %t -source-filename %s | %FileCheck %s --check-prefix=CHECK-ORIGINAL
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -enable-swiftsourceinfo -I %t -source-filename %s -serialized-path-obfuscate /FOO=/CHANGED_FOO -serialized-path-obfuscate /BAR=/CHANGED_BAR | %FileCheck %s --check-prefix=CHECK-RECOVER

public class A {}

// CHECK-ORIGINAL: /CHANGED_FOO/contents
// CHECK-ORIGINAL: /CHANGED_BAR/contents

// CHECK-RECOVER: /FOO/contents
// CHECK-RECOVER: /BAR/contents