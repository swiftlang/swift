// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -emit-module %s -module-name Foo -swift-version 5 -disable-implicit-concurrency-module-import -user-module-version 113.33 -emit-module-interface-path %t/textual/Foo.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Foo.swiftmodule

// RUN: %FileCheck %s --check-prefix=INTERFACE-FLAG < %t/textual/Foo.swiftinterface

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/binary -source-filename %s | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// RUN: rm %t/binary/Foo.swiftmodule
// RUN: %target-swift-frontend -compile-module-from-interface %t/textual/Foo.swiftinterface -o %t/binary/Foo.swiftmodule
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/binary -source-filename %s | %FileCheck %s --check-prefix=USER-MODULE-PRINT

// INTERFACE-FLAG: -user-module-version 113.33

// USER-MODULE-PRINT: user module version: 113.33
