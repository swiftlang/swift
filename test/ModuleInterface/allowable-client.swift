// RUN: %empty-directory(%t)
// RUN: %empty-directory(%t/textual)
// RUN: %empty-directory(%t/binary)
// RUN: %empty-directory(%t/module-cache)

// RUN: %target-swift-frontend -emit-module %s -module-name Foo -swift-version 5 -disable-implicit-concurrency-module-import -allowable-client FooFriend1 -allowable-client FooFriend2 -allowable-client FooFriend3 -emit-module-interface-path %t/textual/Foo.swiftinterface -enable-library-evolution -emit-module-path %t/binary/Foo.swiftmodule

// RUN: %FileCheck %s --check-prefix=INTERFACE-FLAG < %t/textual/Foo.swiftinterface

// INTERFACE-FLAG: swift-module-flags:
// INTERFACE-FLAG: -allowable-client FooFriend1 -allowable-client FooFriend2 -allowable-client FooFriend3


// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/binary -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=BINARY-MODULE
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t/textual -source-filename %s -module-cache-path %t/module-cache | %FileCheck %s --check-prefix=BINARY-MODULE

// BINARY-MODULE-DAG: allowable client: FooFriend1;
// BINARY-MODULE-DAG: allowable client: FooFriend2;
// BINARY-MODULE-DAG: allowable client: FooFriend3;
