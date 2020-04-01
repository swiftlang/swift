// RUN: %empty-directory(%t)
// RUN: %target-swift-frontend -typecheck -module-name Foo -emit-module-interface-path %t/Foo.swiftinterface %s -module-link-name Foo -enable-library-evolution -autolink-force-load
// RUN: %target-swift-frontend -compile-module-from-interface -o %t/Foo.swiftmodule %t/Foo.swiftinterface
// RUN: %target-swift-ide-test -print-module-metadata -module-to-print Foo -I %t -source-filename %s | %FileCheck %s

public func foo() {}

// CHECK: link library: Foo, force load: true