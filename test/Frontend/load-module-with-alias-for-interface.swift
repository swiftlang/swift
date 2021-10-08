/// Test the -module-alias flag with a module interface loader.

// RUN: %empty-directory(%t)

/// Create a module Baz
// RUN: echo 'public func baz() {}' > %t/FileBaz.swift
// RUN: %target-swift-frontend -module-name Baz %t/FileBaz.swift -emit-module -emit-module-path %t/Baz.swiftmodule -enable-library-evolution

/// Check Baz.swiftmodule is created
// RUN: test -f %t/Baz.swiftmodule

/// Create a module FooInterface that imports Cat with -module-alias Cat=Baz
// RUN: echo 'import Cat' > %t/FileFoo.swift
// RUN: %target-swift-frontend -module-name FooInterface -module-alias Cat=Baz %t/FileFoo.swift -emit-module -emit-module-interface-path %t/FooInterface.swiftinterface -swift-version 5 -enable-library-evolution -I %t -Rmodule-loading 2> %t/load-result.output

/// Check Foo.swiftinterface is created and Baz.swiftmodule is loaded
// RUN: test -f %t/FooInterface.swiftinterface
// RUN: test -f %t/Baz.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule

// RUN: %FileCheck %s -input-file %t/FooInterface.swiftinterface -check-prefix CHECK-IMPORT
// CHECK-IMPORT: -module-alias Cat=Baz
// CHECK-IMPORT: import Cat

// RUN: %FileCheck %s -input-file %t/load-result.output -check-prefix CHECK-LOAD
// CHECK-LOAD: remark: loaded module at {{.*}}Baz.swiftmodule

