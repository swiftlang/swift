/// Test mangling with module aliasing

// RUN: %empty-directory(%t)

/// Create a module Bar
// RUN: echo 'public class Klass {}' > %t/FileBar.swift
// RUN: %target-swift-frontend -module-name Bar %t/FileBar.swift -emit-module -emit-module-path %t/Bar.swiftmodule

/// Check Bar.swiftmodule is created
// RUN: test -f %t/Bar.swiftmodule

/// Create a module Foo that imports Cat with -module-alias Cat=Bar
// RUN: echo 'import Cat' > %t/FileFoo.swift
// RUN: echo 'public func meow() -> Cat.Klass? { return nil }' >> %t/FileFoo.swift
// RUN: %target-swift-frontend -module-name Foo %t/FileFoo.swift -module-alias Cat=Bar -I %t -emit-module -emit-module-path %t/Foo.swiftmodule -c -o %t/ResultFoo.o

/// Check Foo.swiftmodule is created and Bar.swiftmodule is loaded
// RUN: test -f %t/Foo.swiftmodule

// RUN: llvm-objdump -t %t/ResultFoo.o | %FileCheck %s -check-prefix=CHECK-A
// CHECK-A: _$s3Foo4meow3Bar5KlassCSgyF

// RUN: llvm-objdump -t %t/ResultFoo.o | swift-demangle | %FileCheck %s -check-prefix=CHECK-B
// CHECK-B: Foo.meow() -> Bar.Klass?

