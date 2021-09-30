/// Test the -module-alias flag.

// RUN: %empty-directory(%t)

/// Create a module Bar
// RUN: echo 'public func bar() {}' > %t/FileBar.swift
// RUN: %target-swift-frontend -module-name Bar %t/FileBar.swift -emit-module -emit-module-path %t/Bar.swiftmodule

/// Check if Bar.swiftmodule is created
// RUN: test -f %t/Bar.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule

/// Create a module Foo that imports Cat with -module-alias Cat=Bar
// RUN: echo 'import Cat' > %t/FileFoo.swift
// RUN: %target-swift-frontend -module-name Foo -module-alias Cat=Bar %t/FileFoo.swift -emit-module -emit-module-path %t/Foo.swiftmodule -I %t

/// Check if Foo.swiftmodule is created without an error
// RUN: test -f %t/Foo.swiftmodule
// RUN: test -f %t/Bar.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule
