/// Test the -module-alias flag.

// RUN: %empty-directory(%t)
// RUN: %{python} %utils/split_file.py -o %t %s

/// Create a module Bar
// RUN: %target-swift-frontend -module-name Bar %t/FileBar.swift -emit-module -emit-module-path %t/Bar.swiftmodule

/// Check Bar.swiftmodule is created
// RUN: test -f %t/Bar.swiftmodule

/// Create a module Foo that imports Cat with -module-alias Cat=Bar with a serialized module loader
// RUN: %target-swift-frontend -module-name Foo %t/FileFoo.swift -module-alias Cat=Bar -I %t -emit-module -emit-module-path %t/Foo.swiftmodule -Rmodule-loading 2> %t/load-result-foo.output

/// Check Foo.swiftmodule is created and Bar.swiftmodule is loaded
// RUN: test -f %t/Foo.swiftmodule
// RUN: test -f %t/Bar.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule

// RUN: %FileCheck %s -input-file %t/load-result-foo.output -check-prefix CHECK-FOO
// CHECK-FOO: remark: loaded module at {{.*}}Bar.swiftmodule

/// Create a module Zoo that imports Cat with -module-alias Cat=Bar with a source loader
// RUN: %target-swift-frontend -module-name Zoo %t/FileFoo.swift -module-alias Cat=Bar -I %t -emit-module -emit-module-path %t/Zoo.swiftmodule -enable-source-import -Rmodule-loading 2> %t/load-result-zoo.output

// RUN: test -f %t/Zoo.swiftmodule
// RUN: test -f %t/Bar.swiftmodule
// RUN: not test -f %t/Cat.swiftmodule

// RUN: %FileCheck %s -input-file %t/load-result-zoo.output -check-prefix CHECK-ZOO
// CHECK-ZOO: remark: loaded module at {{.*}}Bar.swiftmodule


// BEGIN FileBar.swift
public func bar() {}

// BEGIN FileFoo.swift
import Cat
