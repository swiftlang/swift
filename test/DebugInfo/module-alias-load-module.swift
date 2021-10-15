// RUN: %empty-directory(%t)

/// Create a module Bar
// RUN: echo 'public class Klass {}' > %t/FileBar.swift
// RUN: %target-swift-frontend -module-name Bar %t/FileBar.swift -emit-module -emit-module-path %t/Bar.swiftmodule
// RUN: test -f %t/Bar.swiftmodule

/// Create a module Foo that imports Cat with -module-alias Cat=Bar
// RUN: %target-swift-frontend -emit-ir -module-name Foo -module-alias Cat=Bar %s -I %t -g -o - | %FileCheck %s

// CHECK-DAG: ![[FILE:[0-9]+]] = !DIFile(filename: "{{.*}}test{{/|\\\\}}DebugInfo{{/|\\\\}}module-alias-load-module.swift"
// CHECK-DAG: ![[BARMODULE:[0-9]+]] = !DIModule({{.*}}, name: "Bar"
// CHECK-DAG: !DIImportedEntity(tag: DW_TAG_imported_module, scope: ![[FILE]], entity: ![[BARMODULE]]

import Cat
public func meow() -> Cat.Klass? { return nil }
