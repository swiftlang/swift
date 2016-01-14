// RUN: rm -rf %t
// RUN: mkdir -p %t

// REQUIRES: objc_interop

// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -swift3-migration %S/../Inputs/clang-importer-sdk/swift-modules/ObjectiveC.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -swift3-migration %S/../Inputs/clang-importer-sdk/swift-modules/CoreGraphics.swift
// RUN: %target-swift-frontend(mock-sdk: %clang-importer-sdk -I %t) -emit-module -o %t -swift3-migration %S/../Inputs/clang-importer-sdk/swift-modules/Foundation.swift

// RUN: %target-swift-ide-test(mock-sdk: -sdk %S/../Inputs/clang-importer-sdk -I %t) -print-module -source-filename %s -module-to-print=Foundation -function-definitions=false -prefer-type-repr=true -swift3-migration -print-implicit-attrs > %t.Foundation.txt
// RUN: FileCheck %s -check-prefix=CHECK-FOUNDATION -strict-whitespace < %t.Foundation.txt

// CHECK-FOUNDATION: @swift3_migration(renamed="withObjects(_:count:)")
// CHECK-FOUNDATION-NEXT:  @objc class func arrayWithObjects(objects: UnsafePointer<AnyObject?>, count: Int) -> Self!

// CHECK-FOUNDATION: @swift3_migration(renamed="isMakingHoney")
// CHECK-FOUNDATION-NEXT: @objc var makingHoney: Bool
