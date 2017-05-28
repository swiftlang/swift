// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -migrate-keep-objc-visibility -primary-file %S/Inputs/conservative_objc_inference.swift -emit-migrated-file-path %t/migrated_conservative_objc_inference.swift -emit-remap-file-path %t/migrated_conservative_objc_inference.swift.remap -o /dev/null
// RUN: %FileCheck %s < %t/migrated_conservative_objc_inference.swift
// REQUIRES: objc_interop

// CHECK: @objc var property : NSObject? = nil
// CHECK: @objc dynamic var dynamicProperty: Int { return 2 }
// CHECK: @objc func foo() {}
// CHECK: @objc func baz() {}
// CHECK: @objc func bar() {}
