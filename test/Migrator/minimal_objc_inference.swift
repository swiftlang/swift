// RUN: rm -rf %t && mkdir -p %t && %target-swift-frontend -c -primary-file %S/Inputs/minimal_objc_inference.swift -emit-migrated-file-path %t/migrated_minimal_objc_inference.swift -emit-remap-file-path %t/migrated_minimal_objc_inference.swift.remap -o /dev/null
// RUN: %FileCheck %s < %t/migrated_minimal_objc_inference.swift
// REQUIRES: objc_interop

// CHECK-NOT: @objc var propertyUsedInKeyPath: NSObject? = nil
// CHECK:     @objc dynamic var dynamicVarUsedInSelector : Int { return 2 }
// CHECK-NOT: @objc func overridden() {}
// CHECK-NOT: @objc func usedViaAnyObject() {}
// CHECK-NOT: @objc func inExtensionAndOverridden() {}
// CHECK-NOT: @objc func unused() {}

