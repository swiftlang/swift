// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-silgen %S/Inputs/accessibility_vtables_package_helper.swift -package-name Package | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %S/Inputs/accessibility_vtables_package_helper.swift -package-name Package | %FileCheck %s --check-prefix=LIBRARY

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/accessibility_vtables_package_helper.swift -package-name Package
// RUN: %target-swift-emit-silgen -primary-file %s -I %t -package-name Package | %FileCheck %s --check-prefix=CLIENT

// RUN: %target-swift-frontend -enable-library-evolution -emit-module -o %t %S/Inputs/accessibility_vtables_package_helper.swift -package-name Package
// RUN: %target-swift-emit-silgen -primary-file %s -I %t -package-name Package | %FileCheck %s --check-prefix=CLIENT

import accessibility_vtables_package_helper

// LIBRARY-LABEL: sil_vtable Base {
// LIBRARY-NEXT:    #Base.packageMethod: (Base) -> () -> () : @$s36accessibility_vtables_package_helper4BaseC0C6MethodyyF
// LIBRARY-NEXT:    #Base.internalMethod: (Base) -> () -> () : @$s36accessibility_vtables_package_helper4BaseC14internalMethodyyF
// LIBRARY-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s36accessibility_vtables_package_helper4BaseCACycfC
// LIBRARY-NEXT:    #Base.deinit!deallocator: @$s36accessibility_vtables_package_helper4BaseCfD
// LIBRARY-NEXT:  }

// CLIENT-LABEL: sil hidden [ossa] @$s29accessibility_vtables_package15usePackageClassyy0a1_b1_C7_helper4BaseCF : $@convention(thin) (@guaranteed Base) -> () {
func usePackageClass(_ c: Base) {
  c.packageMethod()
}

// TODO: If cross-module inheritance from package visibility superclasses ever becomes a thing,
// test serialization of the vtable for the derived class in this file.
