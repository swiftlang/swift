// RUN: %empty-directory(%t)

// RUN: %target-swift-frontend -emit-silgen %S/Inputs/accessibility_vtables_testable_helper.swift | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %S/Inputs/accessibility_vtables_testable_helper.swift | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -emit-silgen %S/Inputs/accessibility_vtables_testable_helper.swift | %FileCheck %s --check-prefix=LIBRARY
// RUN: %target-swift-frontend -enable-library-evolution -emit-silgen %S/Inputs/accessibility_vtables_testable_helper.swift | %FileCheck %s --check-prefix=LIBRARY

// RUN: %target-swift-frontend -emit-module -o %t %S/Inputs/accessibility_vtables_testable_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=FRAGILE-CLIENT

// RUN: %target-swift-frontend -emit-module -enable-testing -o %t %S/Inputs/accessibility_vtables_testable_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=FRAGILE-CLIENT

// RUN: %target-swift-frontend -enable-library-evolution -emit-module -o %t %S/Inputs/accessibility_vtables_testable_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=RESILIENT-CLIENT

// RUN: %target-swift-frontend -enable-library-evolution -emit-module -enable-testing -o %t %S/Inputs/accessibility_vtables_testable_helper.swift
// RUN: %target-swift-emit-silgen -primary-file %s -I %t | %FileCheck %s --check-prefix=RESILIENT-CLIENT

import accessibility_vtables_testable_helper

public class Derived : Middle {
	open override func method() {}
}

// LIBRARY-LABEL: sil_vtable {{(\[serialized\] )?}}Base {
// LIBRARY-NEXT:    #Base.method: (Base) -> () -> () : @$s37accessibility_vtables_testable_helper4BaseC6methodyyF
// LIBRARY-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s37accessibility_vtables_testable_helper4BaseCACycfC
// LIBRARY-NEXT:    #Base.deinit!deallocator: @$s37accessibility_vtables_testable_helper4BaseCfD
// LIBRARY-NEXT:  }

// LIBRARY-LABEL: sil_vtable {{(\[serialized\] )?}}Middle {
// LIBRARY-NEXT:    #Base.method: (Base) -> () -> () : @$s37accessibility_vtables_testable_helper6MiddleC6methodyyFAA4BaseCADyyFTV [override]
// LIBRARY-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s37accessibility_vtables_testable_helper6MiddleCACycfC [override]
// LIBRARY-NEXT:    #Middle.method: (Middle) -> () -> () : @$s37accessibility_vtables_testable_helper6MiddleC6methodyyF
// LIBRARY-NEXT:    #Middle.deinit!deallocator: @$s37accessibility_vtables_testable_helper6MiddleCfD
// LIBRARY-NEXT:  }

// FRAGILE-CLIENT-LABEL: sil_vtable [serialized] Derived {
// FRAGILE-CLIENT-NEXT:    #Base.method: (Base) -> () -> () : @$s37accessibility_vtables_testable_helper6MiddleC6methodyyFAA4BaseCADyyFTV [inherited]
// FRAGILE-CLIENT-NEXT:    #Base.init!allocator: (Base.Type) -> () -> Base : @$s37accessibility_vtables_testable_helper6MiddleCACycfC [inherited]
// FRAGILE-CLIENT-NEXT:    #Middle.method: (Middle) -> () -> () : @$s30accessibility_vtables_testable7DerivedC6methodyyF [override]
// FRAGILE-CLIENT-NEXT:    #Derived.deinit!deallocator: @$s30accessibility_vtables_testable7DerivedCfD
// FRAGILE-CLIENT-NEXT:  }

// RESILIENT-CLIENT-LABEL: sil_vtable [serialized] Derived {
// RESILIENT-CLIENT-NEXT:    #Middle.method: (Middle) -> () -> () : @$s30accessibility_vtables_testable7DerivedC6methodyyF [override]	// Derived.method()
// RESILIENT-CLIENT-NEXT:    #Derived.deinit!deallocator: @$s30accessibility_vtables_testable7DerivedCfD	// Derived.__deallocating_deinit
// RESILIENT-CLIENT-NEXT:  }